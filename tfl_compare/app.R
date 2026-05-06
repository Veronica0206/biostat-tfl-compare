# app.R — RTF Comparator (Batch + Parallel + TRUE RAW positions)
# 2025-09-26 (Enhanced Page Detection Logic)

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(shinyjs)
  library(DT)
  library(striprtf)
  library(diffobj)
  library(tools)
  library(shinybusy)
  library(shinyWidgets)
  library(promises)
  library(future)
  library(future.apply)
  library(htmltools)
})

# ------------ Parallel plan ------------
plan(multisession)

`%||%` <- function(x, y) {
  if (is.null(x) || !length(x) || (length(x)==1L && is.na(x))) y else x
}

# --------------------- Helpers ---------------------

sanitize_path <- function(s) {
  s <- trimws(s)
  s <- gsub("^\\s*['\"]|['\"]\\s*$", "", s)
  s <- gsub("\\\\", "/", s)
  s <- normalizePath(path.expand(s), winslash = "/", mustWork = FALSE)
  s
}

# Efficient .rtf lister (prefix filter)  ----
list_rtf_by_prefix <- function(dir, prefixes = c("t","l"), recursive = FALSE) {
  all_files <- list.files(dir, pattern = "\\.rtf$|\\.RTF$", recursive = recursive, full.names = TRUE)
  if (!length(all_files)) return(data.frame(path=character(), file=character(), ext=character()))
  base <- basename(all_files)
  keep <- if (length(prefixes)) Reduce(`|`, lapply(prefixes, function(p) grepl(paste0("^", p), base, ignore.case = TRUE))) else rep(TRUE, length(base))
  files <- all_files[keep]
  if (!length(files)) return(data.frame(path=character(), file=character(), ext=character()))
  df <- data.frame(path = files, file = basename(files), stringsAsFactors = FALSE)
  df$ext <- tolower(file_ext(df$file))
  df
}

infer_type <- function(fn) {
  if (grepl("^[Tt]", fn)) "Table" else if (grepl("^[Ll]", fn)) "Listing" else "Other"
}

empty_summary_df <- function() {
  data.frame(
    file = character(),
    type = character(),
    status = character(),
    identical = logical(),
    diff_path = character(),
    diff_csv = character(),
    note = character(),
    stringsAsFactors = FALSE
  )
}

# ---- Page splitter (detects page breaks from RTF \sect, formfeeds, or page footers) ----
split_pages <- function(lines, idx) {
  pages <- list(); pidx <- list()
  cur <- character(0); cidx <- integer(0)

  # A page-footer line is a *whole-line* match of "[optional words] page N [of M]".
  # Anchored to ^...$ so prose like "see page 7 of the appendix" is not treated
  # as a page break. Long lines are skipped — a real footer is short.
  pat_page_footer <- "(?i)^\\s*([a-z]+\\s+)*page\\s+\\d+\\s*(of\\s+\\d+)?\\s*$"
  pat_page_break  <- "(?i)^\\s*page\\s*break\\s*$"
  is_page_footer <- function(ln) {
    if (nchar(ln) > 80L) return(FALSE)
    grepl(pat_page_footer, ln, perl = TRUE) || grepl(pat_page_break, ln, perl = TRUE)
  }

  flush_page <- function() {
    if (length(cur)) {
      while (length(cur) && !nzchar(tail(cur, 1))) {
        cur  <- head(cur,  -1)
        cidx <- head(cidx, -1)
      }
      if (length(cur)) {
        pages[[length(pages)+1L]] <<- cur
        pidx [[length(pidx) +1L]] <<- cidx
      }
      cur <<- character(0); cidx <<- integer(0)
    }
  }

  for (i in seq_along(lines)) {
    ln <- lines[i]

    if (is_page_footer(ln)) {
      flush_page()
      next
    }

    # Handle form feeds (\f), which we inject from \sect and \page commands.
    if (grepl("\f", ln, fixed = TRUE)) {
      parts <- strsplit(ln, "\f", fixed = TRUE)[[1]]
      for (k in seq_along(parts)) {
        # Don't add empty strings resulting from the split
        if (nzchar(trimws(parts[k]))) {
          cur  <- c(cur,  parts[k])
          cidx <- c(cidx, idx[i])
        }
        # A form feed between parts means a page break
        if (k < length(parts)) flush_page()
      }
    } else {
      cur  <- c(cur,  ln)
      cidx <- c(cidx, idx[i])
    }
  }
  flush_page()
  list(lines = pages, idx = pidx)
}

# ---- PER-PAGE NORMALIZATION + IGNORE (keeps RAW and CLEAN indices) ----
normalize_keep_index <- function(
    x,
    per_page_ignore_top = 0L,
    per_page_ignore_bottom = 0L,
    drop_trailing_meta = TRUE,
    # Patterns that identify a *whole-line* footer/meta line. Anchored to
    # ^...$ so an in-cell date or a body line containing "/" is not stripped.
    # The drop-trailing-meta loop walks lines from the bottom up and removes
    # any line that matches; tightening the anchors avoids false trims of
    # real table content.
    footer_patterns = c(
      "(?i)^\\s*([a-z]+\\s+)*page\\s+\\d+\\s*(of\\s+\\d+)?\\s*$",
      "(?i)^\\s*(program|output|log|listing|path|directory|run|printed|created|generated|date|time)\\s*[:=].*$",
      "^\\s*(19|20)\\d{2}[-/.]\\d{1,2}[-/.]\\d{1,2}\\s*$",
      "^\\s*\\d{1,2}[-/.]\\d{1,2}[-/.]\\d{2,4}\\s*$",
      "^\\s*\\d{1,2}:\\d{2}(:\\d{2})?\\s*$",
      "^\\s*[A-Za-z]:\\\\[^\\r\\n]+$",
      "^\\s*\\\\\\\\[^\\r\\n]+$",
      "^\\s*/[^\\r\\n[:space:]][^\\r\\n]*$"
    ),
    dehyphenate = TRUE,
    ignore_whitespace = TRUE
){
  x <- gsub("\r", "\n", x, fixed = TRUE)
  x <- gsub("\u00A0", " ", x, fixed = TRUE)

  # Conditional whitespace handling (from Code #1)
  if (isTRUE(ignore_whitespace)) {
    x <- gsub("[ \t]+", " ", x, perl=TRUE)
    x <- gsub("\n[ \t]+", "\n", x, perl=TRUE)
    x <- gsub("[ \t]+\n", "\n", x, perl=TRUE)
  } else {
    x <- gsub("[ \t]+\n", "\n", x, perl=TRUE)
  }

  if (dehyphenate) x <- gsub("(?<=\\p{L})-\\n(?=\\p{Ll})", "", x, perl = TRUE)
  x <- gsub("\n{3,}", "\n\n", x)

  all_lines <- unlist(strsplit(x, "\n", fixed = TRUE))
  if (!length(all_lines)) {
    return(list(
      text = "", lines = character(0),
      idx_raw = integer(0), idx_clean = integer(0),
      map_df = data.frame(clean=integer(0), raw=integer(0), page=integer(0), line_in_page=integer(0))
    ))
  }
  all_idx_raw <- seq_along(all_lines)

  # Use the new, more flexible page splitting function
  pg <- split_pages(all_lines, all_idx_raw)
  pages <- pg$lines
  pidx  <- pg$idx

  # Collect per-page slices into pre-allocated lists; concatenate once at the
  # end. Avoids the quadratic-growth `c()`-in-loop pattern.
  np <- length(pages)
  kept_lines_l <- vector("list", np)
  kept_raw_l   <- vector("list", np)
  kept_page_l  <- vector("list", np)
  kept_lip_l   <- vector("list", np)

  pat_footer <- paste(footer_patterns, collapse = "|")

  for (p in seq_along(pages)) {
    lines <- pages[[p]]
    idx   <- pidx [[p]]
    if (!length(lines)) next

    # Trim leading/trailing blank lines via index slicing (no copy-per-line).
    first <- 1L
    while (first <= length(lines) && !nzchar(lines[first])) first <- first + 1L
    last <- length(lines)
    while (last >= first && !nzchar(lines[last])) last <- last - 1L
    if (last < first) next
    if (first > 1L || last < length(lines)) {
      lines <- lines[first:last]
      idx   <- idx  [first:last]
    }

    if (drop_trailing_meta) {
      i <- length(lines)
      while (i > 0 && grepl(pat_footer, lines[i], perl = TRUE)) i <- i - 1L
      if (i < length(lines)) {
        if (i == 0L) next
        lines <- lines[seq_len(i)]
        idx   <- idx  [seq_len(i)]
      }
    }

    if (per_page_ignore_top > 0L && length(lines)) {
      k <- min(per_page_ignore_top, length(lines))
      if (k >= length(lines)) next
      lines <- lines[-seq_len(k)]
      idx   <- idx  [-seq_len(k)]
    }

    if (per_page_ignore_bottom > 0L && length(lines)) {
      if (length(lines) <= per_page_ignore_bottom) next
      lines <- head(lines, -per_page_ignore_bottom)
      idx   <- head(idx,   -per_page_ignore_bottom)
    }

    if (length(lines)) {
      kept_lines_l[[p]] <- lines
      kept_raw_l  [[p]] <- idx
      kept_page_l [[p]] <- rep.int(p, length(lines))
      kept_lip_l  [[p]] <- seq_len(length(lines))
    }
  }

  kept_lines <- unlist(kept_lines_l, use.names = FALSE)
  kept_raw   <- unlist(kept_raw_l,   use.names = FALSE)
  kept_page  <- unlist(kept_page_l,  use.names = FALSE)
  kept_lip   <- unlist(kept_lip_l,   use.names = FALSE)
  if (is.null(kept_lines)) kept_lines <- character(0)
  if (is.null(kept_raw))   kept_raw   <- integer(0)
  if (is.null(kept_page))  kept_page  <- integer(0)
  if (is.null(kept_lip))   kept_lip   <- integer(0)

  kept_clean <- seq_along(kept_lines)

  map_df <- data.frame(
    clean = kept_clean,
    raw   = kept_raw,
    page  = kept_page,
    line_in_page = kept_lip,
    stringsAsFactors = FALSE
  )

  list(
    text  = paste(kept_lines, collapse = "\n"),
    lines = kept_lines,
    idx_raw   = kept_raw,
    idx_clean = kept_clean,
    clean_to_raw = setNames(kept_raw, as.character(kept_clean)),
    map_df = map_df
  )
}

# ---- SEGMENTATION + MAPPING ----
make_segments <- function(lines, idx_clean, idx_raw, mode = c("paragraph","line","token")) {
  mode <- match.arg(mode)

  if (mode == "line") {
    segs <- lines
    map_clean <- idx_clean
    map_raw   <- idx_raw

  } else if (mode == "paragraph") {
    # Pre-allocate to an upper bound (each line is at most one paragraph),
    # then trim. Avoids the quadratic-growth `c()`-in-loop pattern.
    n <- length(lines)
    segs_buf <- character(n)
    mc_buf   <- integer(n)
    mr_buf   <- integer(n)
    seg_n <- 0L
    i <- 1L
    while (i <= n) {
      while (i <= n && !nzchar(lines[i])) i <- i + 1L
      if (i > n) break
      start <- i
      while (i <= n && nzchar(lines[i])) i <- i + 1L
      seg_n <- seg_n + 1L
      segs_buf[seg_n] <- paste(lines[start:(i - 1L)], collapse = "\n")
      mc_buf[seg_n]   <- idx_clean[start]
      mr_buf[seg_n]   <- idx_raw[start]
    }
    segs      <- segs_buf[seq_len(seg_n)]
    map_clean <- mc_buf  [seq_len(seg_n)]
    map_raw   <- mr_buf  [seq_len(seg_n)]

  } else { # token
    n <- length(lines)
    tokens_l <- vector("list", n)
    mc_l     <- vector("list", n)
    mr_l     <- vector("list", n)
    for (k in seq_len(n)) {
      tks <- strsplit(lines[k], "\\s+", perl = TRUE)[[1]]
      tks <- tks[nzchar(tks)]
      if (length(tks)) {
        tokens_l[[k]] <- tks
        mc_l    [[k]] <- rep.int(idx_clean[k], length(tks))
        mr_l    [[k]] <- rep.int(idx_raw[k],   length(tks))
      }
    }
    segs      <- unlist(tokens_l, use.names = FALSE)
    map_clean <- unlist(mc_l,     use.names = FALSE)
    map_raw   <- unlist(mr_l,     use.names = FALSE)
    if (is.null(segs))      segs      <- character(0)
    if (is.null(map_clean)) map_clean <- integer(0)
    if (is.null(map_raw))   map_raw   <- integer(0)
  }

  list(
    segs = segs,
    map_clean = map_clean,
    map_raw   = map_raw,
    clean_to_raw = setNames(map_raw, as.character(map_clean))
  )
}

# ---- LCS-based edit script ----
lcs_edits <- function(a, b, max_cells = 25e6) {
  n <- length(a); m <- length(b)
  if (!n && !m) return(data.frame(op=character(), i=integer(), j=integer(), stringsAsFactors = FALSE))
  # The LCS DP table is an integer matrix of (n+1) x (m+1) cells; at 4 bytes
  # per cell, 25M cells ~= 100MB. With multiple parallel workers each comparing
  # large files this can exhaust memory and freeze the Shiny process. Bail
  # early with an actionable error rather than crashing the worker.
  cells <- as.numeric(n + 1L) * as.numeric(m + 1L)
  if (cells > max_cells) {
    stop(sprintf(
      "Diff too large: LCS matrix would be %s x %s = %.1fM cells (limit %.1fM). Try a coarser granularity (line or paragraph instead of token), or split the file.",
      format(n + 1L, big.mark = ","), format(m + 1L, big.mark = ","),
      cells / 1e6, max_cells / 1e6
    ))
  }
  L <- matrix(0L, n + 1L, m + 1L)
  for (i in seq_len(n)) {
    ai <- a[i]
    for (j in seq_len(m)) {
      if (identical(ai, b[j])) L[i + 1L, j + 1L] <- L[i, j] + 1L
      else L[i + 1L, j + 1L] <- max(L[i, j + 1L], L[i + 1L, j])
    }
  }
  i <- n; j <- m; ops <- list()
  while (i > 0L || j > 0L) {
    if (i > 0L && j > 0L && identical(a[i], b[j])) { i <- i - 1L; j <- j - 1L
    } else if (j > 0L && (i == 0L || L[i + 1L, j] >= L[i, j + 1L])) {
      ops[[length(ops) + 1L]] <- list(op="ins", i=NA_integer_, j=j); j <- j - 1L
    } else {
      ops[[length(ops) + 1L]] <- list(op="del", i=i, j=NA_integer_); i <- i - 1L
    }
  }
  ops <- rev(ops)
  do.call(rbind, lapply(ops, function(o)
    data.frame(op=o$op, i=as.integer(o$i), j=as.integer(o$j), stringsAsFactors = FALSE)))
}

# ---- Process diffs into wide format (WITH RAW positions) ----
process_diffs_to_wide_format <- function(d) {
  if (is.null(d) || !nrow(d)) {
    return(data.frame(
      Position = character(), PositionA = character(), PositionB = character(),
      PositionRawA = character(), PositionRawB = character(),
      RawA = integer(), RawB = integer(),
      `Display A` = character(), `Display B` = character(),
      stringsAsFactors = FALSE, check.names = FALSE
    ))
  }

  ops <- ifelse(startsWith(d$diff, "<"), "del", "add")
  block_id <- integer(nrow(d))
  i <- 1L; bid <- 0L
  while (i <= nrow(d)) {
    bid <- bid + 1L
    while (i <= nrow(d) && ops[i] == "del") { block_id[i] <- bid; i <- i + 1L }
    while (i <= nrow(d) && ops[i] == "add") { block_id[i] <- bid; i <- i + 1L }
  }
  d$block_id <- block_id

  out <- d %>%
    dplyr::group_by(block_id) %>%
    dplyr::summarise(
      hasA = any(startsWith(diff, "<")),
      hasB = any(startsWith(diff, ">")),

      page_A0 = pageA[!is.na(pageA)][1],
      lip_A0  = lipA [!is.na(lipA )][1],
      page_B0 = pageB[!is.na(pageB)][1],
      lip_B0  = lipB [!is.na(lipB )][1],

      # true raw indices from each side
      raw_A0  = rawA[!is.na(rawA)][1],
      raw_B0  = rawB[!is.na(rawB)][1],

      `Display A` = {
        x <- diff[startsWith(diff, "<")]
        if (length(x)) sub("^\\s*<\\s*", "", x) |> paste(collapse = "\n") else NA_character_
      },
      `Display B` = {
        y <- diff[startsWith(diff, ">")]
        if (length(y)) sub("^\\s*>\\s*", "", y) |> paste(collapse = "\n") else NA_character_
      },
      .groups = "drop"
    ) %>%
    # Replacement: B shows A's human page:line, but RAW positions remain side-true
    dplyr::mutate(
      page_A = page_A0,  lip_A = lip_A0,
      page_B = dplyr::if_else(hasA & hasB & !is.na(page_A0), page_A0, page_B0),
      lip_B  = dplyr::if_else(hasA & hasB & !is.na(lip_A0 ), lip_A0,  lip_B0 ),
      RawA   = raw_A0,
      RawB   = raw_B0,
      PositionA = ifelse(!is.na(page_A) & !is.na(lip_A), paste0("Page ", page_A, " Line ", lip_A), NA_character_),
      PositionB = ifelse(!is.na(page_B) & !is.na(lip_B), paste0("Page ", page_B, " Line ", lip_B), NA_character_),
      PositionRawA = ifelse(!is.na(RawA), as.character(RawA), NA_character_),
      PositionRawB = ifelse(!is.na(RawB), as.character(RawB), NA_character_),
      Position  = dplyr::coalesce(PositionA, PositionB)
    ) %>%
    dplyr::select(Position, PositionA, PositionB, PositionRawA, PositionRawB, RawA, RawB, `Display A`, `Display B`)

  out
}

# HTML index writer ----
# When `summary_df` is provided, the index lists ONLY the diff files referenced
# by this run (looked up by basename of diff_path). This avoids including stale
# `*.diff.html` from earlier runs that happen to share the output folder — a
# real risk for QC use, where an old "Different" report could be re-presented
# alongside a new run that no longer flags that file.
# Hrefs are URL-encoded, labels are HTML-escaped.
write_diff_index <- function(diff_dir, summary_df = NULL,
                             title = "RTF Differences Index", filename = "index.html") {
  if (!dir.exists(diff_dir)) dir.create(diff_dir, recursive = TRUE, showWarnings = FALSE)
  esc  <- htmltools::htmlEscape
  uenc <- function(x) vapply(x, utils::URLencode, character(1), reserved = FALSE)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  if (!is.null(summary_df) && nrow(summary_df)) {
    keep <- !is.na(summary_df$diff_path)
    diff_files <- basename(summary_df$diff_path[keep])
    sub_df <- summary_df[keep, , drop = FALSE]
  } else {
    diff_files <- list.files(diff_dir, pattern = "\\.diff\\.html$",
                             ignore.case = TRUE, full.names = FALSE)
    sub_df <- NULL
  }
  n <- length(diff_files)

  header <- if (!is.null(sub_df))
    "<tr><th>#</th><th>Type</th><th>Status</th><th>Diff (HTML)</th><th>CSV</th></tr>"
  else "<tr><th>#</th><th>Diff file</th></tr>"

  make_row <- function(f, i) {
    href_html <- uenc(f)
    if (!is.null(sub_df)) {
      row  <- sub_df[basename(sub_df$diff_path) == f, , drop = FALSE]
      typ  <- if (nrow(row)) row$type[1]  else ""
      stat <- if (nrow(row)) row$status[1] else "Different"
      csv  <- if (nrow(row) && !is.na(row$diff_csv[1]))
        sprintf('<a href="%s" download>csv</a>',
                esc(uenc(basename(row$diff_csv[1])))) else ""
      sprintf('<tr><td>%d</td><td>%s</td><td>%s</td><td><a href="%s" target="_blank" rel="noopener">%s</a></td><td>%s</td></tr>',
              i, esc(typ), esc(stat), esc(href_html), esc(f), csv)
    } else {
      sprintf('<tr><td>%d</td><td><a href="%s" target="_blank" rel="noopener">%s</a></td></tr>',
              i, esc(href_html), esc(f))
    }
  }
  rows <- if (n) paste0(vapply(seq_along(diff_files),
                               function(i) make_row(diff_files[i], i),
                               character(1)), collapse = "\n")
  else "<tr><td colspan='5'><em>No diff artifacts found.</em></td></tr>"

  html <- sprintf(
    '<!doctype html><html><head><meta charset="utf-8"><title>%s</title>
<style>body{font-family:system-ui,-apple-system,Segoe UI,Roboto,sans-serif;margin:24px}
table{border-collapse:collapse;width:100%%}th,td{border:1px solid #ddd;padding:8px}
th{background:#f6f6f6;text-align:left}tr:nth-child(even){background:#fafafa}.meta{color:#666;margin-bottom:12px}</style>
</head><body><h1>%s</h1><div class="meta">Generated: %s • Directory: %s • Files: %d</div>
<table><thead>%s</thead><tbody>%s</tbody></table></body></html>',
    esc(title), esc(title), esc(now),
    esc(normalizePath(diff_dir, winslash = "/")), n, header, rows)
  out <- file.path(diff_dir, filename)
  writeLines(html, out, useBytes = TRUE)
  out
}

# ----- Pairwise compare (page-aware; clean+raw positions) -----
compare_two_rtfs <- function(file_a, file_b,
                             compare_mode = c("paragraph","line","token"),
                             diff_html_path = NULL,
                             diff_csv_path  = NULL,
                             per_page_top = 0L, per_page_bottom = 0L,
                             drop_meta = TRUE,
                             ignore_whitespace = TRUE) {
  compare_mode <- match.arg(compare_mode)

  # Two-stage pre-processing: replace RTF page-break control words with a
  # placeholder that survives striprtf, then swap the placeholder for \f.
  # Match only \page and \sect (not \sectd, which is a section-formatting reset
  # and is emitted mid-document by many writers).
  read_and_preprocess_rtf <- function(file_path) {
    raw_content <- readChar(file_path, nchars = file.size(file_path), useBytes = TRUE)
    page_break_placeholder <- "_!!_RTF_PAGE_BREAK_!!_"
    processed_content <- gsub("\\\\(page|sect)\\b", page_break_placeholder, raw_content, perl = TRUE)

    # Write byte-for-byte (no trailing newline, no OS line-ending translation)
    # so striprtf parses the same bytes we generated.
    tmp_file <- tempfile(fileext = ".rtf")
    on.exit(unlink(tmp_file), add = TRUE)
    writeChar(processed_content, tmp_file, eos = NULL, useBytes = TRUE)

    text_with_placeholder <- striprtf::read_rtf(tmp_file)
    gsub(page_break_placeholder, "\f", text_with_placeholder, fixed = TRUE)
  }

  ta_raw <- read_and_preprocess_rtf(file_a)
  tb_raw <- read_and_preprocess_rtf(file_b)

  ta_raw <- iconv(ta_raw, to = "UTF-8"); tb_raw <- iconv(tb_raw, to = "UTF-8")

  na <- normalize_keep_index(ta_raw, per_page_top, per_page_bottom,
                             drop_trailing_meta = drop_meta,
                             ignore_whitespace = ignore_whitespace)
  nb <- normalize_keep_index(tb_raw, per_page_top, per_page_bottom,
                             drop_trailing_meta = drop_meta,
                             ignore_whitespace = ignore_whitespace)

  segA <- make_segments(na$lines, na$idx_clean, na$idx_raw, compare_mode)
  segB <- make_segments(nb$lines, nb$idx_clean, nb$idx_raw, compare_mode)

  same <- identical(segA$segs, segB$segs)

  ed <- if (same) data.frame(op=character(), i=integer(), j=integer(), stringsAsFactors = FALSE)
  else lcs_edits(segA$segs, segB$segs)

  long_df <- if (!nrow(ed)) {
    data.frame(lineA = integer(), lineB = integer(), diff = character(), stringsAsFactors = FALSE)
  } else {
    do.call(rbind, lapply(seq_len(nrow(ed)), function(k) {
      if (ed$op[k] == "del") {
        la <- segA$map_clean[ed$i[k]]
        data.frame(lineA = la, lineB = NA_integer_,
                   diff  = paste0("< ", segA$segs[ed$i[k]]),
                   stringsAsFactors = FALSE)
      } else { # insertion
        lb <- segB$map_clean[ed$j[k]]
        data.frame(lineA = NA_integer_, lineB = lb,
                   diff  = paste0("> ", segB$segs[ed$j[k]]),
                   stringsAsFactors = FALSE)
      }
    }))
  }

  # Join logic (carry page/lip and TRUE raw positions)
  diffs_df <- if (nrow(long_df) > 0) {
    map_a <- na$map_df %>% dplyr::select(clean, pageA = page, lipA = line_in_page, rawA = raw)
    map_b <- nb$map_df %>% dplyr::select(clean, pageB = page, lipB = line_in_page, rawB = raw)

    long_df %>%
      dplyr::left_join(map_a, by = c("lineA" = "clean")) %>%
      dplyr::left_join(map_b, by = c("lineB" = "clean"))
  } else {
    data.frame(lineA=integer(), lineB=integer(), diff=character(), rawA=integer(), rawB=integer(),
               pageA=integer(), lipA=integer(), pageB=integer(), lipB=integer())
  }

  # Write HTML diff (word-level enabled)
  diff_path <- NA_character_
  if (!same && !is.null(diff_html_path)) {
    dir.create(dirname(diff_html_path), recursive = TRUE, showWarnings = FALSE)
    frag <- as.character(diffobj::diffChr(
      segA$segs, segB$segs,
      format = "html", pager = "off",
      tar.banner = "", cur.banner = "",
      word.diff = TRUE
    ))
    html <- paste0(
      '<!doctype html><html><head><meta charset="utf-8">',
      '<meta name="viewport" content="width=device-width,initial-scale=1">',
      '<title>RTF Diff: ', htmltools::htmlEscape(basename(file_a)), ' vs ',
      htmltools::htmlEscape(basename(file_b)), '</title>',
      '<style>body{font-family:system-ui,-apple-system,Segoe UI,Roboto,sans-serif;margin:16px}',
      '.diffobj{max-width:100%;overflow:auto}</style>',
      '</head><body>',
      paste(frag, collapse = "\n"),
      '</body></html>'
    )
    writeLines(html, diff_html_path, useBytes = TRUE)
    diff_path <- normalizePath(diff_html_path, winslash = "/")
  }

  diff_csv_out <- NA_character_
  if (!same && !is.null(diff_csv_path)) {
    dir.create(dirname(diff_csv_path), recursive = TRUE, showWarnings = FALSE)
    display_df <- process_diffs_to_wide_format(diffs_df)
    drop_cols <- c("PositionRawA","PositionRawB","RawA","RawB")
    display_df_csv <- display_df[, setdiff(names(display_df), drop_cols), drop = FALSE]
    utils::write.csv(display_df_csv, diff_csv_path, row.names = FALSE, na = "")
    diff_csv_out <- normalizePath(diff_csv_path, winslash = "/")
  }

  file_label <- if (basename(file_a) == basename(file_b))
    basename(file_a) else paste(basename(file_a), "vs", basename(file_b))
  type_label <- infer_type(basename(file_a))

  display_mode <- switch(compare_mode,
                         "paragraph" = "Global",
                         "line" = "Line",
                         "token" = "Token",
                         compare_mode)

  summary_like <- data.frame(
    file        = file_label,
    type        = type_label,
    status      = if (same) "Identical" else "Different",
    identical   = same,
    diff_path   = diff_path,
    diff_csv    = diff_csv_out,
    note        = if (same) "Identical" else sprintf("Differences found (%s-level)", display_mode),
    stringsAsFactors = FALSE
  )

  list(summary_like = summary_like, differences = diffs_df)
}

# --------------------- UI ---------------------
ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinybusy::add_busy_bar(color = "#29b6f6", height = "3px"),
  tags$head(tags$style(HTML("
    .progress { margin-top: 6px; height: 22px; }
    .shiny-notification { font-size: 14px; }
    .dt-right { text-align: right; }
  "))),
  navbarPage(
    title = "RTF Comparator",
    tabPanel("Directory Compare",
             sidebarLayout(
               sidebarPanel(
                 textInput("dirA", "Directory A path", ""),
                 textInput("dirB", "Directory B path", ""),
                 checkboxGroupInput("prefix", "Compare which prefixes?",
                                    choices = c("Tables (t)"="t", "Listings (l)"="l"),
                                    selected = c("t","l")),
                 checkboxInput("recursive", "Search subfolders (recursive)", FALSE),
                 tags$hr(),
                 h5("Comparison Settings"),
                 selectInput("mode", "Compare granularity",
                             c("Global"="paragraph","Line"="line","Token"="token"), selected="line"),
                 numericInput("ignoreTop_dir", "Ignore first N lines per page (header trim)", value = 0, min = 0, max = 500, step = 1),
                 numericInput("ignoreBottom_dir", "Ignore last N lines per page (footer trim)", value = 0, min = 0, max = 500, step = 1),
                 checkboxInput("dropMeta_dir", "Drop footer/meta on each page", TRUE),
                 checkboxInput("ignoreWhitespace", "Ignore whitespace differences", TRUE),
                 tags$hr(),
                 h5("Performance & Output"),
                 numericInput("batchSize", "Batch size (files per batch)", value = 10, min = 1, max = 200, step = 1),
                 numericInput("workers", "Parallel workers (CPU cores to use)", value = 3, min = 1, max = 16, step = 1),
                 textInput("diffdir", "Diff output folder", "rtf_diffs"),
                 checkboxInput("makeIndex", "Create index.html with links", TRUE),
                 tags$hr(),
                 actionButton("runDir", "Run comparison", class = "btn-primary"),
                 actionButton("clearDir", "Clear Results"),
                 tags$hr(),
                 downloadButton("dlSummary", "Download SUMMARY (CSV)"),
                 downloadButton("dlDiffsOnly", "Download DIFFS (CSV)"),
                 tags$hr(),
                 progressBar(
                   id = "batchProgressBar",
                   value = 0, total = 100, display_pct = TRUE,
                   striped = TRUE, status = "info",
                   title = "File 0 of 0 (Batch 0 of 0)"
                 ),
                 verbatimTextOutput("statusText", placeholder = TRUE)
               ),
               mainPanel(
                 h4("Summary"), DTOutput("tblSummary"),
                 h4("Differences only"), DTOutput("tblDiffs"),
                 uiOutput("indexLink")
               )
             )
    ),
    tabPanel("Pairwise Compare",
             sidebarLayout(
               sidebarPanel(
                 textInput("fileA", "File A (.rtf) path", ""),
                 textInput("fileB", "File B (.rtf) path", ""),
                 tags$hr(),
                 h5("Comparison Settings"),
                 selectInput("mode2", "Compare granularity",
                             c("Global"="paragraph","Line"="line","Token"="token"), selected="line"),
                 numericInput("ignoreTop_pair", "Ignore first N lines per page (header trim)", value = 0, min = 0, max = 500, step = 1),
                 numericInput("ignoreBottom_pair", "Ignore last N lines per page (footer trim)", value = 0, min = 0, max = 500, step = 1),
                 checkboxInput("dropMeta_pair", "Drop footer/meta on each page", TRUE),
                 checkboxInput("ignoreWhitespace_pair", "Ignore whitespace differences", TRUE),
                 tags$hr(),
                 h5("Output Settings"),
                 textInput("pairDiffHtml", "Write HTML diff to (optional)",
                           "rtf_diffs/single_pair.diff.html"),
                 checkboxInput("pairIndex", "Update/create index.html in diff folder", TRUE),
                 tags$hr(),
                 actionButton("runPair", "Run pairwise comparison", class = "btn-primary"),
                 actionButton("clearPair", "Clear Results"),
                 tags$hr(),
                 downloadButton("dlPairDiffs", "Download pairwise DIFF (CSV)")
               ),
               mainPanel(
                 h4("Pairwise Summary"), DTOutput("tblPairSummary"),
                 h4("Pairwise Differences"),
                 DTOutput("tblPairDiffs"),
                 uiOutput("pairIndexLink")
               )
             )
    )
  )
)

# --------------------- Server ---------------------
server <- function(input, output, session) {

  # Session-scoped alias for artifact links
  current_alias <- reactiveVal(NULL)
  bindDiffDir <- function(dir) {
    dir <- sanitize_path(dir)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    alias <- paste0("rtf_diffs_", session$token)
    suppressWarnings(try(shiny::removeResourcePath(alias), silent = TRUE))  # silence first-run warning
    addResourcePath(alias, dir)
    current_alias(alias)
  }

  # NB: do not reset future::plan() in onSessionEnded — plan() is process-global
  # and resetting it on disconnect would kill parallelism for other users in a
  # multi-session deployment. The on.exit(plan(old_plan)) in runDir is enough.

  rv <- reactiveValues(summary_df = empty_summary_df(), results_done = 0L, total_files = 0L, pair_results = NULL)

  # Build a sanitized <a> tag for a server-side path under the session alias.
  # URL-encodes the basename (so quotes/brackets in filenames cannot break out
  # of the href attribute) and HTML-escapes the visible label.
  artifact_link <- function(alias, paths, label, download = FALSE) {
    enc <- vapply(basename(paths), function(s) {
      if (is.na(s)) NA_character_ else utils::URLencode(s, reserved = FALSE)
    }, character(1))
    href <- file.path(alias, enc)
    extra <- if (download) "download rel='noopener'" else "target='_blank' rel='noopener'"
    ifelse(is.na(paths), NA_character_,
           sprintf("<a href=\"%s\" %s>%s</a>",
                   htmltools::htmlEscape(href, attribute = TRUE),
                   extra, htmltools::htmlEscape(label)))
  }

  chunk_indices <- function(n, k) {
    if (n <= 0) return(list())
    starts <- seq(1, n, by = k)
    lapply(starts, function(s) { e <- min(s + k - 1, n); s:e })
  }

  reset_progress <- function() {
    rv$summary_df <- empty_summary_df()
    rv$results_done <- 0L; rv$total_files <- 0L
    updateProgressBar(session, id = "batchProgressBar", value = 0, status = "info",
                      title = "Ready to start")
    output$statusText <- renderText({ "" })
    output$indexLink <- renderUI({NULL})
  }

  # Clear Directory Tab
  observeEvent(input$clearDir, {
    reset_progress()
    updateTextInput(session, "dirA", value = "")
    updateTextInput(session, "dirB", value = "")
  })

  # Clear Pairwise Tab
  observeEvent(input$clearPair, {
    rv$pair_results <- NULL
    updateTextInput(session, "fileA", value = "")
    updateTextInput(session, "fileB", value = "")
    output$pairIndexLink <- renderUI({NULL})
  })

  # ---------------- Directory Compare ----------------
  observeEvent(input$runDir, {
    dirA <- sanitize_path(input$dirA)
    dirB <- sanitize_path(input$dirB)
    recursive <- isTRUE(input$recursive)
    prefixes <- if (length(input$prefix)) input$prefix else c("t","l")
    compare_mode <- input$mode
    batch_size <- as.integer(input$batchSize %||% 10)
    workers <- as.integer(input$workers %||% 3)
    ignore_ws <- isTRUE(input$ignoreWhitespace)
    diff_dir <- sanitize_path(input$diffdir)
    make_index <- isTRUE(input$makeIndex)
    per_top <- as.integer(input$ignoreTop_dir %||% 0)
    per_bottom <- as.integer(input$ignoreBottom_dir %||% 0)
    drop_meta <- isTRUE(input$dropMeta_dir)

    validate(
      need(dir.exists(dirA), paste("Directory A not found:", dirA)),
      need(dir.exists(dirB), paste("Directory B not found:", dirB))
    )

    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    try(plan(multisession, workers = workers), silent = TRUE)

    bindDiffDir(diff_dir)
    reset_progress()

    A <- list_rtf_by_prefix(dirA, prefixes, recursive = recursive)
    B <- list_rtf_by_prefix(dirB, prefixes, recursive = recursive)
    dupA <- names(which(table(A$file) > 1))
    dupB <- names(which(table(B$file) > 1))

    A1 <- subset(A, !(file %in% dupA))
    B1 <- subset(B, !(file %in% dupB))

    common <- sort(intersect(unique(A1$file), unique(B1$file)))
    file_pairs <- lapply(common, function(fname) {
      list(fname = fname, pa = A1$path[A1$file == fname][1], pb = B1$path[B1$file == fname][1])
    })

    onlyA <- setdiff(unique(A$file), unique(B$file))
    onlyB <- setdiff(unique(B$file), unique(A$file))

    make_only_rows <- function(files, where) {
      if (!length(files)) return(NULL)
      data.frame(
        file = files,
        type = vapply(files, infer_type, FUN.VALUE = character(1)),
        status = paste("Only in", where),
        identical = NA,
        diff_path = NA_character_,
        diff_csv  = NA_character_,
        note = "",
        stringsAsFactors = FALSE
      )
    }
    make_dup_rows <- function(files, where) {
      if (!length(files)) return(NULL)
      data.frame(
        file = files,
        type = vapply(files, infer_type, FUN.VALUE = character(1)),
        status = sprintf("Duplicate basenames in %s", where),
        identical = NA,
        diff_path = NA_character_,
        diff_csv  = NA_character_,
        note = "Resolve duplicates before comparing",
        stringsAsFactors = FALSE
      )
    }

    result_seed <- bind_rows(
      make_only_rows(onlyA, "A"),
      make_only_rows(onlyB, "B"),
      make_dup_rows(dupA, "A"),
      make_dup_rows(dupB, "B")
    )
    if (is.null(result_seed)) result_seed <- empty_summary_df()
    rv$summary_df <- result_seed

    total <- length(file_pairs)
    rv$total_files <- total

    if (total == 0) {
      updateProgressBar(session, id = "batchProgressBar", value = 100, status = "success", title = "No common files found")
      output$statusText <- renderText({"No common .rtf files to compare (see Only-in / Duplicates in Summary)."})
      if (isTRUE(make_index) && nrow(rv$summary_df) > 0) {
        idx <- write_diff_index(diff_dir,
                                summary_df = subset(rv$summary_df, status == "Different"),
                                title = "RTF Differences Index", filename = "index.html")
        attr(rv$summary_df, "index_path") <- idx
      }
      return(invisible())
    }

    total_batches <- ceiling(total / batch_size)
    showNotification(
      sprintf("Found %d common files. Processing in %d batches of up to %d files each.", total, total_batches, batch_size),
      type = "message", duration = 8
    )
    idx_chunks <- chunk_indices(total, batch_size)

    for (b in seq_along(idx_chunks)) {
      batch_indices <- idx_chunks[[b]]
      batch_pairs <- file_pairs[batch_indices]

      output$statusText <- renderText({
        sprintf("Starting Batch %d / %d (Files %d–%d of %d)...",
                b, total_batches, batch_indices[1], tail(batch_indices, 1), total)
      })

      batch_results <- future.apply::future_lapply(batch_pairs, function(pair) {
        tryCatch({
          compare_two_rtfs(
            file_a = pair$pa, file_b = pair$pb, compare_mode = compare_mode,
            diff_html_path = file.path(diff_dir, paste0(pair$fname, ".diff.html")),
            diff_csv_path  = file.path(diff_dir, paste0(pair$fname, ".diff.csv")),
            per_page_top = per_top, per_page_bottom = per_bottom,
            drop_meta = drop_meta,
            ignore_whitespace = ignore_ws
          )
        }, error = function(e) {
          data.frame(
            file=pair$fname, type=infer_type(pair$fname), status="Read/Compare error", identical=NA,
            diff_path=NA_character_, diff_csv=NA_character_, note=paste("Error:", conditionMessage(e)),
            stringsAsFactors = FALSE
          ) |>
            (\(df) list(summary_like = df, differences = data.frame()))()
        })
      }, future.seed = TRUE)

      for (res in batch_results) {
        rv$results_done <- rv$results_done + 1L
        rv$summary_df <- bind_rows(rv$summary_df, res$summary_like)

        pct <- round((rv$results_done / total) * 100)
        label <- sprintf("File %d of %d (Batch %d of %d)", rv$results_done, total, b, total_batches)
        updateProgressBar(session, id = "batchProgressBar", value = pct, title = label,
                          status = ifelse(rv$results_done < total, "info", "success"))
      }
    }

    if (isTRUE(make_index) && nrow(rv$summary_df) > 0) {
      idx <- write_diff_index(diff_dir,
                              summary_df = subset(rv$summary_df, status == "Different"),
                              title = "RTF Differences Index", filename = "index.html")
      attr(rv$summary_df, "index_path") <- idx
    }
    showNotification("All comparisons complete!", type = "message")
    output$statusText <- renderText({sprintf("Finished. %d files compared.", total)})
  })

  # Directory tables & links (defensive) ----
  output$tblSummary <- renderDT({
    df <- rv$summary_df
    req(!is.null(df))
    needed <- c("file","type","status","diff_path","diff_csv","note")
    if (!all(needed %in% names(df))) return(NULL)

    alias <- current_alias() %||% paste0("rtf_diffs_", session$token)
    df2 <- df[, needed, drop = FALSE]

    df2$diff_path <- artifact_link(alias, df2$diff_path, "open")
    df2$diff_csv  <- artifact_link(alias, df2$diff_csv,  "csv", download = TRUE)

    datatable(df2, escape = FALSE, options = list(pageLength = 25, scrollX = TRUE))
  })

  output$tblDiffs <- renderDT({
    df <- rv$summary_df
    req(!is.null(df))
    needed <- c("file","type","status","diff_path","diff_csv","note")
    if (!all(needed %in% names(df))) return(NULL)

    d <- subset(df, status == "Different",
                select = c("file","type","status","diff_path","diff_csv","note"))

    alias <- current_alias() %||% paste0("rtf_diffs_", session$token)
    d$diff_path <- artifact_link(alias, d$diff_path, "open")
    d$diff_csv  <- artifact_link(alias, d$diff_csv,  "csv", download = TRUE)

    datatable(d, escape = FALSE, options = list(pageLength = 25, scrollX = TRUE))
  })

  output$indexLink <- renderUI({
    df <- rv$summary_df
    req(!is.null(df))
    idx <- attr(df, "index_path")
    if (!is.null(idx) && file.exists(idx)) {
      alias <- current_alias() %||% paste0("rtf_diffs_", session$token)
      tags$p(HTML(paste0("Index: ", artifact_link(alias, idx, basename(idx)))))
    }
  })

  output$dlSummary <- downloadHandler(
    filename = function() "rtf_compare_summary.csv",
    content  = function(file) {
      df <- isolate(rv$summary_df); req(df)
      write.csv(df, file, row.names = FALSE, na = "")
    }
  )
  output$dlDiffsOnly <- downloadHandler(
    filename = function() "rtf_compare_differences_only.csv",
    content  = function(file) {
      df <- isolate(rv$summary_df); req(df)
      d <- subset(df, status == "Different")
      write.csv(d, file, row.names = FALSE, na = "")
    }
  )

  # ---------------- Pairwise Compare ----------------
  observeEvent(input$runPair, {
    # Bind diff dir as a Shiny resource path so the "open" links resolve.
    fn <- sanitize_path(input$pairDiffHtml)
    if (nzchar(fn)) {
      d <- dirname(fn)
      if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
      bindDiffDir(d)
    }

    fa <- sanitize_path(input$fileA); fb <- sanitize_path(input$fileB)
    if (!file.exists(fa)) { showNotification(paste("File A not found:", fa), type="error", duration=5); return(NULL) }
    if (!file.exists(fb)) { showNotification(paste("File B not found:", fb), type="error", duration=5); return(NULL) }

    out <- tryCatch({
      compare_two_rtfs(
        fa, fb, compare_mode = input$mode2,
        diff_html_path = if (nzchar(input$pairDiffHtml)) sanitize_path(input$pairDiffHtml) else NULL,
        diff_csv_path  = if (nzchar(input$pairDiffHtml))
          sub("\\.html?$", ".csv", sanitize_path(input$pairDiffHtml)) else NULL,
        per_page_top = as.integer(input$ignoreTop_pair %||% 0),
        per_page_bottom = as.integer(input$ignoreBottom_pair %||% 0),
        drop_meta = isTRUE(input$dropMeta_pair),
        ignore_whitespace = isTRUE(input$ignoreWhitespace_pair)
      )
    }, error = function(e) {
      list(
        summary_like = data.frame(
          file = paste(basename(fa), "vs", basename(fb)),
          type = "Pairwise",
          status = "Read/Compare error",
          identical = NA,
          diff_path = NA_character_,
          diff_csv  = NA_character_,
          note = paste("Error:", conditionMessage(e)),
          stringsAsFactors = FALSE
        ),
        differences = data.frame()
      )
    })

    disp <- process_diffs_to_wide_format(out$differences)
    if (nrow(disp) > 0) {
      showNotification(sprintf("Pairwise: %d differing blocks found.", nrow(disp)),
                       type = "warning", duration = 5)
    } else {
      showNotification("Pairwise: files are identical.", type = "message", duration = 5)
    }

    if (nzchar(input$pairDiffHtml) && isTRUE(input$pairIndex) && !is.na(out$summary_like$diff_path)) {
      write_diff_index(dirname(sanitize_path(input$pairDiffHtml)),
                       summary_df = transform(out$summary_like, type = "Pairwise"),
                       title = "RTF Differences Index", filename = "index.html")
      attr(out, "index_path") <- file.path(dirname(sanitize_path(input$pairDiffHtml)), "index.html")
    }
    rv$pair_results <- out
  })

  pairwise_display_data <- reactive({
    r <- rv$pair_results; req(r)
    process_diffs_to_wide_format(r$differences)
  })

  output$tblPairSummary <- renderDT({
    r <- rv$pair_results; if (is.null(r)) return(NULL)
    df <- r$summary_like[, c("file","type","status","diff_path","note")]
    alias <- current_alias() %||% paste0("rtf_diffs_", session$token)
    df$diff_path <- artifact_link(alias, df$diff_path, "open")
    datatable(df, escape = FALSE, options = list(dom = 't', paging = FALSE, scrollX = TRUE))
  })

  output$tblPairDiffs <- renderDT({
    df <- pairwise_display_data()
    if (is.null(df)) return(NULL)
    if (nrow(df)) {
      df$Position <- sprintf(
        "<span title='Page/Line A: %s | Global line A: %s   Page/Line B: %s | Global line B: %s'>%s<br/><small>Global line: A %s • B %s</small></span>",
        df$PositionA %||% "N/A", df$PositionRawA %||% "N/A",
        df$PositionB %||% "N/A", df$PositionRawB %||% "N/A",
        df$Position %||% "",
        df$PositionRawA %||% "N/A", df$PositionRawB %||% "N/A"
      )
    }
    datatable(df[, c("Position","Display A","Display B")],
              escape = FALSE, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })

  output$pairIndexLink <- renderUI({
    r <- rv$pair_results; if (is.null(r)) return(NULL)
    idx <- attr(r, "index_path")
    if (!is.null(idx) && file.exists(idx)) {
      alias <- current_alias() %||% paste0("rtf_diffs_", session$token)
      tags$p(HTML(paste0("Index: ", artifact_link(alias, idx, basename(idx)))))
    }
  })

  output$dlPairDiffs <- downloadHandler(
    filename = function() {
      r <- rv$pair_results; req(r)
      base_name <- gsub(" vs ", "_vs_", r$summary_like$file)
      base_name <- gsub("[^a-zA-Z0-9_.-]", "_", base_name)
      paste0(base_name, "_differences_", Sys.Date(), ".csv")
    },
    content = function(file) {
      display_df <- pairwise_display_data()
      req(display_df)
      drop_cols <- c("PositionRawA","PositionRawB","RawA","RawB")
      display_df_csv <- display_df[, setdiff(names(display_df), drop_cols), drop = FALSE]
      write.csv(display_df_csv, file, row.names = FALSE, na = "")
    }
  )
}

shinyApp(ui, server)

