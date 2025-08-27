# app.R — RTF Comparator (page-aware ignores; clean+raw indices; improved labels)
# Deps: shiny, DT, striprtf, diffobj, shinybusy

library(shiny)
library(DT)
library(striprtf)
library(diffobj)
library(tools)
library(shinybusy)

`%||%` <- function(x, y) if (length(x)) x else y

# --------------------- Helpers ---------------------

sanitize_path <- function(s) {
  s <- trimws(s)
  s <- gsub("^\\s*['\"]|['\"]\\s*$", "", s)  # strip quotes
  s <- gsub("\\\\", "/", s)                 # backslashes -> slashes
  s
}

# ---- Page splitter (uses explicit "Page X [of Y]" lines and formfeeds) ----
split_pages_strict <- function(lines, idx) {
  pages <- list(); pidx <- list()
  cur <- character(0); cidx <- integer(0)
  
  pat_page_footer <- "(?i)^\\s*([a-z]+\\s+)*page\\s+\\d+\\s*(of\\s+\\d+)?\\s*$"
  
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
    
    # explicit page footer line or "Page Break"
    if (grepl(pat_page_footer, ln) || grepl("(?i)^\\s*page\\s*break\\s*$", ln)) {
      flush_page()
      next
    }
    
    # form feed within the line
    if (grepl("\f", ln, fixed = TRUE)) {
      parts <- strsplit(ln, "\f", fixed = TRUE)[[1]]
      for (k in seq_along(parts)) {
        if (nzchar(trimws(parts[k]))) {
          cur  <- c(cur,  parts[k])
          cidx <- c(cidx, idx[i])
        }
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
    footer_patterns = c(
      "(?i)^\\s*([a-z]+\\s+)*page\\s+\\d+\\s*(of\\s+\\d+)?\\s*$",  # generic footer
      "(?i)^(program|output|log|listing|path|directory|run|printed|created|generated|date|time)\\s*[:=].*$",
      "((19|20)\\d{2}[-/.]\\d{1,2}[-/.]\\d{1,2})",
      "(\\b\\d{1,2}[-/.]\\d{1,2}[-/.]\\d{2,4}\\b)",
      "(\\b\\d{1,2}:\\d{2}(:\\d{2})?\\b)",
      "([A-Za-z]:\\\\[^\\r\\n]+)",
      "(\\\\\\\\[^\\r\\n]+)",
      "(/[^\\r\\n]+)"
    ),
    dehyphenate = TRUE
){
  # normalize to LF & tidy
  x <- gsub("\r", "\n", x, fixed = TRUE)
  x <- gsub("\u00A0", " ", x, fixed = TRUE)
  x <- gsub("[ \t]+", " ", x)
  x <- gsub("\n[ \t]+", "\n", x)
  if (dehyphenate) x <- gsub("-\\n", "", x)
  x <- gsub("\n{3,}", "\n\n", x)
  
  all_lines <- unlist(strsplit(x, "\n", fixed = TRUE))
  if (!length(all_lines)) {
    return(list(
      text = "", lines = character(0),
      idx_raw = integer(0), idx_clean = integer(0),
      label_map_clean = character(0),
      clean_to_raw = integer(0),
      map_df = data.frame(clean=integer(0), raw=integer(0), page=integer(0), line_in_page=integer(0))
    ))
  }
  all_idx_raw <- seq_along(all_lines)
  
  pg <- split_pages_strict(all_lines, all_idx_raw)
  pages <- pg$lines
  pidx  <- pg$idx
  
  kept_lines <- character(0)
  kept_raw   <- integer(0)
  kept_page  <- integer(0)
  kept_lip   <- integer(0)
  
  pat_footer <- paste(footer_patterns, collapse = "|")
  
  for (p in seq_along(pages)) {
    lines <- pages[[p]]
    idx   <- pidx [[p]]
    if (!length(lines)) next
    
    while (length(lines) && !nzchar(lines[1])) { lines <- lines[-1]; idx <- idx[-1] }
    if (!length(lines)) next
    while (length(lines) && !nzchar(tail(lines, 1))) { lines <- head(lines, -1); idx <- head(idx, -1) }
    if (!length(lines)) next
    
    if (drop_trailing_meta && length(lines)) {
      i <- length(lines)
      while (i > 0 && grepl(pat_footer, lines[i], perl = TRUE)) i <- i - 1L
      if (i < length(lines)) {
        lines <- lines[seq_len(i)]
        idx   <- idx  [seq_len(i)]
      }
    }
    
    if (per_page_ignore_top > 0L && length(lines)) {
      k <- min(per_page_ignore_top, length(lines))
      if (k > 0L) { lines <- lines[-seq_len(k)]; idx <- idx[-seq_len(k)] }
    }
    
    if (per_page_ignore_bottom > 0L && length(lines)) {
      if (length(lines) > per_page_ignore_bottom) {
        lines <- head(lines, -per_page_ignore_bottom)
        idx   <- head(idx,   -per_page_ignore_bottom)
      } else {
        lines <- character(0); idx <- integer(0)
      }
    }
    
    if (length(lines)) {
      kept_lines <- c(kept_lines, lines)
      kept_raw   <- c(kept_raw,   idx)
      kept_page  <- c(kept_page,  rep.int(p, length(lines)))
      kept_lip   <- c(kept_lip,   seq_len(length(lines)))
    }
  }
  
  kept_clean <- seq_along(kept_lines)
  
  map_df <- data.frame(
    clean = kept_clean,
    raw   = kept_raw,
    page  = kept_page,
    line_in_page = kept_lip,
    stringsAsFactors = FALSE
  )
  
  label_map_clean <- setNames(
    paste0("p", kept_page, ":l", kept_lip,
           " (global=", kept_clean, ", raw=", kept_raw, ")"),
    as.character(kept_clean)
  )
  
  clean_to_raw <- kept_raw
  names(clean_to_raw) <- as.character(kept_clean)
  
  list(
    text  = paste(kept_lines, collapse = "\n"),
    lines = kept_lines,
    idx_raw   = kept_raw,
    idx_clean = kept_clean,
    label_map_clean = label_map_clean,
    clean_to_raw = clean_to_raw,
    map_df = map_df
  )
}

# ---- SEGMENTATION + MAPPING (returns maps for CLEAN and RAW) ----
make_segments <- function(lines, idx_clean, idx_raw, mode = c("paragraph","line","token")) {
  mode <- match.arg(mode)
  
  if (mode == "line") {
    segs <- lines
    map_clean <- idx_clean
    map_raw   <- idx_raw
    
  } else if (mode == "paragraph") {
    segs <- character(0)
    map_clean <- integer(0)
    map_raw   <- integer(0)
    
    n <- length(lines); i <- 1L
    while (i <= n) {
      while (i <= n && !nzchar(lines[i])) i <- i + 1L
      if (i > n) break
      start <- i
      blk <- character(0)
      while (i <= n && nzchar(lines[i])) { blk <- c(blk, lines[i]); i <- i + 1L }
      segs      <- c(segs, paste(blk, collapse = "\n"))
      map_clean <- c(map_clean, idx_clean[start])
      map_raw   <- c(map_raw,   idx_raw[start])
    }
    if (!length(segs)) { segs <- character(0); map_clean <- integer(0); map_raw <- integer(0) }
    
  } else { # token
    tokens <- character(0); map_clean <- integer(0); map_raw <- integer(0)
    for (k in seq_along(lines)) {
      tks <- unlist(strsplit(lines[k], "\\s+", perl = TRUE))
      tks <- tks[nzchar(tks)]
      if (length(tks)) {
        tokens   <- c(tokens, tks)
        map_clean <- c(map_clean, rep(idx_clean[k], length(tks)))
        map_raw   <- c(map_raw,   rep(idx_raw[k],   length(tks)))
      }
    }
    segs <- tokens
  }
  
  list(
    segs = segs,
    map_clean = map_clean,
    map_raw   = map_raw,
    clean_to_raw = setNames(map_raw, as.character(map_clean))
  )
}

# Remove diff noise
clean_raw_diff <- function(raw_lines) {
  if (!length(raw_lines)) return(raw_lines)
  keep <- !grepl('^\\s*(@@|<\\s*(Aseg|segA\\$segs)\\b|>\\s*(Bseg|segB\\$segs)\\b|[<>]\\s*$)', raw_lines)
  out <- raw_lines[keep]; out[nzchar(out)]
}

# Parse "raw" diff with hunk headers into a simple data frame (lineA/lineB are *segment* indices)
parse_diff_with_map <- function(raw, mapA, mapB) {
  df <- data.frame(lineA = integer(), lineB = integer(), diff = character(),
                   stringsAsFactors = FALSE)
  if (!length(raw)) return(df)
  curA <- curB <- NA_integer_
  for (ln in raw) {
    if (grepl("^@@", ln)) {
      nums <- unlist(regmatches(ln, gregexpr("[0-9]+", ln)))
      if (length(nums) >= 4) { curA <- as.integer(nums[1]); curB <- as.integer(nums[3]) }
      next
    }
    if (startsWith(ln, "<")) {
      la <- if (!is.na(curA) && curA >= 1 && curA <= length(mapA)) mapA[curA] else NA_integer_
      df <- rbind(df, data.frame(lineA = la, lineB = NA_integer_, diff = ln, stringsAsFactors = FALSE))
      curA <- curA + 1L
    } else if (startsWith(ln, ">")) {
      lb <- if (!is.na(curB) && curB >= 1 && curB <= length(mapB)) mapB[curB] else NA_integer_
      df <- rbind(df, data.frame(lineA = NA_integer_, lineB = lb, diff = ln, stringsAsFactors = FALSE))
      curB <- curB + 1L
    } else {
      la <- if (!is.na(curA) && curA >= 1 && curA <= length(mapA)) mapA[curA] else NA_integer_
      lb <- if (!is.na(curB) && curB >= 1 && curB <= length(mapB)) mapB[curB] else NA_integer_
      df <- rbind(df, data.frame(lineA = la, lineB = lb, diff = ln, stringsAsFactors = FALSE))
      curA <- curA + 1L; curB <- curB + 1L
    }
  }
  rownames(df) <- NULL
  df
}

filter_diff_rows <- function(d) {
  if (is.null(d) || !nrow(d)) return(d[0, , drop = FALSE])
  real <- grepl("^\\s*[<>]", d$diff)
  structural <- grepl("^\\s*(@@|<\\s*(Aseg|segA\\$segs)\\b|>\\s*(Bseg|segB\\$segs)\\b|[<>]\\s*$)", d$diff)
  out <- d[ real & !structural, , drop = FALSE ]
  rownames(out) <- NULL
  out
}

write_diff_index <- function(diff_dir, summary_df = NULL,
                             title = "RTF Differences Index", filename = "index.html") {
  if (!dir.exists(diff_dir)) dir.create(diff_dir, recursive = TRUE, showWarnings = FALSE)
  diff_files <- list.files(diff_dir, pattern = "\\.diff\\.html$", ignore.case = TRUE, full.names = FALSE)
  n <- length(diff_files); now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  esc <- function(x){x<-gsub("&","&amp;",x,fixed=TRUE);x<-gsub("<","&lt;",x,fixed=TRUE);gsub(">","&gt;",x,fixed=TRUE)}
  header <- if (!is.null(summary_df) && nrow(summary_df))
    "<tr><th>#</th><th>Type</th><th>Status</th><th>Diff file</th></tr>"
  else "<tr><th>#</th><th>Diff file</th></tr>"
  make_row <- function(f,i){
    if (!is.null(summary_df) && nrow(summary_df)) {
      row <- summary_df[ basename(summary_df$diff_path) == basename(f), , drop = FALSE]
      typ <- if (nrow(row)) row$type[1] else ""; stat <- if (nrow(row)) row$status[1] else "Different"
      sprintf("<tr><td>%d</td><td>%s</td><td>%s</td><td><a href=\"%s\">%s</a></td></tr>",
              i, esc(typ), esc(stat), f, esc(basename(f)))
    } else {
      sprintf("<tr><td>%d</td><td><a href=\"%s\">%s</a></td></tr>", i, f, esc(basename(f)))
    }
  }
  rows <- if (n) paste0(vapply(seq_along(diff_files), function(i) make_row(diff_files[i], i),
                               character(1)), collapse="\n")
  else "<tr><td colspan='4'><em>No diff artifacts found.</em></td></tr>"
  html <- sprintf(
    '<!doctype html><html><head><meta charset="utf-8"><title>%s</title>
<style>body{font-family:system-ui,-apple-system,Segoe UI,Roboto,sans-serif;margin:24px}
table{border-collapse:collapse;width:100%%}th,td{border:1px solid #ddd;padding:8px}
th{background:#f6f6f6;text-align:left}tr:nth-child(even){background:#fafafa}.meta{color:#666;margin-bottom:12px}</style>
</head><body><h1>%s</h1><div class="meta">Generated: %s • Directory: %s • Files: %d</div>
<table><thead>%s</thead><tbody>%s</tbody></table></body></html>',
    title, title, now, normalizePath(diff_dir, winslash="/"), n, header, rows)
  out <- file.path(diff_dir, filename); writeLines(html, out, useBytes = TRUE); out
}

infer_type <- function(fn)
  if (grepl("^[Tt]", fn)) "Table" else if (grepl("^[Ll]", fn)) "Listing" else "Other"

.list_by_prefix <- function(dir, prefixes, recursive) {
  all_files <- list.files(dir, recursive = recursive, full.names = TRUE)
  if (!length(all_files)) return(data.frame(path=character(), file=character(), ext=character()))
  base <- basename(all_files)
  keep <- Reduce(`|`, lapply(prefixes, function(p) grepl(paste0("^", p), base, ignore.case = TRUE)))
  df <- data.frame(path = all_files[keep], file = base[keep], stringsAsFactors = FALSE)
  df$ext <- tolower(file_ext(df$file)); df
}

# ----- Directory comparator (summary + diff html) -----
compare_rtf_dirs <- function(dir_a, dir_b, file_prefix = c("t","l"), recursive = TRUE,
                             diff_dir = "rtf_diffs",
                             compare_mode = c("paragraph","line","token"),
                             per_page_top = 0L, per_page_bottom = 0L,
                             drop_meta = TRUE) {
  compare_mode <- match.arg(compare_mode)
  A <- .list_by_prefix(dir_a, file_prefix, recursive)
  B <- .list_by_prefix(dir_b, file_prefix, recursive)
  
  dupA <- names(which(table(A$file) > 1)); dupB <- names(which(table(B$file) > 1))
  common <- intersect(unique(A$file), unique(B$file))
  
  result <- data.frame(file=character(), type=character(), status=character(),
                       identical=logical(), diff_path=character(), note=character(),
                       stringsAsFactors = FALSE)
  result$diff_lines <- I(list())
  
  add_row <- function(...) {
    row <- data.frame(..., stringsAsFactors = FALSE)
    row$diff_lines <- I(list(character(0)))
    result <<- rbind(result, row)
  }
  
  if (length(dupA)) for (f in dupA)
    add_row(file=f, type=infer_type(f), status="Duplicate in A", identical=NA,
            diff_path=NA_character_, note="Multiple files with same name in A")
  if (length(dupB)) for (f in dupB)
    add_row(file=f, type=infer_type(f), status="Duplicate in B", identical=NA,
            diff_path=NA_character_, note="Multiple files with same name in B")
  
  A1 <- subset(A, !(file %in% dupA)); B1 <- subset(B, !(file %in% dupB))
  onlyA <- setdiff(unique(A1$file), unique(B1$file))
  if (length(onlyA)) for (f in onlyA)
    add_row(file=f, type=infer_type(f), status="Only in A", identical=NA,
            diff_path=NA_character_, note="Only in A")
  onlyB <- setdiff(unique(B1$file), unique(A1$file))
  if (length(onlyB)) for (f in onlyB)
    add_row(file=f, type=infer_type(f), status="Only in B", identical=NA,
            diff_path=NA_character_, note="Only in B")
  
  if (!dir.exists(diff_dir)) dir.create(diff_dir, recursive = TRUE, showWarnings = FALSE)
  
  for (fname in intersect(common, intersect(unique(A1$file), unique(B1$file)))) {
    pa <- A1$path[A1$file == fname][1]; pb <- B1$path[B1$file == fname][1]
    if (tolower(file_ext(pa)) != "rtf" || tolower(file_ext(pb)) != "rtf") {
      add_row(file=fname, type=infer_type(fname), status="Unsupported", identical=NA,
              diff_path=NA_character_, note="Non-RTF extension"); next
    }
    ta_raw <- tryCatch(striprtf::read_rtf(pa), error=function(e) e)
    tb_raw <- tryCatch(striprtf::read_rtf(pb), error=function(e) e)
    if (inherits(ta_raw,"error") || inherits(tb_raw,"error")) {
      note <- if (inherits(ta_raw,"error")) paste("Read error A:", conditionMessage(ta_raw))
      else paste("Read error B:", conditionMessage(tb_raw))
      add_row(file=fname, type=infer_type(fname), status="Read error", identical=NA,
              diff_path=NA_character_, note=note); next
    }
    
    na <- normalize_keep_index(ta_raw, per_page_top, per_page_bottom, drop_trailing_meta = drop_meta)
    nb <- normalize_keep_index(tb_raw, per_page_top, per_page_bottom, drop_trailing_meta = drop_meta)
    
    segA <- make_segments(na$lines, na$idx_clean, na$idx_raw, compare_mode)
    segB <- make_segments(nb$lines, nb$idx_clean, nb$idx_raw, compare_mode)
    
    if (identical(segA$segs, segB$segs)) {
      add_row(file=fname, type=infer_type(fname), status="Identical", identical=TRUE,
              diff_path=NA_character_, note="Identical")
    } else {
      raw_diff <- clean_raw_diff(
        capture.output(diffobj::diffChr(segA$segs, segB$segs,
                                        format="raw", pager="off",
                                        tar.banner="", cur.banner=""))
      )
      html <- paste(
        capture.output(show(diffobj::diffChr(segA$segs, segB$segs,
                                             format="html", pager="off",
                                             tar.banner="", cur.banner=""))),
        collapse="\n"
      )
      diff_file <- file.path(diff_dir, paste0(fname, ".diff.html"))
      writeLines(html, diff_file)
      
      row <- data.frame(file=fname, type=infer_type(fname), status="Different", identical=FALSE,
                        diff_path=diff_file, note=sprintf("Differences found (%s-level)", compare_mode),
                        stringsAsFactors = FALSE)
      row$diff_lines <- I(list(raw_diff %||% character(0)))
      result <- rbind(result, row)
    }
  }
  if (nrow(result)) result[order(result$file), , drop = FALSE] else result
}

# ----- Pairwise (page-aware; clean+raw positions) -----
compare_two_rtfs <- function(file_a, file_b,
                             compare_mode = c("paragraph","line","token"),
                             diff_html_path = NULL,
                             per_page_top = 0L, per_page_bottom = 0L,
                             drop_meta = TRUE) {
  compare_mode <- match.arg(compare_mode)
  
  ta_raw <- striprtf::read_rtf(file_a)
  tb_raw <- striprtf::read_rtf(file_b)
  
  na <- normalize_keep_index(ta_raw, per_page_top, per_page_bottom, drop_trailing_meta = drop_meta)
  nb <- normalize_keep_index(tb_raw, per_page_top, per_page_bottom, drop_trailing_meta = drop_meta)
  
  segA <- make_segments(na$lines, na$idx_clean, na$idx_raw, compare_mode)
  segB <- make_segments(nb$lines, nb$idx_clean, nb$idx_raw, compare_mode)
  
  same <- identical(segA$segs, segB$segs)
  
  raw_diff_all <- if (same) character(0) else
    capture.output(diffobj::diffChr(segA$segs, segB$segs,
                                    format = "raw", pager = "off",
                                    tar.banner="", cur.banner=""))
  
  diffs_df <- if (length(raw_diff_all)) {
    parsed <- parse_diff_with_map(raw_diff_all, segA$map_clean, segB$map_clean)  # CLEAN maps
    filter_diff_rows(parsed)
  } else {
    data.frame(lineA=integer(), lineB=integer(), diff=character(), stringsAsFactors = FALSE)
  }
  
  # Add raw line indices using clean->raw translator
  diffs_df$rawA <- ifelse(is.na(diffs_df$lineA), NA_integer_,
                          as.integer(segA$clean_to_raw[as.character(diffs_df$lineA)]))
  diffs_df$rawB <- ifelse(is.na(diffs_df$lineB), NA_integer_,
                          as.integer(segB$clean_to_raw[as.character(diffs_df$lineB)]))
  
  # Human labels (page:line with both indices)
  to_label_clean <- function(v, lab) {
    if (!length(v)) return(character(0))
    key <- as.character(v)
    out <- unname(lab[key])
    out[is.na(out) | is.na(v)] <- NA_character_
    out
  }
  diffs_df$posA <- to_label_clean(diffs_df$lineA, na$label_map_clean)
  diffs_df$posB <- to_label_clean(diffs_df$lineB, nb$label_map_clean)
  
  diff_path <- NA_character_
  if (!same && !is.null(diff_html_path)) {
    dir.create(dirname(diff_html_path), recursive = TRUE, showWarnings = FALSE)
    html <- paste(capture.output(show(diffobj::diffChr(
      segA$segs, segB$segs, format = "html", pager = "off",
      tar.banner="", cur.banner=""))), collapse = "\n")
    writeLines(html, diff_html_path)
    diff_path <- normalizePath(diff_html_path, winslash = "/")
  }
  
  file_label <- if (basename(file_a) == basename(file_b))
    basename(file_a) else paste(basename(file_a), "vs", basename(file_b))
  type_label <- infer_type(basename(file_a))
  
  summary_like <- data.frame(
    file       = file_label,
    type       = type_label,
    status     = if (same) "Identical" else "Different",
    identical  = same,
    diff_path  = diff_path,
    note       = if (same) "Identical" else sprintf("Differences found (%s-level)", compare_mode),
    stringsAsFactors = FALSE
  )
  summary_like$diff_lines <- I(list(character(0)))
  
  list(summary_like = summary_like, differences = diffs_df)
}

# --------------------- UI ---------------------
ui <- fluidPage(
  shinybusy::add_busy_bar(color = "#29b6f6", height = "3px"),
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
                 selectInput("mode", "Compare granularity",
                             c("paragraph","line","token"), selected="paragraph"),
                 checkboxInput("recursive", "Search subfolders (recursive)", TRUE),
                 textInput("diffdir", "Diff output folder", "rtf_diffs"),
                 checkboxInput("makeIndex", "Create index.html with links", TRUE),
                 
                 numericInput("ignoreTop_dir", "Ignore first N lines (per page)", value = 0, min = 0, max = 500, step = 1),
                 numericInput("ignoreBottom_dir", "Ignore last N lines (per page)", value = 0, min = 0, max = 500, step = 1),
                 checkboxInput("dropMeta_dir", "Ignore footer meta on each page", TRUE),
                 
                 actionButton("runDir", "Run comparison", class = "btn-primary"),
                 tags$hr(),
                 downloadButton("dlSummary", "Download SUMMARY (CSV)"),
                 downloadButton("dlDiffsOnly", "Download DIFFERENCES only (CSV)")
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
                 selectInput("mode2", "Compare granularity",
                             c("paragraph","line","token"), selected="paragraph"),
                 textInput("pairDiffHtml", "Write HTML diff to (optional)",
                           "rtf_diffs/single_pair.diff.html"),
                 checkboxInput("pairIndex", "Update/create index.html in diff folder", TRUE),
                 
                 numericInput("ignoreTop_pair", "Ignore first N lines (per page)", value = 0, min = 0, max = 500, step = 1),
                 numericInput("ignoreBottom_pair", "Ignore last N lines (per page)", value = 0, min = 0, max = 500, step = 1),
                 checkboxInput("dropMeta_pair", "Ignore footer meta on each page", TRUE),
                 
                 actionButton("runPair", "Run pairwise comparison", class = "btn-primary"),
                 tags$hr(),
                 downloadButton("dlPairDiffs", "Download pairwise DIFF (CSV)")
               ),
               mainPanel(
                 h4("Pairwise Summary"), DTOutput("tblPairSummary"),
                 h4("Pairwise Differences (page:line; clean + raw indices)"),
                 DTOutput("tblPairDiffs"),
                 uiOutput("pairIndexLink")
               )
             )
    )
  )
)

# --------------------- Server ---------------------
server <- function(input, output, session) {
  
  # serve diff html from the app directory
  addResourcePath("rtf_diffs", sanitize_path("rtf_diffs"))
  
  # Directory compare
  resDir <- eventReactive(input$runDir, {
    dirA <- sanitize_path(input$dirA)
    dirB <- sanitize_path(input$dirB)
    validate(
      need(dir.exists(dirA), paste("Directory A not found:", dirA)),
      need(dir.exists(dirB), paste("Directory B not found:", dirB))
    )
    prefixes <- if (length(input$prefix)) input$prefix else c("t","l")
    
    A <- .list_by_prefix(dirA, prefixes, isTRUE(input$recursive))
    B <- .list_by_prefix(dirB, prefixes, isTRUE(input$recursive))
    dupA <- names(which(table(A$file) > 1)); dupB <- names(which(table(B$file) > 1))
    A1 <- subset(A, !(file %in% dupA)); B1 <- subset(B, !(file %in% dupB))
    common <- intersect(unique(A1$file), unique(B1$file))
    showNotification(sprintf("Matched (by prefix): A=%d, B=%d, Common=%d",
                             nrow(A), nrow(B), length(common)),
                     type = if (length(common)) "message" else "warning", duration = 5)
    
    r <- compare_rtf_dirs(
      dir_a = dirA, dir_b = dirB, file_prefix = prefixes,
      recursive = isTRUE(input$recursive),
      diff_dir = sanitize_path(input$diffdir),
      compare_mode = input$mode,
      per_page_top = as.integer(input$ignoreTop_dir %||% 0),
      per_page_bottom = as.integer(input$ignoreBottom_dir %||% 0),
      drop_meta = isTRUE(input$dropMeta_dir)
    )
    if (isTRUE(input$makeIndex) && nrow(r)) {
      idx <- write_diff_index(sanitize_path(input$diffdir),
                              summary_df = subset(r, status == "Different"),
                              title = "RTF Differences Index", filename = "index.html")
      attr(r, "index_path") <- idx
    }
    r
  })
  
  output$tblSummary <- renderDT({
    r <- resDir(); if (is.null(r)) return(NULL)
    df <- r[, c("file","type","status","diff_path","note")]
    df$diff_path <- ifelse(
      is.na(df$diff_path), NA,
      sprintf("<a href='%s' target='_blank'>open</a>",
              file.path("rtf_diffs", basename(df$diff_path)))
    )
    datatable(df, escape = FALSE, options = list(pageLength = 25, scrollX = TRUE))
  })
  
  output$tblDiffs <- renderDT({
    r <- resDir(); if (is.null(r)) return(NULL)
    d <- subset(r, status == "Different")[, c("file","type","status","diff_path")]
    d$diff_path <- ifelse(
      is.na(d$diff_path), NA,
      sprintf("<a href='%s' target='_blank'>open</a>",
              file.path("rtf_diffs", basename(d$diff_path)))
    )
    datatable(d, escape = FALSE, options = list(pageLength = 25, scrollX = TRUE))
  })
  
  output$indexLink <- renderUI({
    r <- resDir(); if (is.null(r)) return(NULL)
    idx <- attr(r, "index_path")
    if (!is.null(idx) && file.exists(idx)) {
      tags$p(HTML(sprintf("Index: <a href='%s' target='_blank'>%s</a>",
                          file.path("rtf_diffs", basename(idx)),
                          basename(idx))))
    }
  })
  
  output$dlSummary <- downloadHandler(
    filename = function() "rtf_compare_summary.csv",
    content  = function(file) {
      r <- resDir(); req(r)
      write.csv(r[, !(names(r) %in% "diff_lines"), drop = FALSE], file, row.names = FALSE)
    }
  )
  output$dlDiffsOnly <- downloadHandler(
    filename = function() "rtf_compare_differences_only.csv",
    content  = function(file) {
      r <- resDir(); req(r)
      d <- subset(r, status == "Different")
      write.csv(d[, !(names(d) %in% "diff_lines"), drop = FALSE], file, row.names = FALSE)
    }
  )
  
  # Pairwise compare
  resPair <- eventReactive(input$runPair, {
    fa <- sanitize_path(input$fileA); fb <- sanitize_path(input$fileB)
    if (!file.exists(fa)) { showNotification(paste("File A not found:", fa), type="error", duration=5); return(NULL) }
    if (!file.exists(fb)) { showNotification(paste("File B not found:", fb), type="error", duration=5); return(NULL) }
    
    out <- compare_two_rtfs(
      fa, fb, compare_mode = input$mode2,
      diff_html_path = if (nzchar(input$pairDiffHtml)) sanitize_path(input$pairDiffHtml) else NULL,
      per_page_top = as.integer(input$ignoreTop_pair %||% 0),
      per_page_bottom = as.integer(input$ignoreBottom_pair %||% 0),
      drop_meta = isTRUE(input$dropMeta_pair)
    )
    
    if (nrow(out$differences)) showNotification(sprintf("Pairwise: %d diff rows.", nrow(out$differences)),
                                                type="warning", duration=5)
    else showNotification("Pairwise: files are identical.", type="message", duration=5)
    
    if (nzchar(input$pairDiffHtml) && isTRUE(input$pairIndex) && !is.na(out$summary_like$diff_path)) {
      write_diff_index(dirname(sanitize_path(input$pairDiffHtml)),
                       summary_df = transform(out$summary_like, type = "Pairwise"),
                       title = "RTF Differences Index", filename = "index.html")
      attr(out, "index_path") <- file.path(dirname(sanitize_path(input$pairDiffHtml)), "index.html")
    }
    out
  })
  
  output$tblPairSummary <- renderDT({
    r <- resPair(); if (is.null(r)) return(NULL)
    df <- r$summary_like[, c("file","type","status","diff_path","note")]
    df$diff_path <- ifelse(
      is.na(df$diff_path), NA,
      sprintf("<a href='%s' target='_blank'>open</a>",
              file.path("rtf_diffs", basename(df$diff_path)))
    )
    datatable(df, escape = FALSE, options = list(dom = 't', paging = FALSE, scrollX = TRUE))
  })
  
  output$tblPairDiffs <- renderDT({
    r <- resPair(); if (is.null(r)) return(NULL)
    d <- r$differences[, c("posA","posB","rawA","rawB","diff")]
    # simplify posA/posB labels: keep only "pX:lY"
    d$posA <- sub("^([^ ]+).*", "\\1", d$posA)
    d$posB <- sub("^([^ ]+).*", "\\1", d$posB)
    d$diff <- sub("^\\s*[<>]\\s*", "", d$diff)  # readability
    datatable(d[, c("posA","posB","diff")], options = list(pageLength = 25, scrollX = TRUE))
  })
  
  output$pairIndexLink <- renderUI({
    r <- resPair(); if (is.null(r)) return(NULL)
    idx <- attr(r, "index_path")
    if (!is.null(idx) && file.exists(idx)) {
      tags$p(HTML(sprintf("Index: <a href='%s' target='_blank'>%s</a>",
                          file.path("rtf_diffs", basename(idx)),
                          basename(idx))))
    }
  })
}

shinyApp(ui, server)
