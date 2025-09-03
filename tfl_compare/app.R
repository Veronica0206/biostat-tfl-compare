# app.R
library(shiny)
library(dplyr)
library(shinyjs)
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
  s <- gsub("\\\\", "/", s)                  # backslashes -> slashes
  s <- normalizePath(path.expand(s), winslash = "/", mustWork = FALSE)
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
    
    # PCRE needed for (?i)
    if (grepl(pat_page_footer, ln, perl = TRUE) ||
        grepl("(?i)^\\s*page\\s*break\\s*$", ln, perl = TRUE)) {
      flush_page()
      next
    }
    
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
      "(?i)^\\s*([a-z]+\\s+)*page\\s+\\d+\\s*(of\\s+\\d+)?\\s*$",
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
  x <- gsub("\r", "\n", x, fixed = TRUE)
  x <- gsub("\u00A0", " ", x, fixed = TRUE)
  x <- gsub("[ \t]+", " ", x)
  x <- gsub("\n[ \t]+", "\n", x)
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
  
  clean_to_raw <- kept_raw
  names(clean_to_raw) <- as.character(kept_clean)
  
  list(
    text  = paste(kept_lines, collapse = "\n"),
    lines = kept_lines,
    idx_raw   = kept_raw,
    idx_clean = kept_clean,
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
        tokens    <- c(tokens, tks)
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

write_diff_index <- function(diff_dir, summary_df = NULL,
                             title = "RTF Differences Index", filename = "index.html") {
  if (!dir.exists(diff_dir)) dir.create(diff_dir, recursive = TRUE, showWarnings = FALSE)
  diff_files <- list.files(diff_dir, pattern = "\\.diff\\.html$", ignore.case = TRUE, full.names = FALSE)
  n <- length(diff_files); now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  esc <- function(x){x<-gsub("&","&amp;",x,fixed=TRUE);x<-gsub("<","&lt;",x,fixed=TRUE);gsub(">","&gt;",x,fixed=TRUE)}
  header <- if (!is.null(summary_df) && nrow(summary_df))
    "<tr><th>#</th><th>Type</th><th>Status</th><th>Diff (HTML)</th><th>CSV</th></tr>"
  else "<tr><th>#</th><th>Diff file</th></tr>"
  make_row <- function(f,i){
    if (!is.null(summary_df) && nrow(summary_df)) {
      row <- summary_df[ basename(summary_df$diff_path) == basename(f), , drop = FALSE]
      typ <- if (nrow(row)) row$type[1] else ""; stat <- if (nrow(row)) row$status[1] else "Different"
      csv <- if (nrow(row) && !is.na(row$diff_csv[1]))
        sprintf("<a href=\"%s\">csv</a>", basename(row$diff_csv[1])) else ""
      sprintf("<tr><td>%d</td><td>%s</td><td>%s</td><td><a href=\"%s\">%s</a></td><td>%s</td></tr>",
              i, esc(typ), esc(stat), f, esc(basename(f)), csv)
    } else {
      sprintf("<tr><td>%d</td><td><a href=\"%s\">%s</a></td></tr>", i, f, esc(basename(f)))
    }
  }
  rows <- if (n) paste0(vapply(seq_along(diff_files), function(i) make_row(diff_files[i], i),
                               character(1)), collapse="\n")
  else "<tr><td colspan='5'><em>No diff artifacts found.</em></td></tr>"
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

# ---- LCS-based edit script (robust when raw diff has no @@ headers) ----
lcs_edits <- function(a, b) {
  n <- length(a); m <- length(b)
  if (!n && !m) return(data.frame(op=character(), i=integer(), j=integer(), stringsAsFactors = FALSE))
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

# ---- HELPER to process a diff data.frame into the wide, side-by-side format ----
process_diffs_to_wide_format <- function(d) {
  if (is.null(d) || !nrow(d)) {
    return(data.frame(
      Position = character(), PositionA = character(), PositionB = character(),
      `Display A` = character(), `Display B` = character(),
      stringsAsFactors = FALSE, check.names = FALSE
    ))
  }
  
  is_del <- startsWith(d$diff, "<")
  is_add <- startsWith(d$diff, ">")
  block_starts <- is_del | (is_add & !dplyr::lag(is_del, default = FALSE))
  d$block_id <- cumsum(block_starts)
  
  d %>%
    dplyr::group_by(block_id) %>%
    dplyr::summarise(
      page_A = pageA[!is.na(pageA)][1],
      lip_A  = lipA[!is.na(lipA)][1],
      page_B = pageB[!is.na(pageB)][1],
      lip_B  = lipB[!is.na(lipB)][1],
      `Display A` = sub("^\\s*[<>]\\s*", "", diff[startsWith(diff, "<")][1]),
      `Display B` = sub("^\\s*[<>]\\s*", "", diff[startsWith(diff, ">")][1]),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      page = dplyr::coalesce(page_A, page_B),
      lip  = dplyr::coalesce(lip_A,  lip_B),
      Position  = dplyr::case_when(
        !is.na(page)   & !is.na(lip)   ~ paste0("page ", page, " line ", lip),
        !is.na(page_A) & !is.na(lip_A) ~ paste0("page ", page_A, " line ", lip_A),
        !is.na(page_B) & !is.na(lip_B) ~ paste0("page ", page_B, " line ", lip_B),
        TRUE ~ NA_character_
      ),
      PositionA = ifelse(!is.na(page_A) & !is.na(lip_A), paste0("page ", page_A, " line ", lip_A), NA_character_),
      PositionB = ifelse(!is.na(page_B) & !is.na(lip_B), paste0("page ", page_B, " line ", lip_B), NA_character_)
    ) %>%
    dplyr::select(Position, PositionA, PositionB, `Display A`, `Display B`)
}

# ----- Pairwise (page-aware; clean+raw positions) -----
compare_two_rtfs <- function(file_a, file_b,
                             compare_mode = c("paragraph","line","token"),
                             diff_html_path = NULL,
                             diff_csv_path  = NULL,
                             per_page_top = 0L, per_page_bottom = 0L,
                             drop_meta = TRUE) {
  compare_mode <- match.arg(compare_mode)
  
  ta_raw <- striprtf::read_rtf(file_a)
  tb_raw <- striprtf::read_rtf(file_b)
  
  ta_raw <- iconv(ta_raw, to = "UTF-8"); tb_raw <- iconv(tb_raw, to = "UTF-8")
  
  na <- normalize_keep_index(ta_raw, per_page_top, per_page_bottom, drop_trailing_meta = drop_meta)
  nb <- normalize_keep_index(tb_raw, per_page_top, per_page_bottom, drop_trailing_meta = drop_meta)
  
  segA <- make_segments(na$lines, na$idx_clean, na$idx_raw, compare_mode)
  segB <- make_segments(nb$lines, nb$idx_clean, nb$idx_raw, compare_mode)
  
  same <- identical(segA$segs, segB$segs)
  
  # --- robust diff map using LCS on the segment vectors ---
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
  
  # Join in the page and line-number info
  diffs_df <- if (nrow(long_df) > 0) {
    long_df %>%
      dplyr::left_join(na$map_df, by = c("lineA" = "clean")) %>%
      dplyr::rename(pageA = page, lipA = line_in_page, rawA = raw) %>%
      dplyr::left_join(nb$map_df, by = c("lineB" = "clean")) %>%
      dplyr::rename(pageB = page, lipB = line_in_page, rawB = raw)
  } else {
    data.frame(lineA=integer(), lineB=integer(), diff=character(), rawA=integer(), rawB=integer(),
               pageA=integer(), lipA=integer(), pageB=integer(), lipB=integer())
  }
  
  # Write HTML diff if requested (independent of indexing logic)
  diff_path <- NA_character_
  if (!same && !is.null(diff_html_path)) {
    dir.create(dirname(diff_html_path), recursive = TRUE, showWarnings = FALSE)
    
    # Get the HTML fragment from diffobj
    frag <- as.character(diffobj::diffChr(
      segA$segs, segB$segs,
      format = "html", pager = "off",
      tar.banner = "", cur.banner = ""
    ))
    
    # Wrap it in a full, self-contained HTML document
    html <- paste0(
      '<!doctype html><html><head><meta charset="utf-8">',
      '<meta name="viewport" content="width=device-width,initial-scale=1">',
      '<title>RTF Diff: ', htmltools::htmlEscape(basename(file_a)), ' vs ',
      htmltools::htmlEscape(basename(file_b)), '</title>',
      # minimal base styles so content is visible even without diffobj CSS
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
    utils::write.csv(display_df, diff_csv_path, row.names = FALSE, na = "")
    diff_csv_out <- normalizePath(diff_csv_path, winslash = "/")
  }
  
  file_label <- if (basename(file_a) == basename(file_b))
    basename(file_a) else paste(basename(file_a), "vs", basename(file_b))
  type_label <- infer_type(basename(file_a))
  
  summary_like <- data.frame(
    file        = file_label,
    type        = type_label,
    status      = if (same) "Identical" else "Different",
    identical   = same,
    diff_path   = diff_path,
    diff_csv    = diff_csv_out,
    note        = if (same) "Identical" else sprintf("Differences found (%s-level)", compare_mode),
    stringsAsFactors = FALSE
  )
  
  list(summary_like = summary_like, differences = diffs_df)
}

# --------------------- UI ---------------------
ui <- fluidPage(
  shinyjs::useShinyjs(),
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
                 downloadButton("dlDiffsOnly", "Download DIFFS (CSV)"),
                 tags$hr()
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
  
  # dynamic alias for whatever diff dir the user chooses
  current_alias <- reactiveVal("rtf_diffs_current")
  bindDiffDir <- function(dir) {
    alias <- "rtf_diffs_current"
    dir <- sanitize_path(dir)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)  # ensure exists
    try(shiny::removeResourcePath(alias), silent = TRUE)
    addResourcePath(alias, dir)
    current_alias(alias)
  }
  
  # Directory compare
  resDir <- eventReactive(input$runDir, {
    dirA <- sanitize_path(input$dirA)
    dirB <- sanitize_path(input$dirB)
    validate(
      need(dir.exists(dirA), paste("Directory A not found:", dirA)),
      need(dir.exists(dirB), paste("Directory B not found:", dirB))
    )
    prefixes <- if (length(input$prefix)) input$prefix else c("t","l")
    
    diff_dir <- sanitize_path(input$diffdir)
    if (!dir.exists(diff_dir)) dir.create(diff_dir, recursive = TRUE, showWarnings = FALSE)
    bindDiffDir(diff_dir)
    
    A <- .list_by_prefix(dirA, prefixes, isTRUE(input$recursive))
    B <- .list_by_prefix(dirB, prefixes, isTRUE(input$recursive))
    dupA <- names(which(table(A$file) > 1)); dupB <- names(which(table(B$file) > 1))
    A1 <- subset(A, !(file %in% dupA)); B1 <- subset(B, !(file %in% dupB))
    common <- intersect(unique(A1$file), unique(B1$file))
    showNotification(sprintf("Matched (by prefix): A=%d, B=%d, Common=%d",
                             nrow(A), nrow(B), length(common)),
                     type = if (length(common)) "message" else "warning", duration = 5)
    
    compare_mode <- input$mode
    
    result <- data.frame(file=character(), type=character(), status=character(),
                         identical=logical(), diff_path=character(), note=character(),
                         diff_df=I(list()), diff_csv=character(),
                         stringsAsFactors = FALSE)
    
    add_row <- function(..., diff_df = NULL, diff_csv = NA_character_) {
      row <- data.frame(..., stringsAsFactors = FALSE)
      row$diff_df <- I(list(diff_df))
      row$diff_csv <- diff_csv
      result <<- rbind(result, row)
    }
    
    if (!dir.exists(diff_dir)) dir.create(diff_dir, recursive = TRUE, showWarnings = FALSE)
    
    if (length(dupA)) for (f in dupA)
      add_row(file=f, type=infer_type(f), status="Duplicate in A", identical=NA,
              diff_path=NA_character_, note="Multiple files with same name in A")
    if (length(dupB)) for (f in dupB)
      add_row(file=f, type=infer_type(f), status="Duplicate in B", identical=NA,
              diff_path=NA_character_, note="Multiple files with same name in B")
    
    onlyA <- setdiff(unique(A1$file), unique(B1$file))
    if (length(onlyA)) for (f in onlyA)
      add_row(file=f, type=infer_type(f), status="Only in A", identical=NA,
              diff_path=NA_character_, note="Only in A")
    onlyB <- setdiff(unique(B1$file), unique(A1$file))
    if (length(onlyB)) for (f in onlyB)
      add_row(file=f, type=infer_type(f), status="Only in B", identical=NA,
              diff_path=NA_character_, note="Only in B")
    
    for (fname in intersect(common, intersect(unique(A1$file), unique(B1$file)))) {
      pa <- A1$path[A1$file == fname][1]; pb <- B1$path[B1$file == fname][1]
      
      diff_results <- tryCatch({
        compare_two_rtfs(
          file_a = pa, file_b = pb, compare_mode = compare_mode,
          diff_html_path = file.path(diff_dir, paste0(fname, ".diff.html")),
          diff_csv_path  = file.path(diff_dir, paste0(fname, ".diff.csv")),
          per_page_top = as.integer(input$ignoreTop_dir %||% 0),
          per_page_bottom = as.integer(input$ignoreBottom_dir %||% 0),
          drop_meta = isTRUE(input$dropMeta_dir)
        )
      }, error = function(e) {
        list(
          summary_like = data.frame(
            file=fname, type=infer_type(fname), status="Read error", identical=NA,
            diff_path=NA_character_, diff_csv=NA_character_,
            note=paste("Read/Compare Error:", conditionMessage(e)),
            stringsAsFactors = FALSE
          ),
          differences = NULL
        )
      })
      
      add_row(
        file = diff_results$summary_like$file,
        type = diff_results$summary_like$type,
        status = diff_results$summary_like$status,
        identical = diff_results$summary_like$identical,
        diff_path = diff_results$summary_like$diff_path,
        note = diff_results$summary_like$note,
        diff_df = diff_results$differences,
        diff_csv = diff_results$summary_like$diff_csv
      )
    }
    
    if (isTRUE(input$makeIndex) && nrow(result)) {
      idx <- write_diff_index(
        sanitize_path(input$diffdir),
        summary_df = subset(result, status == "Different"),
        title = "RTF Differences Index", filename = "index.html"
      )
      attr(result, "index_path") <- idx
    }
    result
  })
  
  output$tblSummary <- renderDT({
    r <- resDir(); if (is.null(r)) return(NULL)
    df <- r[, c("file","type","status","diff_path","diff_csv","note")]
    alias <- current_alias() %||% "rtf_diffs_current"
    
    df$diff_path <- ifelse(
      is.na(df$diff_path), NA,
      {
        fname <- utils::URLencode(basename(df$diff_path))
        href  <- file.path(alias, fname)
        sprintf("<a href='%s' target='_blank'>open</a>", href)
      }
    )
    
    df$diff_csv <- ifelse(
      is.na(df$diff_csv), NA,
      sprintf("<a href='%s' download>csv</a>",
              file.path(alias, basename(df$diff_csv)))
    )
    datatable(df, escape = FALSE, options = list(pageLength = 25, scrollX = TRUE))
  })
  
  output$tblDiffs <- renderDT({
    r <- resDir(); if (is.null(r)) return(NULL)
    d <- subset(r, status == "Different")[, c("file","type","status","diff_path","diff_csv")]
    alias <- current_alias() %||% "rtf_diffs_current"
    
    d$diff_path <- ifelse(
      is.na(d$diff_path), NA,
      sprintf("<a href='%s' target='_blank'>open</a>",
              file.path(alias, basename(d$diff_path)))
    )
    d$diff_csv <- ifelse(
      is.na(d$diff_csv), NA,
      sprintf("<a href='%s' download>csv</a>",
              file.path(alias, basename(d$diff_csv)))
    )
    datatable(d, escape = FALSE, options = list(pageLength = 25, scrollX = TRUE))
  })
  
  output$indexLink <- renderUI({
    r <- resDir(); if (is.null(r)) return(NULL)
    idx <- attr(r, "index_path")
    if (!is.null(idx) && file.exists(idx)) {
      tags$p(HTML(sprintf("Index: <a href='%s' target='_blank'>%s</a>",
                          file.path(current_alias() %||% "rtf_diffs_current", basename(idx)),
                          basename(idx))))
    }
  })
  
  output$dlSummary <- downloadHandler(
    filename = function() "rtf_compare_summary.csv",
    content  = function(file) {
      r <- resDir(); req(r)
      keep <- setdiff(names(r), c("diff_df"))
      write.csv(r[, keep, drop = FALSE], file, row.names = FALSE)
    }
  )
  output$dlDiffsOnly <- downloadHandler(
    filename = function() "rtf_compare_differences_only.csv",
    content  = function(file) {
      r <- resDir(); req(r)
      d <- subset(r, status == "Different")
      keep <- setdiff(names(d), c("diff_df"))
      write.csv(d[, keep, drop = FALSE], file, row.names = FALSE)
    }
  )
  
  # Pairwise compare ===================================================
  
  observeEvent(input$runPair, {
    fn <- sanitize_path(input$pairDiffHtml)
    if (nzchar(fn)) {
      d <- dirname(fn)
      if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
      bindDiffDir(d)
    }
  })
  
  resPair <- eventReactive(input$runPair, {
    fa <- sanitize_path(input$fileA); fb <- sanitize_path(input$fileB)
    if (!file.exists(fa)) { showNotification(paste("File A not found:", fa), type="error", duration=5); return(NULL) }
    if (!file.exists(fb)) { showNotification(paste("File B not found:", fb), type="error", duration=5); return(NULL) }
    
    out <- compare_two_rtfs(
      fa, fb, compare_mode = input$mode2,
      diff_html_path = if (nzchar(input$pairDiffHtml)) sanitize_path(input$pairDiffHtml) else NULL,
      diff_csv_path  = if (nzchar(input$pairDiffHtml))
        sub("\\.html?$", ".csv", sanitize_path(input$pairDiffHtml)) else NULL,
      per_page_top = as.integer(input$ignoreTop_pair %||% 0),
      per_page_bottom = as.integer(input$ignoreBottom_pair %||% 0),
      drop_meta = isTRUE(input$dropMeta_pair)
    )
    
    # ✅ Count *collapsed* diffs (replace = 1; not del+ins=2)
    disp <- process_diffs_to_wide_format(out$differences)
    if (nrow(disp) > 0) {
      showNotification(sprintf("Pairwise: %d differing lines.", nrow(disp)),
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
    out
  })
  
  pairwise_display_data <- reactive({
    r <- resPair(); req(r)
    d <- r$differences
    process_diffs_to_wide_format(d)
  })
  
  output$tblPairSummary <- renderDT({
    r <- resPair(); if (is.null(r)) return(NULL)
    df <- r$summary_like[, c("file","type","status","diff_path","note")]
    alias <- current_alias() %||% "rtf_diffs_current"
    df$diff_path <- ifelse(
      is.na(df$diff_path), NA,
      {
        fname <- utils::URLencode(basename(df$diff_path))
        href  <- file.path(alias, fname)
        sprintf("<a href='%s' target='_blank'>open</a>", href)
      }
    )
    datatable(df, escape = FALSE, options = list(dom = 't', paging = FALSE, scrollX = TRUE))
  })
  
  output$tblPairDiffs <- renderDT({
    df <- pairwise_display_data()
    df$Position <- sprintf("<span title='%s | %s'>%s</span>", df$PositionA, df$PositionB, df$Position)
    datatable(df[, c("Position","Display A","Display B")],
              escape = FALSE, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  output$pairIndexLink <- renderUI({
    r <- resPair(); if (is.null(r)) return(NULL)
    idx <- attr(r, "index_path")
    if (!is.null(idx) && file.exists(idx)) {
      tags$p(HTML(sprintf("Index: <a href='%s' target='_blank'>%s</a>",
                          file.path(current_alias() %||% "rtf_diffs_current", basename(idx)),
                          basename(idx))))
    }
  })
  
  output$dlPairDiffs <- downloadHandler(
    filename = function() {
      r <- resPair()
      req(r)
      base_name <- gsub(" vs ", "_vs_", r$summary_like$file)
      base_name <- gsub("[^a-zA-Z0-9_.-]", "_", base_name)
      paste0(base_name, "_differences_", Sys.Date(), ".csv")
    },
    content = function(file) {
      display_df <- pairwise_display_data()
      req(display_df)
      write.csv(display_df, file, row.names = FALSE, na = "")
    }
  )
}

shinyApp(ui, server)
