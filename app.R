# app.R — RTF Comparator (base Shiny progress, working links, pairwise matches directory view)
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
  s <- gsub("^\\s*['\"]|['\"]\\s*$", "", s)  # strip surrounding quotes
  s <- gsub("\\\\", "/", s)                  # backslashes -> forward slashes
  s
}

normalize_rtf_text <- function(x,
                               header_patterns = c(
                                 "(?i)^\\s*takeda\\s*$",
                                 "(?i)^\\s*takeda\\s+page\\s+\\d+\\s*(of\\s+\\d+)?\\s*$",
                                 "(?i)^\\s*page\\s+\\d+\\s*(of\\s+\\d+)?\\s*$"
                               ),
                               drop_short_repeating = TRUE,
                               repeat_threshold = 3,
                               max_len_for_repeat = 60,
                               dehyphenate = TRUE
){
  x <- gsub("\r", "\n", x, fixed = TRUE)
  x <- gsub("\u00A0", " ", x, fixed = TRUE)
  x <- gsub("[ \t]+", " ", x)
  x <- gsub("\n[ \t]+", "\n", x)
  if (dehyphenate) x <- gsub("-\\n", "", x)
  x <- gsub("\n{3,}", "\n\n", x)
  lines <- unlist(strsplit(trimws(x), "\n", fixed = TRUE))
  if (length(header_patterns)) {
    pat <- paste(header_patterns, collapse = "|")
    lines <- lines[ !grepl(pat, lines, perl = TRUE) ]
  }
  if (drop_short_repeating && length(lines)) {
    tab <- table(lines)
    rep_lines <- names(tab[tab >= repeat_threshold])
    rep_lines <- rep_lines[nchar(rep_lines) <= max_len_for_repeat]
    if (length(rep_lines)) lines <- lines[ !(lines %in% rep_lines) ]
  }
  paste(lines, collapse = "\n")
}

chunk_text <- function(x, mode = c("paragraph","line","token")) {
  mode <- match.arg(mode)
  if (mode == "line")       strsplit(x, "\n", fixed = TRUE)[[1]]
  else if (mode == "paragraph") unlist(strsplit(x, "\n\\s*\n", perl = TRUE))
  else                       unlist(strsplit(x, "\\s+", perl = TRUE))
}

infer_type <- function(fn)
  if (grepl("^[Tt]", fn)) "Table" else if (grepl("^[Ll]", fn)) "Listing" else "Other"

clean_raw_diff <- function(raw_lines) {
  if (!length(raw_lines)) return(raw_lines)
  keep <- !grepl('^\\s*(@@|< Aseg|> Bseg|No visible differences between objects\\.)', raw_lines)
  out <- raw_lines[keep]; out[nzchar(out)]
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

.list_by_prefix <- function(dir, prefixes, recursive) {
  all_files <- list.files(dir, recursive = recursive, full.names = TRUE)
  if (!length(all_files)) return(data.frame(path=character(), file=character(), ext=character()))
  base <- basename(all_files)
  keep <- Reduce(`|`, lapply(prefixes, function(p) grepl(paste0("^", p), base, ignore.case = TRUE)))
  df <- data.frame(path = all_files[keep], file = base[keep], stringsAsFactors = FALSE)
  df$ext <- tolower(file_ext(df$file)); df
}

# ----- core directory comparator -----
compare_rtf_dirs <- function(dir_a, dir_b, file_prefix = c("t","l"), recursive = TRUE,
                             diff_dir = "rtf_diffs", compare_mode = c("paragraph","line","token")) {
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
    ta <- tryCatch(striprtf::read_rtf(pa), error=function(e) e)
    tb <- tryCatch(striprtf::read_rtf(pb), error=function(e) e)
    if (inherits(ta,"error") || inherits(tb,"error")) {
      note <- if (inherits(ta,"error")) paste("Read error A:", conditionMessage(ta))
      else paste("Read error B:", conditionMessage(tb))
      add_row(file=fname, type=infer_type(fname), status="Read error", identical=NA,
              diff_path=NA_character_, note=note); next
    }
    ta <- normalize_rtf_text(ta); tb <- normalize_rtf_text(tb)
    if (identical(ta, tb)) {
      add_row(file=fname, type=infer_type(fname), status="Identical", identical=TRUE,
              diff_path=NA_character_, note="Identical")
    } else {
      Aseg <- chunk_text(ta, compare_mode); Bseg <- chunk_text(tb, compare_mode)
      raw_diff <- clean_raw_diff(capture.output(diffobj::diffChr(Aseg, Bseg, format="raw", pager="off")))
      html <- paste(capture.output(show(diffobj::diffChr(Aseg, Bseg, format="html", pager="off"))), collapse="\n")
      diff_file <- file.path(diff_dir, paste0(fname, ".diff.html")); writeLines(html, diff_file)
      row <- data.frame(file=fname, type=infer_type(fname), status="Different", identical=FALSE,
                        diff_path=diff_file, note=sprintf("Differences found (%s-level)", compare_mode),
                        stringsAsFactors = FALSE)
      row$diff_lines <- I(list(raw_diff %||% character(0))); result <- rbind(result, row)
    }
  }
  if (nrow(result)) result[order(result$file), , drop = FALSE] else result
}

# ----- Pairwise (returns directory-style summary row) -----
compare_two_rtfs <- function(file_a, file_b,
                             compare_mode = c("paragraph","line","token"),
                             diff_html_path = NULL) {
  compare_mode <- match.arg(compare_mode)
  
  # --- read + normalize ---
  ta <- striprtf::read_rtf(file_a)
  tb <- striprtf::read_rtf(file_b)
  ta <- normalize_rtf_text(ta)
  tb <- normalize_rtf_text(tb)
  
  same <- identical(ta, tb)
  Aseg <- chunk_text(ta, compare_mode)
  Bseg <- chunk_text(tb, compare_mode)
  
  # --- raw diff ---
  raw_diff <- if (same) character(0) else
    capture.output(diffobj::diffChr(Aseg, Bseg, format = "raw", pager = "off"))
  
  diff_path <- NA_character_
  if (!same && !is.null(diff_html_path)) {
    dir.create(dirname(diff_html_path), recursive = TRUE, showWarnings = FALSE)
    html <- paste(
      capture.output(show(diffobj::diffChr(Aseg, Bseg, format = "html", pager = "off"))),
      collapse = "\n"
    )
    writeLines(html, diff_html_path)
    diff_path <- normalizePath(diff_html_path, winslash = "/")
  }
  
  # --- parse raw diff with true line numbers ---
  parse_diff <- function(raw) {
    out <- data.frame(lineA = integer(), lineB = integer(), diff = character(),
                      stringsAsFactors = FALSE)
    curA <- curB <- NA
    
    for (ln in raw) {
      # skip "Aseg"/"Bseg" marker lines
      if (grepl("^< Aseg", ln) || grepl("^> Bseg", ln)) next
      
      # hunk header: @@ startA,countA / startB,countB @@
      if (grepl("^@@", ln)) {
        parts <- unlist(regmatches(ln, gregexpr("[0-9]+", ln)))
        if (length(parts) >= 4) {
          curA <- as.integer(parts[1])
          curB <- as.integer(parts[3])
        }
        next
      }
      
      if (startsWith(ln, "<")) {   # only in A
        out <- rbind(out,
                     data.frame(lineA = curA, lineB = NA, diff = ln,
                                stringsAsFactors = FALSE))
        curA <- curA + 1
      } else if (startsWith(ln, ">")) {  # only in B
        out <- rbind(out,
                     data.frame(lineA = NA, lineB = curB, diff = ln,
                                stringsAsFactors = FALSE))
        curB <- curB + 1
      } else {  # common line
        out <- rbind(out,
                     data.frame(lineA = curA, lineB = curB, diff = ln,
                                stringsAsFactors = FALSE))
        curA <- curA + 1
        curB <- curB + 1
      }
    }
    out
  }
  
  diffs_df <- if (length(raw_diff)) parse_diff(raw_diff) else
    data.frame(lineA=integer(), lineB=integer(), diff=character(),
               stringsAsFactors = FALSE)
  
  # --- directory-style summary row ---
  file_label <- if (basename(file_a) == basename(file_b))
    basename(file_a) else paste(basename(file_a), "vs", basename(file_b))
  
  type_label <- infer_type(basename(file_a))
  
  summary_like <- data.frame(
    file       = file_label,
    type       = type_label,
    status     = if (same) "Identical" else "Different",
    identical  = same,
    diff_path  = diff_path,
    note       = if (same) "Identical"
    else sprintf("Differences found (%s-level)", compare_mode),
    stringsAsFactors = FALSE
  )
  summary_like$diff_lines <- I(list(raw_diff))
  
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
                                    selected = "t"),
                 selectInput("mode", "Compare granularity",
                             c("paragraph","line","token"), selected="paragraph"),
                 checkboxInput("recursive", "Search subfolders (recursive)", TRUE),
                 textInput("diffdir", "Diff output folder", "rtf_diffs"),
                 checkboxInput("makeIndex", "Create index.html with links", TRUE),
                 actionButton("runDir", "Run comparison", class = "btn-primary"),
                 tags$hr(),
                 strong("Debug:"), verbatimTextOutput("debugClicks"),
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
                 actionButton("runPair", "Run pairwise comparison", class = "btn-primary"),
                 tags$hr(),
                 downloadButton("dlPairDiffs", "Download pairwise DIFF (CSV)")
               ),
               mainPanel(
                 h4("Pairwise Summary"), DTOutput("tblPairSummary"),
                 h4("Pairwise Differences"), DTOutput("tblPairDiffs"),
                 uiOutput("pairIndexLink")
               )
             )
    )
  )
)

# --------------------- Server ---------------------
server <- function(input, output, session) {
  
  # Serve the diff output folder so links work (e.g., /rtf_diffs/xxx.diff.html)
  addResourcePath("rtf_diffs", sanitize_path("rtf_diffs"))
  
  # Debug click counter
  output$debugClicks <- renderText({ paste("Run comparison clicks:", input$runDir) })
  observeEvent(input$runDir, {
    cat("\n[DEBUG] runDir clicked at", format(Sys.time(), "%H:%M:%S"),
        " value =", input$runDir, "\n")
  }, ignoreInit = TRUE, priority = 100)
  
  # ---- Directory compare using base Shiny progress modal ----
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
    msg <- sprintf("Matched (by prefix): A=%d, B=%d, Common=%d", nrow(A), nrow(B), length(common))
    showNotification(msg, type = if (length(common)) "message" else "warning", duration = 6)
    output$debugClicks <- renderText({ paste("Run comparison clicks:", input$runDir, "|", msg) })
    
    if (!length(common)) {
      return(compare_rtf_dirs(
        dir_a = dirA, dir_b = dirB, file_prefix = prefixes,
        recursive = isTRUE(input$recursive),
        diff_dir = sanitize_path(input$diffdir),
        compare_mode = input$mode
      ))
    }
    
    withProgress(message = "Comparing directories…", value = 0, {
      n <- length(common); step <- 1 / n
      diff_dir <- sanitize_path(input$diffdir)
      if (!dir.exists(diff_dir)) dir.create(diff_dir, recursive = TRUE, showWarnings = FALSE)
      
      result <- data.frame(file=character(), type=character(), status=character(),
                           identical=logical(), diff_path=character(), note=character(),
                           stringsAsFactors = FALSE)
      result$diff_lines <- I(list())
      add_row <- function(...) {
        row <- data.frame(..., stringsAsFactors = FALSE)
        row$diff_lines <- I(list(character(0)))
        result <<- rbind(result, row)
      }
      
      if (length(dupA)) for (f in dupA) add_row(file=f, type=infer_type(f), status="Duplicate in A", identical=NA, diff_path=NA_character_, note="Multiple files with same name in A")
      if (length(dupB)) for (f in dupB) add_row(file=f, type=infer_type(f), status="Duplicate in B", identical=NA, diff_path=NA_character_, note="Multiple files with same name in B")
      onlyA <- setdiff(unique(A1$file), unique(B1$file)); if (length(onlyA)) for (f in onlyA) add_row(file=f, type=infer_type(f), status="Only in A", identical=NA, diff_path=NA_character_, note="Only in A")
      onlyB <- setdiff(unique(B1$file), unique(A1$file)); if (length(onlyB)) for (f in onlyB) add_row(file=f, type=infer_type(f), status="Only in B", identical=NA, diff_path=NA_character_, note="Only in B")
      
      for (i in seq_along(common)) {
        fname <- common[i]
        incProgress(step, detail = sprintf("Processing %s (%d/%d)", fname, i, n))
        
        pa <- A1$path[A1$file == fname][1]
        pb <- B1$path[B1$file == fname][1]
        if (tolower(tools::file_ext(pa)) != "rtf" || tolower(tools::file_ext(pb)) != "rtf") {
          add_row(file=fname, type=infer_type(fname), status="Unsupported", identical=NA, diff_path=NA_character_, note="Non-RTF extension")
          next
        }
        
        ta <- tryCatch(striprtf::read_rtf(pa), error=function(e) structure(list(.err=conditionMessage(e)), class="rtf_err"))
        tb <- tryCatch(striprtf::read_rtf(pb), error=function(e) structure(list(.err=conditionMessage(e)), class="rtf_err"))
        if (inherits(ta,"rtf_err") || inherits(tb,"rtf_err")) {
          note <- if (inherits(ta,"rtf_err")) paste("Read error A:", ta$.err) else paste("Read error B:", tb$.err)
          add_row(file=fname, type=infer_type(fname), status="Read error", identical=NA, diff_path=NA_character_, note=note)
          next
        }
        
        ta <- normalize_rtf_text(ta); tb <- normalize_rtf_text(tb)
        if (identical(ta, tb)) {
          add_row(file=fname, type=infer_type(fname), status="Identical", identical=TRUE, diff_path=NA_character_, note="Identical")
        } else {
          Aseg <- chunk_text(ta, input$mode); Bseg <- chunk_text(tb, input$mode)
          raw_diff <- capture.output(diffobj::diffChr(Aseg, Bseg, format="raw", pager="off"))
          html <- paste(capture.output(show(diffobj::diffChr(Aseg, Bseg, format="html", pager="off"))), collapse="\n")
          diff_file <- file.path(diff_dir, paste0(fname, ".diff.html")); writeLines(html, diff_file)
          row <- data.frame(file=fname, type=infer_type(fname), status="Different", identical=FALSE,
                            diff_path=diff_file, note=sprintf("Differences found (%s-level)", input$mode),
                            stringsAsFactors = FALSE)
          row$diff_lines <- I(list(raw_diff)); result <- rbind(result, row)
        }
      }
      
      if (isTRUE(input$makeIndex) && nrow(result)) {
        idx <- write_diff_index(diff_dir, summary_df = subset(result, status == "Different"),
                                title = "RTF Differences Index", filename = "index.html")
        attr(result, "index_path") <- idx
      }
      result[order(result$file), , drop = FALSE]
    })
  })
  
  # ---- Directory tables (map links via resource path) ----
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
    content = function(file) {
      r <- resDir(); req(r)
      write.csv(r[, !(names(r) %in% "diff_lines"), drop = FALSE], file, row.names = FALSE)
    }
  )
  
  output$dlDiffsOnly <- downloadHandler(
    filename = function() "rtf_compare_differences_only.csv",
    content = function(file) {
      r <- resDir(); req(r)
      d <- subset(r, status == "Different")
      write.csv(d[, !(names(d) %in% "diff_lines"), drop = FALSE], file, row.names = FALSE)
    }
  )
  
  # ----- Pairwise (matches directory-style summary) -----
  resPair <- eventReactive(input$runPair, {
    fa <- sanitize_path(input$fileA); fb <- sanitize_path(input$fileB)
    if (!file.exists(fa)) { showNotification(paste("File A not found:", fa), type="error", duration=6); return(NULL) }
    if (!file.exists(fb)) { showNotification(paste("File B not found:", fb), type="error", duration=6); return(NULL) }
    out <- compare_two_rtfs(
      fa, fb, compare_mode = input$mode2,
      diff_html_path = if (nzchar(input$pairDiffHtml)) sanitize_path(input$pairDiffHtml) else NULL
    )
    if (nrow(out$differences)) showNotification(sprintf("Pairwise: %d diff lines.", nrow(out$differences)), type="warning", duration=6)
    else showNotification("Pairwise: files are identical.", type="message", duration=6)
    
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
    datatable(r$differences, options = list(pageLength = 25, scrollX = TRUE))
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
