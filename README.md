# biostat-tfl-compare

[![GitHub repo](https://img.shields.io/badge/GitHub-biostat--tfl--compare-black)](https://github.com/Veronica0206/biostat-tfl-compare)
[![R Shiny](https://img.shields.io/badge/R-Shiny-blue)](https://shiny.posit.co/)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)

*A Shiny application for quality oversight of TFL outputs, enabling RTF file and directory comparisons with detailed HTML and CSV diff reports.*

## Portfolio positioning

`biostat-tfl-compare` is a regulated-workflow QC tool for reviewing clinical reporting outputs in RTF format. It supports statisticians, statistical programmers, and evidence-generation teams who need to compare TFLs across development cycles, production runs, vendor deliveries, or internal validation outputs.

The app is especially relevant to clinical trial, RWE, and HEOR workflows where reviewers need traceable differences rather than informal side-by-side inspection. It reduces manual review burden by producing navigable HTML and CSV artifacts while preserving human review as the final decision point.

## Executive relevance

This project is a public example of how Statistics and programming review workflows can be standardized for repeated TFL comparison tasks across studies or therapeutic areas. It supports auditability and traceability by turning RTF file/folder comparisons into documented HTML and CSV artifacts with page-aware inspection. The tool assists human reviewers; it does not replace statistical programming QC, clinical interpretation, regulatory judgment, or final sign-off.

## For hiring reviewers

This project demonstrates:

- practical software development for clinical trial and evidence-reporting workflows
- QC automation for table, figure, and listing output review
- R Shiny implementation with user-facing controls, progress indicators, downloads, and dynamic tables
- regulated-document comparison logic for RTF outputs
- vendor oversight and audit-readiness through traceable diff artifacts
- page-aware line inspection and paragraph/line/token comparison modes

## Overview / purpose

Although often used in the context of clinical trial TFLs, the app is equally applicable to RWE and HEOR deliverables, or any workflow where outputs are delivered in RTF format.

By providing side-by-side and bulk comparison of RTF files, this app helps ensure consistency, accuracy, and transparency in reporting.

### Example use cases

- **Check changes between deliverables:** compare development vs. production TFLs.
- **QC verification:** confirm that programmed updates are reflected in output files.
- **Vendor oversight:** compare CRO deliverables against internal runs.
- **Audit readiness:** generate traceable HTML/CSV diff reports for regulatory or quality review.
- **Cross-functional communication:** give statisticians, medical writers, clinicians, and reviewers readable diff artifacts instead of raw RTF comparisons.

## Features

- **File-level comparison:** side-by-side review of two RTF files with detailed HTML and CSV diff reports.
- **Folder-level comparison:** bulk comparison of two directories, matching files by name and highlighting differences in content, structure, or formatting.
- **Page-aware inspection:** detects RTF page breaks, form feeds, and page footer patterns, and reports page/line positions plus global line positions for differences.
- **Granularity controls:** supports paragraph, line, and token comparison modes.
- **Header/footer handling:** includes controls for ignoring per-page header/footer lines and dropping footer/meta lines.
- **Diff artifacts:** generates interactive HTML reports, CSV summaries, and an optional `index.html` linking all differences.
- **Interactive UI:** Shiny app with progress indicators, download buttons, resource links, and dynamic tables for navigation.

---

## Installation  

### Prerequisites  
- R (≥ 4.5) 
- RStudio (optional but recommended)  

### Required packages  
- `shiny`
- `dplyr`
- `shinyjs`
- `DT`  
- `striprtf`  
- `diffobj`
- `tools`
- `shinybusy`  
- `shinyWidgets`
- `promises`
- `future`
- `future.apply`
- `htmltools`


### Setup  
```r
# clone this repository
git clone https://github.com/Veronica0206/biostat-tfl-compare.git
cd biostat-tfl-compare

# open R or RStudio
# install required packages if not already available
install.packages(c("shiny","dplyr",'shinyjs',"DT","striprtf","diffobj","tools","shinybusy","shinyWidgets","promises","future","future.apply","htmltools"))

# run the app (the Shiny app lives in the tfl_compare/ subfolder)
shiny::runApp("tfl_compare", launch.browser = TRUE)
