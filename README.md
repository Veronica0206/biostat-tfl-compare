# biostat-tfl-compare  
*A Shiny application for quality oversight of TFL outputs, enabling RTF file and directory comparisons with detailed HTML and CSV diff reports.*  

## Overview / Purpose  

`biostat-tfl-compare` is designed to support statisticians and programmers in efficiently reviewing RTF outputs.  
Although often used in the context of **clinical trial TFLs**, the tool is equally applicable to **RWE** and **HEOR** deliverables, or any workflow where outputs are delivered in RTF format.  

By providing side-by-side and bulk comparison of RTF files, this app helps ensure consistency, accuracy, and transparency in reporting.  

### Example use cases:  
- **Check changes between two deliverables** (e.g., dev vs. prod TFLs).  
- **QC verification** — confirm that updates made during a programming cycle are correctly reflected in the outputs.  
- **Vendor oversight** — compare CRO deliverables against internal runs to ensure alignment.  
- **Audit readiness** — generate traceable HTML diff reports to document differences for regulatory or quality review.  
- **Cross-functional communication** — provide reviewers (statisticians, medical writers, clinicians) with easy-to-read diff outputs instead of raw RTFs.

## Features  

- **File-level comparison** — side-by-side review of two RTF files with detailed HTML diff reports.  
- **Folder-level comparison** — bulk comparison of two directories, matching files by name and highlighting differences in content, structure, or formatting.  
- **Diff artifacts** — generates interactive HTML reports, CSV summaries, and an optional `index.html` linking all differences.  
- **Interactive UI** — Shiny app with progress indicators, download buttons, and dynamic tables for easy navigation.  

## Installation  

### Prerequisites  
- R (≥ 4.2)  
- RStudio (optional but recommended)  

### Required packages  
- `shiny`  
- `DT`  
- `striprtf`  
- `diffobj`  
- `shinybusy`  
- `shinyWidgets`  

### Setup  
```r
# clone this repository
git clone https://github.com/<your-username>/biostat-tfl-compare.git
cd biostat-tfl-compare

# open R or RStudio
# install required packages if not already available
install.packages(c("shiny","DT","striprtf","diffobj","shinybusy","shinyWidgets"))

# run the app
shiny::runApp(launch.browser = TRUE)
