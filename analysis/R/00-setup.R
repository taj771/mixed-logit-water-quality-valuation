# =============================================================================
# Chapter III - shared session setup
#
# Source this ONCE at the start of a session, then any pipeline script can be
# run on its own:
#
#     source("analysis/R/00-setup.R")
#     source("analysis/R/01-dataprep/Survey Data Preparation Part 1.R")
#
# It does two things the individual scripts assume but never check:
#   1. confirms the working directory is the project root
#   2. loads every package the pipeline uses
# =============================================================================

## ---- 1. working directory ---------------------------------------------------
if (!file.exists("Chapter3-WQ-Manuscript.Rproj")) {
  stop(
    "Working directory is not the project root.\n",
    "  Currently: ", getwd(), "\n",
    "  Fix: open Chapter3-WQ-Manuscript.Rproj, or setwd() to the folder\n",
    "       containing it. All paths in the pipeline are relative to it.",
    call. = FALSE
  )
}

## ---- 2. packages ------------------------------------------------------------
.pkgs <- c(
  # core
  "tidyverse", "dplyr", "ggplot2", "tidyr", "stringr", "readr", "purrr",
  # io
  "readxl", "haven",
  # choice modelling
  "apollo", "car",
  # survey weighting / cleaning
  "anesrake", "janitor", "naniar", "cansim",
  # spatial
  "sf", "spdep", "tmap", "tidygeocoder",
  # plotting / tables
  "patchwork", "RColorBrewer", "viridis", "reshape2", "xtable"
)

.missing <- .pkgs[!vapply(.pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(.missing)) {
  stop("Missing packages: ", paste(.missing, collapse = ", "),
       "\n  Install with: install.packages(c(",
       paste0('"', .missing, '"', collapse = ", "), "))",
       call. = FALSE)
}

suppressPackageStartupMessages(
  invisible(lapply(.pkgs, library, character.only = TRUE))
)

rm(.pkgs, .missing)

## ---- 3. output directories --------------------------------------------------
# No script in the pipeline creates its own output directory, and Apollo's own
# dir.create() is not recursive - on a fresh clone it warns and carries on, then
# the save step has nowhere to write. Creating them once here is idempotent.
for (.d in c("data/derived", "data/derived/test",
             "analysis/outputs/models",
             "analysis/outputs/figures", "analysis/outputs/figures/diagnostics",
             "analysis/outputs/tables")) {
  dir.create(.d, recursive = TRUE, showWarnings = FALSE)
}
rm(.d)

message("Setup OK - working directory: ", getwd())
