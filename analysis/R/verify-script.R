# =============================================================================
# Run ONE pipeline script and report whether it reproduces the existing outputs.
#
#   source("analysis/R/00-setup.R")
#   source("analysis/R/verify-script.R")
#   verify("analysis/R/01-dataprep/Survey Data Preparation Part 1.R")
#
# It snapshots every file under data/derived/ and analysis/outputs/ before the
# run, then reports what the script wrote and whether the content changed.
# Nothing is deleted. A full pre-repair backup lives outside the project at
#   ../Chapter3-outputs-backup-20260905-0812/
# =============================================================================

.watch_dirs <- c("data/derived", "analysis/outputs")

.snapshot <- function() {
  fs <- list.files(.watch_dirs, recursive = TRUE, full.names = TRUE, all.files = FALSE)
  fs <- fs[file.info(fs)$isdir %in% FALSE]
  setNames(unname(tools::md5sum(fs)), fs)
}

verify <- function(script) {
  stopifnot(file.exists(script))
  before <- .snapshot()

  t0 <- Sys.time()
  message("\n=== running: ", basename(script), " ===")
  ok <- TRUE
  err <- NULL
  tryCatch(
    source(script, encoding = "UTF-8", local = new.env()),
    error = function(e) { ok <<- FALSE; err <<- conditionMessage(e) }
  )
  el <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)

  after <- .snapshot()

  created   <- setdiff(names(after), names(before))
  common    <- intersect(names(before), names(after))
  changed   <- common[before[common] != after[common]]
  unchanged <- common[before[common] == after[common]]
  removed   <- setdiff(names(before), names(after))

  cat("\n---------------------------------------------------------------\n")
  cat("script    :", basename(script), "\n")
  cat("status    :", if (ok) "completed" else "FAILED", "\n")
  if (!ok) cat("error     :", err, "\n")
  cat("elapsed   :", el, "sec\n")
  cat("created   :", length(created), "file(s)\n")
  cat("changed   :", length(changed), "file(s)   <- differ from the version the manuscript used\n")
  cat("identical :", length(unchanged), "file(s) untouched or byte-identical\n")
  if (length(removed)) cat("removed   :", length(removed), "file(s)\n")
  cat("---------------------------------------------------------------\n")

  if (length(created)) { cat("\nCREATED:\n");  cat(paste0("  + ", created, collapse = "\n"), "\n") }
  if (length(changed)) { cat("\nCHANGED:\n");  cat(paste0("  ~ ", changed, collapse = "\n"), "\n") }
  if (length(removed)) { cat("\nREMOVED:\n");  cat(paste0("  - ", removed, collapse = "\n"), "\n") }

  if (!length(changed) && !length(created)) {
    cat("\nRESULT: reproduces exactly - no output changed.\n")
  } else if (!length(changed)) {
    cat("\nRESULT: only new files written; nothing the manuscript uses changed.\n")
  } else {
    cat("\nRESULT: ", length(changed), " output(s) differ. Inspect before accepting.\n", sep = "")
  }

  invisible(list(script = script, ok = ok, error = err, elapsed = el,
                 created = created, changed = changed, removed = removed))
}
