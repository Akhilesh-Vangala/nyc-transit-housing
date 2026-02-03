if (file.exists("config.R")) setwd(getwd()) else stop("Run from project root.")

source("config.R")

required <- c("sf", "dplyr", "readr", "jsonlite", "units", "here", "ggplot2")
if (length(missing <- required[!sapply(required, requireNamespace, quietly = TRUE)]))
  stop("Missing: ", paste(missing, collapse = ", "), ". Run: Rscript install_deps.R")

run_download_stage <- function() {
  source(file.path("R", "01_download.R"), local = TRUE)
  run_download()
}

run_validate_stage <- function() {
  source(file.path("R", "02_validate.R"), local = TRUE)
  run_validate()
  f <- list.files(OUT_VALIDATION, pattern = "validation_.*\\.json", full.names = TRUE)
  if (length(f) == 0) stop("No validation report in ", OUT_VALIDATION)
  rep <- jsonlite::read_json(sort(f, decreasing = TRUE)[1])
  if (isTRUE(rep$critical_fail)) stop("Validation failed. Check ", OUT_VALIDATION)
}

run_assign_coords_stage <- function() {
  source(file.path("R", "03b_assign_building_coords.R"), local = TRUE)
  run_assign_building_coords()
  pct_path <- file.path(OUT_VALIDATION, "coord_source_validation.json")
  if (file.exists(pct_path)) {
    cv <- jsonlite::read_json(pct_path)
    pct_fb <- if (is.null(cv$pct_coords_borough_fallback)) cv$pct_fallback_coords else cv$pct_coords_borough_fallback
    if (!is.null(pct_fb) && is.numeric(pct_fb) && pct_fb > (VALIDATION_MAX_PCT_FALLBACK * 100))
      warning("pct_borough_fallback = ", pct_fb, "% exceeds ", VALIDATION_MAX_PCT_FALLBACK * 100, "%. Results may be biased; use credible subset (sensitivity_by_coord_source.csv).")
  }
}

run_eda_stage <- function() {
  source(file.path("R", "03a_eda.R"), local = TRUE)
  run_eda()
}

run_prepare_stage <- function() {
  source(file.path("R", "03_prepare_spatial.R"), local = TRUE)
  run_prepare_spatial()
}

run_buffer_stage <- function() {
  source(file.path("R", "04_buffer_analysis.R"), local = TRUE)
  run_buffer_analysis()
}

run_aggregate_stage <- function() {
  source(file.path("R", "05_aggregate.R"), local = TRUE)
  run_aggregate()
}

run_visualize_stage <- function() {
  source(file.path("R", "06_visualize.R"), local = TRUE)
  run_visualize()
}

dry_run <- function() {
  stopifnot(dir.exists(RAW), dir.exists(PROCESSED), length(BUFFER_RADIUS_M) >= 1, CRS_WGS84 == 4326)
}

main <- function(skip_download = FALSE, dry_run_only = FALSE) {
  if (dry_run_only) { dry_run(); return(invisible(NULL)) }
  if (!skip_download) run_download_stage()
  run_validate_stage()
  run_assign_coords_stage()
  run_eda_stage()
  run_prepare_stage()
  run_buffer_stage()
  run_aggregate_stage()
  run_visualize_stage()
  tryCatch({
    source(file.path("R", "07_data_requests.R"), local = TRUE)
    run_data_requests()
  }, error = function(e) NULL)
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  main(
    skip_download = any(args == "--skip-download"),
    dry_run_only = any(args == "--dry-run")
  )
}
