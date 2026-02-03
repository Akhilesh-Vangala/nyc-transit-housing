source("config.R")

library(readr)
library(jsonlite)
library(sf)

validation_report <- list(
  run_id = RUN_ID,
  schema_version = SCHEMA_VERSION,
  timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
  critical_fail = FALSE,
  subway = list(),
  hpd_buildings = list(),
  boroughs_sf = list()
)

add_check <- function(node, check_id, passed, severity = "warning", detail = NULL) {
  node$checks[[check_id]] <- list(
    passed = passed,
    severity = severity,
    detail = detail
  )
  if (severity == "critical" && !passed) node$critical_fail <- TRUE
  node
}

validate_subway <- function() {
  path <- if (exists("USE_SUBWAY_ENTRANCES") && isTRUE(USE_SUBWAY_ENTRANCES) && exists("FILE_SUBWAY_ENTRANCES"))
    file.path(RAW, FILE_SUBWAY_ENTRANCES) else file.path(RAW, FILE_SUBWAY)
  out <- list(n_rows = 0L, n_cols = 0L, columns = character(), checks = list(), critical_fail = FALSE)

  if (!file.exists(path)) {
    out$checks$file_exists <- list(passed = FALSE, severity = "critical", detail = "File not found")
    out$critical_fail <- TRUE
    validation_report$subway <<- out
    return(invisible(out))
  }

  df <- read_csv(path, show_col_types = FALSE)
  names(df) <- normalize_names(names(df))
  out$n_rows <- nrow(df)
  out$n_cols <- ncol(df)
  out$columns <- names(df)

  out$checks$file_exists <- list(passed = TRUE, severity = "critical", detail = NULL)
  out$checks$has_rows <- list(passed = nrow(df) > 0, severity = "critical", detail = if (nrow(df) == 0) "No rows" else NULL)
  out$checks$min_rows <- list(
    passed = nrow(df) >= VALIDATION_MIN_SUBWAY_ROWS,
    severity = "warning",
    detail = if (nrow(df) < VALIDATION_MIN_SUBWAY_ROWS) paste0("n=", nrow(df), " < ", VALIDATION_MIN_SUBWAY_ROWS) else NULL
  )

  has_lat <- any(grepl("lat|latitude", names(df)))
  has_lon <- any(grepl("lon|longitude|x$", names(df)))
  out$checks$has_coords <- list(passed = has_lat && has_lon, severity = "critical",
    detail = if (!has_lat || !has_lon) "Missing lat/lon columns" else NULL)
  if (!has_lat || !has_lon) out$critical_fail <- TRUE

  lat_col <- names(df)[grepl("lat|latitude", names(df))][1]
  lon_col <- names(df)[grepl("lon|longitude", names(df))][1]
  if (length(lat_col) && length(lon_col)) {
    lat <- as.numeric(df[[lat_col]])
    lon <- as.numeric(df[[lon_col]])
    n_valid <- sum(!is.na(lat) & !is.na(lon))
    n_in_bbox <- sum(!is.na(lat) & !is.na(lon) &
      lon >= BBOX_NYC_WGS84$lon_min & lon <= BBOX_NYC_WGS84$lon_max &
      lat >= BBOX_NYC_WGS84$lat_min & lat <= BBOX_NYC_WGS84$lat_max)
    out$n_valid_coords <- n_valid
    out$n_in_nyc_bbox <- n_in_bbox
    bbox_area_deg2 <- (BBOX_NYC_WGS84$lon_max - BBOX_NYC_WGS84$lon_min) * (BBOX_NYC_WGS84$lat_max - BBOX_NYC_WGS84$lat_min)
    out$bbox_nyc_deg2 <- bbox_area_deg2
    out$checks$coords_in_nyc <- list(
      passed = n_in_bbox >= n_valid * VALIDATION_BBOX_TOLERANCE,
      severity = "warning",
      detail = paste0(n_valid - n_in_bbox, " outside bbox")
    )
    out$checks$coord_numeric <- list(
      passed = is.numeric(lat) && is.numeric(lon),
      severity = "warning",
      detail = NULL
    )
    coords_rounded <- round(lat, VALIDATION_COORD_PRECISION_DECIMALS)
    coords_rounded_lon <- round(lon, VALIDATION_COORD_PRECISION_DECIMALS)
    key <- paste(coords_rounded, coords_rounded_lon, sep = "_")
    n_dup <- sum(duplicated(key[!is.na(key)]))
    out$n_duplicate_coords <- n_dup
    out$checks$no_duplicate_coords <- list(
      passed = n_dup == 0,
      severity = "warning",
      detail = if (n_dup > 0) paste0(n_dup, " duplicate coord pairs") else NULL
    )
  }

  validation_report$subway <<- out
  invisible(out)
}

validate_hpd_buildings <- function() {
  path <- file.path(RAW, FILE_HPD_BUILDINGS)
  out <- list(n_rows = 0L, n_cols = 0L, columns = character(), checks = list(), critical_fail = FALSE)

  if (!file.exists(path)) {
    out$checks$file_exists <- list(passed = FALSE, severity = "critical", detail = "File not found")
    out$critical_fail <- TRUE
    validation_report$hpd_buildings <<- out
    return(invisible(out))
  }

  df <- read_csv(path, show_col_types = FALSE)
  names(df) <- normalize_names(names(df))
  out$n_rows <- nrow(df)
  out$n_cols <- ncol(df)
  out$columns <- names(df)

  out$checks$file_exists <- list(passed = TRUE, severity = "critical", detail = NULL)
  out$checks$has_rows <- list(passed = nrow(df) > 0, severity = "critical", detail = if (nrow(df) == 0) "No rows" else NULL)
  out$checks$min_rows <- list(
    passed = nrow(df) >= VALIDATION_MIN_HPD_ROWS,
    severity = "warning",
    detail = if (nrow(df) < VALIDATION_MIN_HPD_ROWS) paste0("n=", nrow(df)) else NULL
  )

  pid_col <- names(df)[grepl("project|id", names(df))][1]
  boro_col <- names(df)[grepl("borough|boro", names(df))][1]
  out$checks$has_key_cols <- list(
    passed = length(pid_col) > 0 && length(boro_col) > 0,
    severity = "warning",
    detail = if (length(pid_col) == 0 || length(boro_col) == 0) "Missing project_id or borough" else NULL
  )

  unit_col <- names(df)[grepl("total_unit|unit_count|units", names(df))][1]
  if (length(unit_col) && unit_col %in% names(df)) {
    u <- as.numeric(df[[unit_col]])
    u <- u[!is.na(u) & u >= 0]
    out$units_sum <- sum(u, na.rm = TRUE)
    out$units_median <- if (length(u)) as.numeric(median(u)) else NA_real_
    out$checks$units_nonnegative <- list(passed = all(as.numeric(df[[unit_col]]) >= 0, na.rm = TRUE), severity = "warning", detail = NULL)
    out$units_p99 <- if (length(u)) as.numeric(quantile(u, 0.99)) else NA_real_
    n_extreme <- sum(u > 1000, na.rm = TRUE)
    out$checks$units_no_extreme_outliers <- list(passed = n_extreme == 0, severity = "warning",
      detail = if (n_extreme > 0) paste0(n_extreme, " rows with units > 1000") else NULL)
  }
  bid_col <- names(df)[grepl("building_id", names(df))][1]
  bbl_col <- names(df)[grepl("^bbl$", names(df))][1]
  pid_col <- names(df)[grepl("project_id|project_", names(df))][1]
  if (length(pid_col) && pid_col %in% names(df)) {
    key <- if (length(bid_col) && bid_col %in% names(df))
      paste0(as.character(df[[pid_col]]), "_", as.character(df[[bid_col]]))
    else if (length(bbl_col) && bbl_col %in% names(df))
      paste0(as.character(df[[pid_col]]), "_", as.character(df[[bbl_col]]))
    else NULL
    if (!is.null(key)) {
      n_dup <- sum(duplicated(key))
      out$n_duplicate_keys <- n_dup
      out$checks$uniqueness_building_key <- list(passed = n_dup == 0, severity = "warning",
        detail = if (n_dup > 0) paste0(n_dup, " duplicate (project_id, building_id/bbl)") else NULL)
    }
  }

  coord_val_path <- file.path(OUT_VALIDATION, "coord_source_validation.json")
  if (file.exists(coord_val_path)) {
    cv <- jsonlite::read_json(coord_val_path)
    out$coord_source_counts <- cv$coord_source_counts
    out$pct_fallback_coords <- cv$pct_fallback_coords
    out$checks$low_fallback_coords <- list(
      passed = is.null(cv$pct_fallback_coords) || (cv$pct_fallback_coords / 100) <= VALIDATION_MAX_PCT_FALLBACK,
      severity = "warning",
      detail = if (!is.null(cv$pct_fallback_coords)) paste0("pct_fallback = ", cv$pct_fallback_coords, "%") else NULL)
  }

  validation_report$hpd_buildings <<- out
  invisible(out)
}

validate_boroughs_sf <- function() {
  path <- file.path(RAW, FILE_BOROUGH_GEOJSON)
  out <- list(n_features = 0L, crs = NULL, checks = list(), critical_fail = FALSE)

  if (!file.exists(path)) {
    out$checks$file_exists <- list(passed = FALSE, severity = "critical", detail = "File not found")
    out$critical_fail <- TRUE
    validation_report$boroughs_sf <<- out
    return(invisible(out))
  }

  sf_obj <- try(sf::st_read(path, quiet = TRUE), silent = TRUE)
  if (inherits(sf_obj, "try-error")) {
    out$checks$read_geojson <- list(passed = FALSE, severity = "critical", detail = as.character(attr(sf_obj, "condition")))
    out$critical_fail <- TRUE
    validation_report$boroughs_sf <<- out
    return(invisible(out))
  }

  out$n_features <- nrow(sf_obj)
  out$crs <- st_crs(sf_obj)$input
  out$checks$file_exists <- list(passed = TRUE, severity = "critical", detail = NULL)
  out$checks$has_features <- list(passed = nrow(sf_obj) >= 5, severity = "warning", detail = if (nrow(sf_obj) < 5) "Expected 5 boroughs" else NULL)
  valid_geoms <- st_is_valid(sf_obj)
  out$checks$geometry_valid <- list(passed = all(valid_geoms), severity = "warning", detail = if (!all(valid_geoms)) paste0(sum(!valid_geoms), " invalid") else NULL)
  bbox_sf <- st_bbox(sf_obj)
  out$bbox <- as.list(bbox_sf)
  out$geometry_type <- as.character(unique(st_geometry_type(sf_obj)))

  validation_report$boroughs_sf <<- out
  invisible(out)
}

run_validate <- function() {
  validate_subway()
  validate_hpd_buildings()
  validate_boroughs_sf()
  validation_report$critical_fail <<- validation_report$subway$critical_fail ||
    validation_report$hpd_buildings$critical_fail ||
    validation_report$boroughs_sf$critical_fail
  write(toJSON(validation_report, auto_unbox = TRUE, pretty = TRUE),
        file.path(OUT_VALIDATION, paste0("validation_", RUN_ID, ".json")))
  invisible(validation_report)
}

if (identical(Sys.getenv("SOURCE_ME"), "1")) run_validate()
