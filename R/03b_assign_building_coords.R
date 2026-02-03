source("config.R")

library(readr)
library(dplyr)
library(jsonlite)

boro_centroids <- data.frame(
  borough = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"),
  lon = c(-73.8648, -73.9442, -73.9712, -73.7949, -74.1502),
  lat = c(40.8448, 40.6782, 40.7831, 40.7282, 40.5795),
  stringsAsFactors = FALSE
)

in_nyc_bbox <- function(lat, lon) {
  !is.na(lat) & !is.na(lon) &
    lon >= BBOX_NYC_WGS84$lon_min & lon <= BBOX_NYC_WGS84$lon_max &
    lat >= BBOX_NYC_WGS84$lat_min & lat <= BBOX_NYC_WGS84$lat_max
}

run_assign_building_coords <- function() {
  path <- file.path(RAW, FILE_HPD_BUILDINGS)
  if (!file.exists(path)) stop("HPD buildings file not found. Run 01_download.R first.")
  df <- read_csv(path, show_col_types = FALSE)
  names(df) <- normalize_names(names(df))

  pid_col <- names(df)[grepl("^project_id$|^project_", names(df))][1]
  bid_col <- names(df)[grepl("building_id|building_", names(df))][1]
  bbl_col <- names(df)[grepl("^bbl$", names(df))][1]
  boro_col <- names(df)[grepl("borough|boro", names(df), ignore.case = TRUE)][1]
  lat_col <- names(df)[grepl("^latitude$|^lat$", names(df))][1]
  lon_col <- names(df)[grepl("^longitude$|^lon$|^long$", names(df))][1]
  lat_int <- names(df)[grepl("latitude_internal|lat_internal", names(df))][1]
  lon_int <- names(df)[grepl("longitude_internal|lon_internal", names(df))][1]
  unit_col <- names(df)[grepl("total_units|all_counted_units", names(df))][1]
  if (is.na(unit_col)) unit_col <- names(df)[grepl("unit", names(df))][1]

  if (is.na(pid_col) || is.na(boro_col)) stop("HPD: missing project_id or borough column.")

  df$project_id <- as.character(df[[pid_col]])
  df$borough_std <- trimws(tolower(as.character(df[[boro_col]])))
  df$building_key <- if (!is.na(bid_col) && bid_col %in% names(df))
    paste0(df$project_id, "_", as.character(df[[bid_col]]))
  else if (!is.na(bbl_col) && bbl_col %in% names(df))
    paste0(df$project_id, "_", as.character(df[[bbl_col]]))
  else
    paste0(df$project_id, "_", seq_len(nrow(df)))
  df$units <- NA_real_
  if (length(unit_col) && unit_col %in% names(df))
    df$units <- as.numeric(df[[unit_col]])
  df$units[is.na(df$units) | df$units < 0] <- 1

  dedupe_by <- if (length(unit_col) && unit_col %in% names(df)) {
    df %>% group_by(building_key) %>% slice_max(order_by = units, n = 1, with_ties = FALSE) %>% ungroup()
  } else {
    df %>% group_by(building_key) %>% slice(1) %>% ungroup()
  }
  df <- as.data.frame(dedupe_by)

  lat <- lon <- rep(NA_real_, nrow(df))
  coord_source <- rep("fallback", nrow(df))

  if (length(lat_col) && length(lon_col) && lat_col %in% names(df) && lon_col %in% names(df)) {
    lat_d <- as.numeric(df[[lat_col]])
    lon_d <- as.numeric(df[[lon_col]])
    ok <- in_nyc_bbox(lat_d, lon_d)
    lat[ok] <- lat_d[ok]
    lon[ok] <- lon_d[ok]
    coord_source[ok] <- "direct"
  }
  need_coords <- coord_source == "fallback"
  if (any(need_coords) && length(lat_int) && length(lon_int) && lat_int %in% names(df) && lon_int %in% names(df)) {
    lat_i <- as.numeric(df[[lat_int]])
    lon_i <- as.numeric(df[[lon_int]])
    ok_int <- need_coords & in_nyc_bbox(lat_i, lon_i)
    lat[ok_int] <- lat_i[ok_int]
    lon[ok_int] <- lon_i[ok_int]
    coord_source[ok_int] <- "internal"
  }
  still_fallback <- coord_source == "fallback"
  for (i in seq_len(nrow(boro_centroids))) {
    b <- tolower(boro_centroids$borough[i])
    idx <- still_fallback & (df$borough_std == b)
    lat[idx] <- boro_centroids$lat[i]
    lon[idx] <- boro_centroids$lon[i]
  }

  df$latitude <- lat
  df$longitude <- lon
  df$coord_source <- coord_source
  df <- df %>% filter(!is.na(latitude), !is.na(longitude), in_nyc_bbox(latitude, longitude))
  if (nrow(df) == 0) stop("No buildings with valid coordinates.")

  counts <- as.data.frame(table(coord_source = df$coord_source, useNA = "no"))
  names(counts) <- c("coord_source", "n")
  counts$pct <- round(100 * counts$n / nrow(df), 2)
  n_fallback <- sum(df$coord_source == "fallback")
  pct_fallback <- round(100 * n_fallback / nrow(df), 2)
  pct_hpd <- round(100 * sum(df$coord_source == "direct") / nrow(df), 2)
  pct_taxlot <- round(100 * sum(df$coord_source == "internal") / nrow(df), 2)

  path_rds <- file.path(PROCESSED, "buildings_points.rds")
  saveRDS(df, path_rds)
  write_csv(counts, file.path(OUT_TABLES, "coord_source_summary.csv"))
  summary_pct <- data.frame(
    pct_coords_hpd = pct_hpd,
    pct_coords_taxlot = pct_taxlot,
    pct_coords_borough_fallback = pct_fallback
  )
  write_csv(summary_pct, file.path(OUT_TABLES, "coord_source_pct.csv"))
  val_coord <- list(
    run_id = RUN_ID,
    n_buildings = nrow(df),
    coord_source_counts = setNames(as.list(counts$n), counts$coord_source),
    pct_coords_hpd = pct_hpd,
    pct_coords_taxlot = pct_taxlot,
    pct_coords_borough_fallback = pct_fallback,
    pct_fallback_coords = pct_fallback,
    critical_fail = pct_fallback > (VALIDATION_MAX_PCT_FALLBACK * 100)
  )
  write(toJSON(val_coord, auto_unbox = TRUE), file.path(OUT_VALIDATION, "coord_source_validation.json"))
  invisible(list(data = df, coord_source_summary = counts, pct_fallback_coords = pct_fallback))
}

if (identical(Sys.getenv("SOURCE_ME"), "1")) run_assign_building_coords()
