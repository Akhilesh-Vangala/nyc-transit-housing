source("config.R")

library(readr)
library(dplyr)
library(sf)
library(here)
library(jsonlite)

load_subway_sf <- function() {
  path <- if (exists("USE_SUBWAY_ENTRANCES") && isTRUE(USE_SUBWAY_ENTRANCES) && exists("FILE_SUBWAY_ENTRANCES"))
    file.path(RAW, FILE_SUBWAY_ENTRANCES) else file.path(RAW, FILE_SUBWAY)
  if (!file.exists(path)) stop("Subway file not found. Run 01_download.R first.")
  df <- read_csv(path, show_col_types = FALSE)
  names(df) <- normalize_names(names(df))
  lat_col <- names(df)[grepl("lat|latitude", names(df))][1]
  lon_col <- names(df)[grepl("lon|longitude", names(df))][1]
  if (is.na(lat_col) || is.na(lon_col)) stop("Subway: no lat/lon columns.")
  df <- df %>%
    filter(!is.na(.data[[lat_col]]), !is.na(.data[[lon_col]])) %>%
    filter(.data[[lon_col]] >= BBOX_NYC_WGS84$lon_min, .data[[lon_col]] <= BBOX_NYC_WGS84$lon_max,
           .data[[lat_col]] >= BBOX_NYC_WGS84$lat_min, .data[[lat_col]] <= BBOX_NYC_WGS84$lat_max)
  sf_out <- st_as_sf(df, coords = c(lon_col, lat_col), crs = CRS_WGS84, remove = FALSE)
  if (inherits(st_geometry(sf_out), "sfc_POINT") && length(st_geometry(sf_out)) > 0) {
    g <- st_geometry(sf_out)
    if (length(g[[1]]) > 2) g <- st_zm(g, drop = TRUE, what = "ZM")
    st_geometry(sf_out) <- g
  }
  sf_out
}

load_hpd_buildings_with_coords <- function() {
  path_points <- file.path(PROCESSED, "buildings_points.rds")
  if (file.exists(path_points)) {
    df <- readRDS(path_points)
    if (!"latitude" %in% names(df)) df$latitude <- df$lat
    if (!"longitude" %in% names(df)) df$longitude <- df$lon
    if (!"units" %in% names(df)) df$units <- 1L
    sf_out <- st_as_sf(df, coords = c("longitude", "latitude"), crs = CRS_WGS84, remove = FALSE)
  } else {
    path <- file.path(RAW, FILE_HPD_BUILDINGS)
    if (!file.exists(path)) stop("HPD buildings file not found. Run 01_download.R then 03b_assign_building_coords.R.")
    df <- read_csv(path, show_col_types = FALSE)
    names(df) <- normalize_names(names(df))
    pid_col <- names(df)[grepl("project|id", names(df))][1]
    boro_col <- names(df)[grepl("borough|boro", names(df))][1]
    unit_col <- names(df)[grepl("total_unit|unit_count|units", names(df))][1]
    if (is.na(pid_col)) pid_col <- names(df)[1]
    if (is.na(boro_col)) boro_col <- names(df)[min(2, ncol(df))]
    boro_centroids <- data.frame(
      borough = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"),
      lon = c(-73.8648, -73.9442, -73.9712, -73.7949, -74.1502),
      lat = c(40.8448, 40.6782, 40.7831, 40.7282, 40.5795)
    )
    df$longitude <- NA_real_
    df$latitude <- NA_real_
    boro_col_use <- names(df)[grepl("borough|boro", names(df), ignore.case = TRUE)][1]
    if (length(boro_col_use) == 0) boro_col_use <- names(df)[min(2, ncol(df))]
    for (i in seq_len(nrow(boro_centroids))) {
      b <- tolower(trimws(boro_centroids$borough[i]))
      idx <- which(tolower(trimws(as.character(df[[boro_col_use]]))) == b)
      df$longitude[idx] <- boro_centroids$lon[i]
      df$latitude[idx] <- boro_centroids$lat[i]
    }
    df <- df %>% filter(!is.na(longitude), !is.na(latitude))
    if (nrow(df) == 0) stop("No buildings with valid borough.")
    if (length(unit_col) && unit_col %in% names(df)) df[[unit_col]] <- as.numeric(df[[unit_col]])
    df$units <- if (length(unit_col) && unit_col %in% names(df)) df[[unit_col]] else 1L
    df$units[is.na(df$units)] <- 1L
    df$coord_source <- "fallback"
    sf_out <- st_as_sf(df, coords = c("longitude", "latitude"), crs = CRS_WGS84, remove = FALSE)
  }
  if (inherits(st_geometry(sf_out), "sfc_POINT") && length(st_geometry(sf_out)) > 0) {
    g <- st_geometry(sf_out)
    if (length(g[[1]]) > 2) g <- st_zm(g, drop = TRUE, what = "ZM")
    st_geometry(sf_out) <- g
  }
  path_nta <- exists("FILE_NTA_GEOJSON") && nzchar(FILE_NTA_GEOJSON) && file.exists(file.path(RAW, FILE_NTA_GEOJSON))
  if (path_nta) {
    nta_sf <- st_read(file.path(RAW, FILE_NTA_GEOJSON), quiet = TRUE)
    nta_sf <- st_transform(nta_sf, st_crs(sf_out))
    nta_code_col <- names(nta_sf)[grepl("ntacode|nta_code|nta", tolower(names(nta_sf)))][1]
    if (!is.na(nta_code_col)) {
      nta_join <- nta_sf[, nta_code_col, drop = FALSE]
      names(nta_join)[1] <- "nta_code"
      sf_out <- st_join(sf_out, nta_join, join = st_intersects, left = TRUE)
    }
  }
  sf_out
}

to_metric <- function(sf_obj) {
  if (st_crs(sf_obj)$epsg == CRS_NYC_M) return(sf_obj)
  st_transform(sf_obj, CRS_NYC_M)
}

ensure_valid_geometry <- function(sf_obj) {
  if (!all(st_is_valid(sf_obj))) {
    sf_obj <- st_make_valid(sf_obj)
  }
  sf_obj
}

save_processed <- function(sf_subway, sf_buildings) {
  sf_subway <- ensure_valid_geometry(sf_subway)
  sf_buildings <- ensure_valid_geometry(sf_buildings)
  sf_subway_m <- to_metric(sf_subway)
  sf_buildings_m <- to_metric(sf_buildings)
  saveRDS(sf_subway, file.path(PROCESSED, "subway_wgs84.rds"))
  saveRDS(sf_subway_m, file.path(PROCESSED, "subway_m.rds"))
  saveRDS(sf_buildings, file.path(PROCESSED, "buildings_wgs84.rds"))
  saveRDS(sf_buildings_m, file.path(PROCESSED, "buildings_m.rds"))
  meta <- list(
    run_id = RUN_ID,
    schema_version = SCHEMA_VERSION,
    n_subway = nrow(sf_subway),
    n_buildings = nrow(sf_buildings),
    crs_wgs84 = CRS_WGS84,
    crs_metric = CRS_NYC_M,
    bbox_wgs84 = as.list(st_bbox(sf_buildings)),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  )
  write(toJSON(meta, auto_unbox = TRUE), file.path(PROCESSED, "prepare_metadata.json"))
}

run_prepare_spatial <- function() {
  sf_subway <- load_subway_sf()
  sf_buildings <- load_hpd_buildings_with_coords()
  save_processed(sf_subway, sf_buildings)
  list(subway = sf_subway, buildings = sf_buildings)
}

if (identical(Sys.getenv("SOURCE_ME"), "1")) run_prepare_spatial()
