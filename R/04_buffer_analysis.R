source("config.R")

library(dplyr)
library(sf)
library(units)
library(jsonlite)
library(readr)

load_processed_m <- function() {
  path_sub <- file.path(PROCESSED, "subway_m.rds")
  path_bld <- file.path(PROCESSED, "buildings_m.rds")
  if (!file.exists(path_sub) || !file.exists(path_bld))
    stop("Processed spatial data not found. Run 03_prepare_spatial.R first.")
  list(
    subway = readRDS(path_sub),
    buildings = readRDS(path_bld)
  )
}

add_distance_to_subway <- function(sf_buildings, sf_subway) {
  nn <- st_nearest_feature(sf_buildings, sf_subway)
  dist_m <- st_distance(sf_buildings, sf_subway[nn, ], by_element = TRUE)
  sf_buildings$nearest_subway_idx <- nn
  sf_buildings$distance_to_subway_m <- as.numeric(dist_m)
  sf_buildings
}

add_buffer_flags <- function(sf_buildings, sf_subway, radii_m = BUFFER_RADIUS_M) {
  for (r in radii_m) {
    buf <- st_union(st_buffer(sf_subway, r))
    inside <- st_intersects(sf_buildings, buf, sparse = FALSE)[, 1]
    sf_buildings[[paste0("within_", r, "m")]] <- inside
  }
  sf_buildings
}

export_buffer_polygons <- function(sf_subway, radii_m = BUFFER_RADIUS_M) {
  buf_list <- list()
  for (r in radii_m) {
    buf <- st_union(st_buffer(sf_subway, r))
    buf <- st_sf(buffer_m = r, geometry = st_sfc(buf, crs = st_crs(sf_subway)))
    buf_list[[as.character(r)]] <- buf
  }
  buf_sf <- bind_rows(buf_list)
  path_buf <- file.path(PROCESSED, "buffer_polygons_m.rds")
  saveRDS(buf_sf, path_buf)
  invisible(path_buf)
}

distance_summary_stats <- function(sf_buildings) {
  d <- sf_buildings$distance_to_subway_m
  d <- d[!is.na(d) & is.finite(d) & d >= 0]
  if (length(d) == 0) return(NULL)
  list(
    run_id = RUN_ID,
    n = length(d),
    mean_m = mean(d),
    median_m = median(d),
    sd_m = sd(d),
    p25_m = quantile(d, 0.25),
    p75_m = quantile(d, 0.75),
    p90_m = quantile(d, 0.90),
    min_m = min(d),
    max_m = max(d)
  )
}

run_buffer_analysis <- function() {
  dat <- load_processed_m()
  sf_subway <- dat$subway
  sf_buildings <- dat$buildings
  if (st_crs(sf_subway)$epsg == 4326)
    stop("Buffers must be computed in projected CRS (not WGS84). Check 03_prepare_spatial outputs.")

  sf_buildings <- add_distance_to_subway(sf_buildings, sf_subway)
  sf_buildings <- add_buffer_flags(sf_buildings, sf_subway, BUFFER_RADIUS_M)

  path_out <- file.path(PROCESSED, "buildings_with_buffer.rds")
  saveRDS(sf_buildings, path_out)

  export_buffer_polygons(sf_subway, BUFFER_RADIUS_M)

  stats <- distance_summary_stats(sf_buildings)
  if (!is.null(stats)) {
    write(toJSON(stats, auto_unbox = TRUE), file.path(OUT_TABLES, "distance_summary.json"))
  }

  invisible(sf_buildings)
}

if (identical(Sys.getenv("SOURCE_ME"), "1")) run_buffer_analysis()
