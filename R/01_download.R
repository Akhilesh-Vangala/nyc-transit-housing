source("config.R")

library(here)
library(readr)
library(jsonlite)

write_manifest <- function(path) {
  if (!file.exists(path)) return(invisible(NULL))
  m <- list(
    path = path,
    size_bytes = file.info(path)$size,
    modified = format(file.info(path)$mtime, "%Y-%m-%dT%H:%M:%SZ"),
    run_id = RUN_ID
  )
  manifest_path <- file.path(RAW, paste0(basename(path), ".manifest.json"))
  write(toJSON(m, auto_unbox = TRUE), manifest_path)
  invisible(manifest_path)
}

download_with_retry <- function(url, path, max_attempts = DOWNLOAD_MAX_ATTEMPTS) {
  extra <- if (capabilities("libcurl")) c("--location", "--max-time", as.character(DOWNLOAD_TIMEOUT_SEC)) else character()
  for (attempt in seq_len(max_attempts)) {
    tryCatch({
      suppressWarnings(
        download.file(url, path, mode = "wb", quiet = TRUE,
          method = if (capabilities("libcurl")) "libcurl" else "auto",
          extra = extra)
      )
      if (file.exists(path) && file.info(path)$size > 0) return(path)
    }, error = function(e) NULL)
    Sys.sleep(2^attempt)
  }
  NULL
}

assert_has_columns <- function(df, required_substrings) {
  nms <- tolower(names(df))
  for (sub in required_substrings) {
    if (!any(grepl(sub, nms, fixed = TRUE)))
      stop("Missing column matching: ", sub)
  }
  invisible(TRUE)
}

download_subway <- function(force = FALSE) {
  path <- file.path(RAW, FILE_SUBWAY)
  if (file.exists(path) && !force) {
    write_manifest(path)
    return(path)
  }
  ok <- download_with_retry(URL_SUBWAY_CSV, path)
  if (is.null(ok)) return(NULL)
  df <- read_csv(path, show_col_types = FALSE)
  names(df) <- normalize_names(names(df))
  assert_has_columns(df, c("lat", "lon"))
  lat_cand <- names(df)[grepl("lat|y$", names(df))][1]
  lon_cand <- names(df)[grepl("lon|x$|long", names(df))][1]
  if (length(lat_cand) >= 1) names(df)[names(df) == lat_cand[1]] <- "latitude"
  if (length(lon_cand) >= 1) names(df)[names(df) == lon_cand[1]] <- "longitude"
  if (!"station_name" %in% names(df) && ncol(df) >= 2)
    names(df)[2] <- "station_name"
  write_csv(df, path)
  write_manifest(path)
  path
}

download_hpd_buildings <- function(force = FALSE) {
  path <- file.path(RAW, FILE_HPD_BUILDINGS)
  if (file.exists(path) && !force) {
    write_manifest(path)
    return(path)
  }
  ok <- download_with_retry(URL_HPD_BUILDINGS, path)
  if (is.null(ok)) return(NULL)
  df <- read_csv(path, show_col_types = FALSE)
  names(df) <- normalize_names(names(df))
  assert_has_columns(df, c("borough", "project"))
  write_csv(df, path)
  write_manifest(path)
  path
}

download_hpd_projects <- function(force = FALSE) {
  path <- file.path(RAW, FILE_HPD_PROJECTS)
  if (file.exists(path) && !force) return(path)
  ok <- download_with_retry(URL_HPD_PROJECTS, path)
  if (is.null(ok)) return(NULL)
  df <- read_csv(path, show_col_types = FALSE)
  names(df) <- normalize_names(names(df))
  write_csv(df, path)
  write_manifest(path)
  path
}

download_boroughs <- function(force = FALSE) {
  path <- file.path(RAW, FILE_BOROUGH_GEOJSON)
  if (file.exists(path) && !force) {
    write_manifest(path)
    return(path)
  }
  ok <- download_with_retry(URL_BOROUGH_GEOJSON, path)
  if (is.null(ok) && exists("URL_BOROUGH_GEOJSON_FALLBACK"))
    ok <- download_with_retry(URL_BOROUGH_GEOJSON_FALLBACK, path)
  if (is.null(ok)) return(NULL)
  write_manifest(path)
  path
}

download_nta <- function(force = FALSE) {
  if (!exists("URL_NTA_GEOJSON") || !exists("FILE_NTA_GEOJSON")) return(NULL)
  path <- file.path(RAW, FILE_NTA_GEOJSON)
  if (file.exists(path) && !force) return(path)
  ok <- download_with_retry(URL_NTA_GEOJSON, path)
  if (is.null(ok)) return(NULL)
  write_manifest(path)
  path
}

download_subway_entrances <- function(force = FALSE) {
  if (!exists("URL_SUBWAY_ENTRANCES") || !exists("FILE_SUBWAY_ENTRANCES")) return(NULL)
  path <- file.path(RAW, FILE_SUBWAY_ENTRANCES)
  if (file.exists(path) && !force) {
    write_manifest(path)
    return(path)
  }
  ok <- download_with_retry(URL_SUBWAY_ENTRANCES, path)
  if (is.null(ok)) return(NULL)
  df <- read_csv(path, show_col_types = FALSE)
  names(df) <- normalize_names(names(df))
  if (!any(grepl("lat|latitude", names(df))) || !any(grepl("lon|longitude", names(df)))) return(NULL)
  write_csv(df, path)
  write_manifest(path)
  path
}

run_download <- function(force = FALSE) {
  if (exists("USE_SUBWAY_ENTRANCES") && isTRUE(USE_SUBWAY_ENTRANCES))
    download_subway_entrances(force)
  else
    download_subway(force)
  download_hpd_buildings(force)
  download_hpd_projects(force)
  download_boroughs(force)
  download_nta(force)
  invisible(NULL)
}

if (identical(Sys.getenv("SOURCE_ME"), "1")) run_download()
