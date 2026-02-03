BASE <- Sys.getenv("TRANSIT_BASE", unset = normalizePath(getwd()))
if (!dir.exists(BASE)) dir.create(BASE, recursive = TRUE)

RAW            <- file.path(BASE, "data", "raw")
PROCESSED      <- file.path(BASE, "data", "processed")
INTERMEDIATE   <- file.path(BASE, "data", "intermediate")
OUTPUTS        <- file.path(BASE, "outputs")
OUT_FIGURES    <- file.path(OUTPUTS, "figures")
OUT_TABLES     <- file.path(OUTPUTS, "tables")
OUT_VALIDATION <- file.path(OUTPUTS, "validation")

for (d in c(RAW, PROCESSED, INTERMEDIATE, OUT_FIGURES, OUT_TABLES, OUT_VALIDATION)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

URL_SUBWAY_CSV <- "https://data.ny.gov/api/views/39hk-dx4f/rows.csv?accessType=DOWNLOAD"
URL_SUBWAY_ENTRANCES <- "https://data.cityofnewyork.us/api/views/kk4q-3rt2/rows.csv?accessType=DOWNLOAD"
URL_HPD_BUILDINGS <- "https://data.cityofnewyork.us/api/views/hg8x-zxpr/rows.csv?accessType=DOWNLOAD"
URL_HPD_PROJECTS  <- "https://data.cityofnewyork.us/api/views/hq68-rnsi/rows.csv?accessType=DOWNLOAD"
URL_BOROUGH_GEOJSON <- "https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=GeoJSON"
URL_BOROUGH_GEOJSON_FALLBACK <- "https://raw.githubusercontent.com/codeforgermany/click_that_hood/main/public/data/new-york-city-boroughs.geojson"

FILE_SUBWAY       <- "mta_subway_stations.csv"
FILE_HPD_BUILDINGS <- "hpd_buildings.csv"
FILE_HPD_PROJECTS  <- "hpd_projects.csv"
FILE_BOROUGH_GEOJSON <- "nyc_boroughs.geojson"

CRS_WGS84 <- 4326
CRS_NYC_STATEPLANE_FT <- 2263
CRS_NYC_M <- 32118

buf_env <- Sys.getenv("TRANSIT_BUFFER_M", unset = "")
BUFFER_RADIUS_M <- if (nzchar(buf_env)) as.integer(strsplit(buf_env, ",", fixed = TRUE)[[1]]) else c(250, 500, 1000)
BUFFER_LABELS   <- setNames(paste0(BUFFER_RADIUS_M, "m"), paste0("r", BUFFER_RADIUS_M))

DISTANCE_BAND_BREAKS_M <- c(0, 250, 500, 1000, 2000, Inf)
DISTANCE_BAND_LABELS <- c("0-250m", "250-500m", "500m-1km", "1-2km", ">2km")

BBOX_NYC_WGS84 <- list(
  lon_min = -74.26, lon_max = -73.70,
  lat_min = 40.49,  lat_max = 40.92
)

VALIDATION_MIN_SUBWAY_ROWS <- 400L
VALIDATION_MIN_HPD_ROWS <- 1000L
VALIDATION_BBOX_TOLERANCE <- 0.9
VALIDATION_COORD_PRECISION_DECIMALS <- 5L

GEOCODE_CACHE_FILE <- file.path(INTERMEDIATE, "geocode_cache.rds")
GEOCODE_BATCH_SIZE <- 500L
PATH_PLUTO_BBL_CENTROID <- NULL
VALIDATION_MAX_PCT_FALLBACK <- 0.10
USE_SUBWAY_ENTRANCES <- FALSE
FILE_SUBWAY_ENTRANCES <- "mta_subway_entrances.csv"
URL_NTA_GEOJSON <- "https://data.cityofnewyork.us/api/geospatial/9nt8-h7nd?method=export&format=GeoJSON"
FILE_NTA_GEOJSON <- "nyc_nta.geojson"

DOWNLOAD_TIMEOUT_SEC <- 60L
DOWNLOAD_MAX_ATTEMPTS <- 3L

REQUIRED_SUBWAY_COLS <- c("station_name", "latitude", "longitude")
REQUIRED_HPD_BUILDING_COLS <- c("project_id", "borough", "building_id")

RUN_ID <- format(Sys.time(), "%Y%m%d_%H%M%S")
SCHEMA_VERSION <- 1L

if (file.exists("R/utils.R")) source("R/utils.R")
