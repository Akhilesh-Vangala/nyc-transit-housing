source("config.R")

library(readr)
library(dplyr)
library(sf)

OUT_DATA_REQUESTS <- file.path(OUT_TABLES, "data_requests")
if (!dir.exists(OUT_DATA_REQUESTS)) dir.create(OUT_DATA_REQUESTS, recursive = TRUE)

run_data_requests <- function() {
  path_bld <- file.path(PROCESSED, "buildings_with_buffer.rds")
  if (!file.exists(path_bld)) stop("Run 04_buffer_analysis first.")
  b <- readRDS(path_bld)
  if (!"distance_to_subway_m" %in% names(b)) stop("buildings_with_buffer must have distance_to_subway_m.")

  path_borough <- file.path(OUT_TABLES, "units_by_borough_500m.csv")
  if (file.exists(path_borough)) {
    write_csv(read_csv(path_borough, show_col_types = FALSE),
              file.path(OUT_DATA_REQUESTS, "request_units_500m_by_borough.csv"))
  }
  path_borough_summary <- file.path(OUT_TABLES, "borough_summary_500m.csv")
  if (file.exists(path_borough_summary)) {
    write_csv(read_csv(path_borough_summary, show_col_types = FALSE),
              file.path(OUT_DATA_REQUESTS, "request_borough_summary_500m.csv"))
  }
  path_nta_rank <- file.path(OUT_TABLES, "top_bottom_10_nta_500m.csv")
  if (file.exists(path_nta_rank)) {
    write_csv(read_csv(path_nta_rank, show_col_types = FALSE),
              file.path(OUT_DATA_REQUESTS, "request_top_bottom_10_neighborhoods.csv"))
  }
  beyond_1km <- b %>%
    as.data.frame() %>%
    filter(distance_to_subway_m > 1000 | is.na(distance_to_subway_m)) %>%
    select(any_of(c("project_id", "building_id", "borough", "units", "distance_to_subway_m", "coord_source")))
  if (nrow(beyond_1km) > 0)
    write_csv(beyond_1km, file.path(OUT_DATA_REQUESTS, "request_buildings_beyond_1km_subway.csv"))
  invisible(NULL)
}

if (identical(Sys.getenv("SOURCE_ME"), "1")) run_data_requests()
