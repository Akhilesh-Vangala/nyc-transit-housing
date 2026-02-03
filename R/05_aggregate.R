source("config.R")

library(dplyr)
library(readr)
library(sf)

load_buildings_buffer <- function() {
  path <- file.path(PROCESSED, "buildings_with_buffer.rds")
  if (!file.exists(path)) stop("Run 04_buffer_analysis.R first.")
  readRDS(path)
}

aggregate_by_buffer <- function(sf_buildings) {
  unit_col <- "units"
  if (!"units" %in% names(sf_buildings)) {
    if ("total_units" %in% names(sf_buildings)) sf_buildings$units <- sf_buildings$total_units
    else sf_buildings$units <- 1L
  }
  total_units <- sum(sf_buildings$units, na.rm = TRUE)
  out <- list()
  for (r in BUFFER_RADIUS_M) {
    col <- paste0("within_", r, "m")
    if (!col %in% names(sf_buildings)) next
    units_in <- sum(sf_buildings$units[sf_buildings[[col]]], na.rm = TRUE)
    out[[paste0("r", r)]] <- data.frame(
      buffer_m = r,
      units_within_buffer = units_in,
      units_total = total_units,
      pct_units_near_transit = round(100 * units_in / total_units, 2)
    )
  }
  bind_rows(out)
}

aggregate_by_borough_buffer <- function(sf_buildings) {
  if (!"units" %in% names(sf_buildings)) sf_buildings$units <- 1L
  boro_col <- names(sf_buildings)[grepl("borough|boro", names(sf_buildings), ignore.case = TRUE)][1]
  if (is.na(boro_col)) boro_col <- "borough_std"
  col500 <- paste0("within_", 500, "m")
  if (!col500 %in% names(sf_buildings)) return(NULL)
  out <- as.data.frame(sf_buildings)
  out <- out[, c(boro_col, "units", col500)]
  names(out) <- c("borough", "units", "within_500m")
  out %>%
    mutate(borough = trimws(as.character(borough))) %>%
    group_by(borough) %>%
    summarise(
      units_total = sum(units, na.rm = TRUE),
      units_within_500m = sum(units[within_500m], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(pct_near_transit = round(100 * units_within_500m / pmax(units_total, 1), 2))
}

aggregate_by_borough_all_buffers <- function(sf_buildings) {
  if (!"units" %in% names(sf_buildings)) sf_buildings$units <- 1L
  boro_col <- names(sf_buildings)[grepl("borough|boro", names(sf_buildings), ignore.case = TRUE)][1]
  if (is.na(boro_col)) return(NULL)
  buf_cols <- paste0("within_", BUFFER_RADIUS_M, "m")
  if (!all(buf_cols %in% names(sf_buildings))) return(NULL)
  df <- as.data.frame(sf_buildings)
  df$borough <- trimws(as.character(df[[boro_col]]))
  out <- df %>% group_by(borough) %>% summarise(
    units_total = sum(units, na.rm = TRUE),
    .groups = "drop"
  )
  for (bc in buf_cols) {
    agg <- df %>% group_by(borough) %>% summarise(!!bc := sum(units[!!sym(bc)], na.rm = TRUE), .groups = "drop")
    out <- left_join(out, agg, by = "borough")
  }
  for (bc in buf_cols) {
    if (bc %in% names(out)) out[[paste0("pct_", bc)]] <- round(100 * out[[bc]] / pmax(out$units_total, 1), 2)
  }
  out
}

aggregate_distance_bins <- function(sf_buildings) {
  if (!"distance_to_subway_m" %in% names(sf_buildings)) return(NULL)
  sf_buildings %>%
    as.data.frame() %>%
    mutate(
      dist_band = cut(
        distance_to_subway_m,
        breaks = DISTANCE_BAND_BREAKS_M,
        labels = DISTANCE_BAND_LABELS,
        include.lowest = TRUE
      )
    ) %>%
    group_by(dist_band) %>%
    summarise(
      n_buildings = n(),
      units = sum(units, na.rm = TRUE),
      .groups = "drop"
    )
}

aggregate_borough_distance_band <- function(sf_buildings) {
  if (!"distance_to_subway_m" %in% names(sf_buildings)) return(NULL)
  boro_col <- names(sf_buildings)[grepl("borough|boro", names(sf_buildings), ignore.case = TRUE)][1]
  if (is.na(boro_col)) return(NULL)
  sf_buildings %>%
    as.data.frame() %>%
    mutate(
      borough = trimws(as.character(.data[[boro_col]])),
      dist_band = cut(
        distance_to_subway_m,
        breaks = DISTANCE_BAND_BREAKS_M,
        labels = DISTANCE_BAND_LABELS,
        include.lowest = TRUE
      )
    ) %>%
    group_by(borough, dist_band) %>%
    summarise(units = sum(units, na.rm = TRUE), .groups = "drop")
}

aggregate_by_nta_500m <- function(sf_buildings) {
  nta_col <- names(sf_buildings)[grepl("nta|neighborhood_tabulation", names(sf_buildings), ignore.case = TRUE)][1]
  boro_col <- names(sf_buildings)[grepl("borough|boro", names(sf_buildings), ignore.case = TRUE)][1]
  if (is.na(nta_col)) return(NULL)
  col500 <- paste0("within_", 500, "m")
  if (!col500 %in% names(sf_buildings)) return(NULL)
  sf_buildings %>%
    as.data.frame() %>%
    mutate(nta_code = as.character(.data[[nta_col]])) %>%
    filter(!is.na(nta_code), nta_code != "") %>%
    group_by(nta_code) %>%
    summarise(
      units_total = sum(units, na.rm = TRUE),
      units_within_500m = sum(units[!!sym(col500)], na.rm = TRUE),
      borough = if (!is.na(boro_col)) first(trimws(as.character(.data[[boro_col]]))) else NA_character_,
      .groups = "drop"
    ) %>%
    mutate(pct_near_transit = round(100 * units_within_500m / pmax(units_total, 1), 2))
}

top_bottom_10_nta <- function(nta_table) {
  if (is.null(nta_table) || nrow(nta_table) < 2) return(NULL)
  nta_table <- nta_table %>% filter(units_total >= 10)
  if (nrow(nta_table) < 2) return(NULL)
  top10 <- nta_table %>% slice_max(order_by = pct_near_transit, n = 10)
  bottom10 <- nta_table %>% slice_min(order_by = pct_near_transit, n = 10)
  top10$rank_type <- "top10_pct_near_transit"
  bottom10$rank_type <- "bottom10_pct_near_transit"
  bind_rows(top10, bottom10)
}

aggregate_by_coord_source <- function(sf_buildings) {
  if (!"coord_source" %in% names(sf_buildings)) return(NULL)
  df <- as.data.frame(sf_buildings)
  if (!"units" %in% names(df)) df$units <- 1L
  out <- list()
  for (r in BUFFER_RADIUS_M) {
    col <- paste0("within_", r, "m")
    if (!col %in% names(df)) next
    agg <- df %>%
      group_by(coord_source) %>%
      summarise(
        buffer_m = r,
        units_total = sum(units, na.rm = TRUE),
        units_within_buffer = sum(units[!!sym(col)], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(pct_within = round(100 * units_within_buffer / pmax(units_total, 1), 2))
    out[[length(out) + 1]] <- agg
  }
  if (length(out) == 0) return(NULL)
  bind_rows(out)
}

aggregate_borough_500m_with_quality <- function(sf_buildings) {
  if (!"coord_source" %in% names(sf_buildings)) return(NULL)
  boro_col <- names(sf_buildings)[grepl("borough|boro", names(sf_buildings), ignore.case = TRUE)][1]
  if (is.na(boro_col)) return(NULL)
  col500 <- paste0("within_", 500, "m")
  if (!col500 %in% names(sf_buildings)) return(NULL)
  df <- as.data.frame(sf_buildings)
  df$borough <- trimws(as.character(df[[boro_col]]))
  df$reliable <- df$coord_source %in% c("direct", "internal")
  df %>%
    group_by(borough) %>%
    summarise(
      total_units = sum(units, na.rm = TRUE),
      units_within_500m = sum(units[!!sym(col500)], na.rm = TRUE),
      units_reliable_coords = sum(units[reliable], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      share_within_500m = round(100 * units_within_500m / pmax(total_units, 1), 2),
      coord_quality_share = round(100 * units_reliable_coords / pmax(total_units, 1), 2)
    ) %>%
    select(borough, total_units, units_within_500m, share_within_500m, coord_quality_share)
}

sensitivity_by_coord_subset <- function(sf_buildings) {
  if (!"coord_source" %in% names(sf_buildings)) return(NULL)
  df <- as.data.frame(sf_buildings)
  if (!"units" %in% names(df)) df$units <- 1L
  subsets <- list(
    hpd_only = df %>% filter(coord_source == "direct"),
    hpd_plus_taxlot = df %>% filter(coord_source %in% c("direct", "internal")),
    all = df
  )
  out <- list()
  for (sub_name in names(subsets)) {
    sub <- subsets[[sub_name]]
    if (nrow(sub) == 0) next
    total_units <- sum(sub$units, na.rm = TRUE)
    for (r in BUFFER_RADIUS_M) {
      col <- paste0("within_", r, "m")
      if (!col %in% names(sub)) next
      units_in <- sum(sub$units[sub[[col]]], na.rm = TRUE)
      out[[length(out) + 1]] <- data.frame(
        subset = sub_name,
        buffer_m = r,
        units_total = total_units,
        units_within_buffer = units_in,
        pct_within = round(100 * units_in / pmax(total_units, 1), 2)
      )
    }
  }
  if (length(out) == 0) return(NULL)
  bind_rows(out)
}

run_aggregate <- function() {
  sf_b <- load_buildings_buffer()

  tbl_buffer <- aggregate_by_buffer(sf_b)
  tbl_borough_buffer <- aggregate_by_borough_buffer(sf_b)
  tbl_borough_buffers <- aggregate_by_borough_all_buffers(sf_b)
  tbl_dist <- aggregate_distance_bins(sf_b)
  tbl_borough_dist <- aggregate_borough_distance_band(sf_b)
  tbl_nta <- aggregate_by_nta_500m(sf_b)
  tbl_nta_rank <- top_bottom_10_nta(tbl_nta)
  tbl_by_coord <- aggregate_by_coord_source(sf_b)
  tbl_borough_summary <- aggregate_borough_500m_with_quality(sf_b)
  tbl_sensitivity <- sensitivity_by_coord_subset(sf_b)
  sf_reliable <- sf_b %>% filter(coord_source %in% c("direct", "internal"))
  tbl_buffer_reliable <- if (nrow(sf_reliable) > 0) aggregate_by_buffer(sf_reliable) else NULL
  tbl_borough_500m_reliable <- if (nrow(sf_reliable) > 0) aggregate_by_borough_buffer(sf_reliable) else NULL

  if (!is.null(tbl_buffer_reliable) && !is.null(tbl_borough_500m_reliable)) {
    sum_boro <- sum(tbl_borough_500m_reliable$units_total)
    buf_total <- tbl_buffer_reliable$units_total[1]
    if (abs(sum_boro - buf_total) > 1) warning("Borough total (", sum_boro, ") != buffer total (", buf_total, ") for reliable subset.")
  }

  write_csv(tbl_buffer, file.path(OUT_TABLES, "units_by_buffer.csv"))
  write_csv(tbl_buffer, file.path(OUT_TABLES, "units_by_buffer_all_coords.csv"))
  if (!is.null(tbl_buffer_reliable)) write_csv(tbl_buffer_reliable, file.path(OUT_TABLES, "units_by_buffer_reliable_coords.csv"))
  if (!is.null(tbl_borough_buffer)) write_csv(tbl_borough_buffer, file.path(OUT_TABLES, "units_by_borough_500m.csv"))
  if (!is.null(tbl_borough_500m_reliable)) write_csv(tbl_borough_500m_reliable, file.path(OUT_TABLES, "units_by_borough_500m_reliable.csv"))
  if (!is.null(tbl_borough_summary)) write_csv(tbl_borough_summary, file.path(OUT_TABLES, "borough_summary_500m.csv"))
  if (!is.null(tbl_borough_buffers)) write_csv(tbl_borough_buffers, file.path(OUT_TABLES, "units_by_borough_all_buffers.csv"))
  if (!is.null(tbl_dist)) write_csv(tbl_dist, file.path(OUT_TABLES, "units_by_distance_band.csv"))
  if (!is.null(tbl_borough_dist)) write_csv(tbl_borough_dist, file.path(OUT_TABLES, "units_by_borough_distance_band.csv"))
  if (!is.null(tbl_nta)) write_csv(tbl_nta, file.path(OUT_TABLES, "units_by_nta_500m.csv"))
  if (!is.null(tbl_nta_rank)) write_csv(tbl_nta_rank, file.path(OUT_TABLES, "top_bottom_10_nta_500m.csv"))
  tbl_underserved_memo <- if (!is.null(tbl_nta) && nrow(tbl_nta) > 0 && "borough" %in% names(tbl_nta))
    tbl_nta %>% filter(units_total >= 100) %>% arrange(pct_near_transit, desc(units_total)) %>% slice_head(n = 8) else NULL
  if (!is.null(tbl_underserved_memo)) write_csv(tbl_underserved_memo, file.path(OUT_TABLES, "underserved_nta_writing_sample.csv"))
  if (!is.null(tbl_by_coord)) write_csv(tbl_by_coord, file.path(OUT_TABLES, "units_by_buffer_by_coord_source.csv"))
  if (!is.null(tbl_sensitivity)) write_csv(tbl_sensitivity, file.path(OUT_TABLES, "sensitivity_by_coord_source.csv"))
  invisible(list(by_buffer = tbl_buffer, by_borough = tbl_borough_buffer, by_borough_buffers = tbl_borough_buffers, by_dist = tbl_dist, by_borough_dist = tbl_borough_dist))
}

if (identical(Sys.getenv("SOURCE_ME"), "1")) run_aggregate()
