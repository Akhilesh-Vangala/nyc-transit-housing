source("config.R")

library(dplyr)
library(sf)
library(ggplot2)

load_for_map <- function() {
  path_bld <- file.path(PROCESSED, "buildings_with_buffer.rds")
  path_sub <- file.path(PROCESSED, "subway_wgs84.rds")
  path_boro <- file.path(RAW, FILE_BOROUGH_GEOJSON)
  if (!file.exists(path_bld) || !file.exists(path_sub)) stop("Run 04_buffer_analysis and 03_prepare_spatial first.")
  buildings <- readRDS(path_bld)
  subway <- readRDS(path_sub)
  if (st_crs(buildings)$epsg != CRS_WGS84) buildings <- st_transform(buildings, CRS_WGS84)
  if (st_crs(subway)$epsg != CRS_WGS84) subway <- st_transform(subway, CRS_WGS84)
  boroughs_sf <- if (file.exists(path_boro)) st_read(path_boro, quiet = TRUE) else NULL
  list(buildings = buildings, subway = subway, boroughs = boroughs_sf)
}

plot_units_by_buffer <- function() {
  path_reliable <- file.path(OUT_TABLES, "units_by_buffer_reliable_coords.csv")
  path_tbl <- file.path(OUT_TABLES, "units_by_buffer.csv")
  path_tbl <- if (file.exists(path_reliable)) path_reliable else path_tbl
  if (!file.exists(path_tbl)) return(invisible(NULL))
  tbl <- read.csv(path_tbl)
  tbl$buffer_m <- factor(tbl$buffer_m, levels = c(250, 500, 1000), labels = c("250 m", "500 m", "1 km"))
  tbl$label_pct <- sprintf("%.1f%%", tbl$pct_units_near_transit)
  tbl$label_units <- format(tbl$units_within_buffer, big.mark = ",")
  tbl$label <- paste0(tbl$label_pct, "\n(", tbl$label_units, " units)")
  fill_vals <- c("250 m" = "#7f8c8d", "500 m" = "#2c3e50", "1 km" = "#7f8c8d")
  p <- ggplot(tbl, aes(x = buffer_m, y = pct_units_near_transit, fill = buffer_m)) +
    geom_col(width = 0.65) +
    scale_fill_manual(values = fill_vals, guide = "none") +
    geom_text(aes(label = label), vjust = -0.15, size = 3.6, fontface = "plain", lineheight = 0.95) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.22)), labels = function(x) paste0(x, "%")) +
    labs(
      title = "Share of Housing New York units within distance of subway",
      subtitle = "HPD + tax-lot coordinates only (reliable subset)",
      x = "Distance to nearest subway",
      y = "Share of units within buffer"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.35, color = "grey85"),
      plot.title = element_text(size = 13.5, face = "plain", hjust = 0, margin = margin(b = 4)),
      plot.subtitle = element_text(size = 10, color = "grey40", hjust = 0, margin = margin(b = 10)),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 11),
      plot.margin = margin(8, 12, 8, 8)
    )
  ggsave(file.path(OUT_FIGURES, "fig_units_by_buffer.png"), p, width = 6.5, height = 4.75, dpi = 300, bg = "white")
  p
}

plot_distance_dist <- function() {
  path_tbl <- file.path(OUT_TABLES, "units_by_distance_band.csv")
  if (!file.exists(path_tbl)) return(invisible(NULL))
  tbl <- read.csv(path_tbl)
  p <- ggplot(tbl, aes(x = dist_band, y = units, fill = dist_band)) +
    geom_col() +
    labs(x = "Distance to subway", y = "Units") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  ggsave(file.path(OUT_FIGURES, "fig_distance_band.png"), p, width = 7, height = 4, dpi = 150)
  p
}

plot_borough_comparison <- function() {
  path_tbl <- file.path(OUT_TABLES, "units_by_borough_500m.csv")
  if (!file.exists(path_tbl)) return(invisible(NULL))
  tbl <- read.csv(path_tbl)
  tbl$borough <- factor(tbl$borough, levels = tbl$borough[order(tbl$pct_near_transit, decreasing = TRUE)])
  p <- ggplot(tbl, aes(x = borough, y = pct_near_transit, fill = borough)) +
    geom_col() +
    labs(x = "Borough", y = "% within 500m") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  ggsave(file.path(OUT_FIGURES, "fig_borough_500m.png"), p, width = 6, height = 4, dpi = 150)
  p
}

plot_distance_density <- function() {
  path_bld <- file.path(PROCESSED, "buildings_with_buffer.rds")
  if (!file.exists(path_bld) || !"distance_to_subway_m" %in% names(readRDS(path_bld))) return(invisible(NULL))
  b <- readRDS(path_bld)
  d <- as.numeric(b$distance_to_subway_m)
  d <- d[is.finite(d) & d >= 0 & d <= 5000]
  if (length(d) == 0) return(invisible(NULL))
  df <- data.frame(distance_m = d)
  p <- ggplot(df, aes(x = distance_m)) +
    geom_histogram(bins = 50, fill = "steelblue", alpha = 0.8) +
    labs(x = "Distance to nearest subway (m)", y = "Count") +
    theme_minimal()
  ggsave(file.path(OUT_FIGURES, "fig_distance_histogram.png"), p, width = 6, height = 4, dpi = 150)
  p
}

plot_nta_proximity <- function() {
  path_tbl <- file.path(OUT_TABLES, "units_by_nta_500m.csv")
  if (!file.exists(path_tbl)) return(invisible(NULL))
  tbl <- read.csv(path_tbl)
  tbl <- tbl %>% filter(units_total >= 20) %>% slice_max(order_by = pct_near_transit, n = 20)
  if (nrow(tbl) == 0) return(invisible(NULL))
  tbl$nta_code <- factor(tbl$nta_code, levels = tbl$nta_code[order(tbl$pct_near_transit)])
  p <- ggplot(tbl, aes(x = nta_code, y = pct_near_transit, fill = pct_near_transit)) +
    geom_col() +
    labs(x = "Neighborhood (NTA)", y = "% units within 500m of subway") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  ggsave(file.path(OUT_FIGURES, "fig_nta_proximity.png"), p, width = 8, height = 5, dpi = 150)
  p
}

plot_nta_choropleth <- function() {
  path_nta <- file.path(RAW, FILE_NTA_GEOJSON)
  path_tbl <- file.path(OUT_TABLES, "units_by_nta_500m.csv")
  if (!file.exists(path_nta) || !file.exists(path_tbl)) return(invisible(NULL))
  nta_sf <- st_read(path_nta, quiet = TRUE)
  nta_code_col <- names(nta_sf)[grepl("ntacode|nta_code|nta", tolower(names(nta_sf)))][1]
  if (is.na(nta_code_col)) return(invisible(NULL))
  tbl <- read.csv(path_tbl)
  nta_sf[[nta_code_col]] <- as.character(nta_sf[[nta_code_col]])
  tbl$nta_code <- as.character(tbl$nta_code)
  nta_sf <- left_join(nta_sf, tbl[, c("nta_code", "pct_near_transit", "units_total")], by = setNames("nta_code", nta_code_col))
  if (!"pct_near_transit" %in% names(nta_sf)) return(invisible(NULL))
  p <- ggplot(nta_sf) +
    geom_sf(aes(fill = pct_near_transit), color = NA) +
    scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
    labs(fill = "% within 500m") +
    theme_void()
  ggsave(file.path(OUT_FIGURES, "fig_nta_choropleth.png"), p, width = 8, height = 8, dpi = 150)
  p
}

map_interactive <- function() {
  if (!requireNamespace("leaflet", quietly = TRUE) || !requireNamespace("htmlwidgets", quietly = TRUE))
    return(invisible(NULL))
  dat <- load_for_map()
  buildings <- dat$buildings
  subway <- dat$subway
  if (nrow(subway) == 0 || nrow(buildings) == 0) return(invisible(NULL))
  if (nrow(buildings) > 2000) buildings <- buildings %>% slice_sample(n = 2000)
  col500 <- paste0("within_", 500, "m")
  near_transit <- if (col500 %in% names(buildings)) as.logical(buildings[[col500]]) else rep(FALSE, nrow(buildings))
  if (length(near_transit) != nrow(buildings)) near_transit <- rep(FALSE, nrow(buildings))
  near_transit[is.na(near_transit)] <- FALSE
  sub_xy <- st_coordinates(st_geometry(subway))
  bld_xy <- st_coordinates(st_geometry(buildings))
  pal <- leaflet::colorFactor(c("navy", "darkgreen"), domain = c(FALSE, TRUE))
  m <- leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(lng = sub_xy[, 1], lat = sub_xy[, 2], radius = 4, color = "red", fillOpacity = 0.8, group = "Subway") %>%
    leaflet::addCircleMarkers(lng = bld_xy[, 1], lat = bld_xy[, 2], radius = 2, color = pal(near_transit), fillOpacity = 0.5, group = "Buildings") %>%
    leaflet::addLegend("bottomright", pal = pal, values = c(FALSE, TRUE), title = "Within 500m", labels = c("No", "Yes")) %>%
    leaflet::setView(-73.94, 40.70, zoom = 11)
  path_out <- file.path(OUT_FIGURES, "map_interactive.html")
  htmlwidgets::saveWidget(m, path_out, selfcontained = TRUE)
  m
}

run_visualize <- function() {
  plot_units_by_buffer()
  plot_distance_dist()
  plot_borough_comparison()
  plot_distance_density()
  plot_nta_proximity()
  tryCatch(plot_nta_choropleth(), error = function(e) NULL)
  tryCatch(map_interactive(), error = function(e) NULL)
  invisible(NULL)
}

if (identical(Sys.getenv("SOURCE_ME"), "1")) run_visualize()
