source("config.R")

library(readr)
library(dplyr)
library(ggplot2)

run_eda <- function() {
  path_bld <- file.path(PROCESSED, "buildings_points.rds")
  if (!file.exists(path_bld)) return(invisible(NULL))
  df <- readRDS(path_bld)
  if (!"units" %in% names(df)) df$units <- 1L
  if (!"coord_source" %in% names(df)) return(invisible(NULL))

  path_summary <- file.path(OUT_TABLES, "coord_source_summary.csv")
  if (file.exists(path_summary)) {
    cts <- read_csv(path_summary, show_col_types = FALSE)
    ord <- c("direct", "internal", "fallback")
    cts <- cts %>% filter(coord_source %in% ord) %>% mutate(coord_source = factor(coord_source, levels = ord))
    p1 <- ggplot(cts, aes(x = coord_source, y = n, fill = coord_source)) +
      geom_col(width = 0.65) +
      geom_text(aes(label = n), vjust = -0.3, size = 3) +
      scale_fill_manual(values = c("#2c3e50", "#34495e", "#7f8c8d"), guide = "none") +
      labs(x = "Coordinate source", y = "Buildings") +
      theme_minimal(base_size = 10) +
      theme(panel.grid.major.x = element_blank())
    ggsave(file.path(OUT_FIGURES, "fig_eda_coord_source.png"), p1, width = 5.5, height = 3.5, dpi = 300, bg = "white")
  }

  u <- df$units
  u <- u[!is.na(u) & u > 0 & u <= 2000]
  if (length(u) > 0) {
    dfu <- data.frame(units = u)
    p2 <- ggplot(dfu, aes(x = units)) +
      geom_histogram(bins = 50, fill = "#2c3e50", alpha = 0.9) +
      labs(x = "Units per building", y = "Count") +
      theme_minimal(base_size = 10)
    ggsave(file.path(OUT_FIGURES, "fig_eda_units_distribution.png"), p2, width = 5.5, height = 3.5, dpi = 300, bg = "white")
  }

  total_u <- sum(df$units, na.rm = TRUE)
  u_all <- df$units
  u_all <- u_all[!is.na(u_all) & u_all > 0]
  summ <- data.frame(
    n_buildings = nrow(df),
    total_units = total_u,
    mean_units_per_building = if (length(u_all)) round(mean(u_all), 2) else NA_real_,
    median_units_per_building = if (length(u_all)) median(u_all) else NA_real_,
    sd_units = if (length(u_all) > 1) round(sd(u_all), 2) else NA_real_,
    p25 = if (length(u_all)) as.numeric(quantile(u_all, 0.25)) else NA_real_,
    p75 = if (length(u_all)) as.numeric(quantile(u_all, 0.75)) else NA_real_
  )
  write_csv(summ, file.path(OUT_TABLES, "eda_buildings_summary.csv"))

  if ("borough_std" %in% names(df)) {
    by_boro <- df %>%
      group_by(borough_std) %>%
      summarise(n_buildings = n(), total_units = sum(units, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_units))
    write_csv(by_boro, file.path(OUT_TABLES, "eda_units_by_borough.csv"))
  }

  invisible(NULL)
}

if (identical(Sys.getenv("SOURCE_ME"), "1")) run_eda()
