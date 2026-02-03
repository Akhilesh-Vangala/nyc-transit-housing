# NYC Transit–Housing Proximity

HPD affordable housing units and subway proximity: buffer analysis (250/500/1000 m), distance to nearest station, aggregation by buffer, borough, distance band, and NTA. Building coordinates from HPD lat/lon or tax-lot centroid; borough centroid fallback. Outputs: tables (CSVs), figures (PNGs), interactive map (HTML), validation JSON.

Pipeline produces: share of units within 250/500/1000 m (overall and by coord source), borough and NTA tables, sensitivity table (hpd_only, hpd_plus_taxlot, all), and coord-quality percentages (pct_coords_hpd, pct_coords_taxlot, pct_coords_borough_fallback).

**What you can use this for:** comparing boroughs; identifying low-access areas; monitoring change over time (re-run with updated data).

**Data quality outputs** (run pipeline first): `outputs/tables/coord_source_summary.csv`, `outputs/tables/coord_source_pct.csv`, `outputs/validation/coord_source_validation.json` (pct_coords_hpd, pct_coords_taxlot, pct_coords_borough_fallback). EDA: `outputs/tables/eda_buildings_summary.csv`, `outputs/figures/fig_eda_coord_source.png`, `outputs/figures/fig_eda_units_distribution.png`. Proximity by coord source: `outputs/tables/units_by_buffer_by_coord_source.csv`. Sensitivity (HPD only vs HPD+tax-lot vs all): `outputs/tables/sensitivity_by_coord_source.csv`. Borough summary with coord quality: `outputs/tables/borough_summary_500m.csv`. For headline metrics and report, use reliable coords only (hpd_only or hpd_plus_taxlot in `sensitivity_by_coord_source.csv`); treat all-including-fallback as sensitivity only.

## Run

```bash
Rscript install_deps.R
Rscript run_all.R
```

Options: `--skip-download`, `--dry-run`.

macOS if `sf` fails to install: `./install_deps_macos.sh` or `./setup_r_homebrew_once.sh` then `Rscript install_deps.R`. Conda: `conda install -c conda-forge r-sf r-s2 r-leaflet r-dplyr r-readr r-ggplot2 r-htmlwidgets r-jsonlite r-units r-here r-rmarkdown`.

Report: `./render_report.sh` (uses Homebrew pandoc if set). The report includes data quality/EDA (coord source, units distribution), buffer and borough tables, NTA map, and distance-band figures; run the pipeline first so outputs exist.

## Reproduce outputs

All outputs under `outputs/` are generated locally by the pipeline and are gitignored. Run `Rscript run_all.R` from the project root. Tables: `outputs/tables/`. Figures: `outputs/figures/`. Validation: `outputs/validation/`. Only R scripts, config, and docs are committed; no HTML/map bundles or generated CSVs in repo. Example table layout: `docs/assets/sample_units_by_buffer.csv`.

## Config

`config.R`: paths, CRS (WGS84, EPSG:32118), buffer radii, validation thresholds. Env: `TRANSIT_BASE`, `TRANSIT_BUFFER_M`. Set `USE_SUBWAY_ENTRANCES = TRUE` to use entrance points instead of station points. For stations vs entrances comparison: run pipeline with `USE_SUBWAY_ENTRANCES = FALSE` and again with `TRUE`, then compare `borough_summary_500m.csv` or `units_by_buffer.csv`.

Validation outputs in `outputs/validation/`. High `pct_coords_borough_fallback` in `coord_source_validation.json` means many HPD rows lack coordinates (e.g. CONFIDENTIAL); pipeline continues with a warning. Use `sensitivity_by_coord_source.csv` for credible subsets.

## Data

- MTA Subway Stations (data.ny.gov)
- Housing New York Units by Building (data.cityofnewyork.us)
- NYC Borough boundaries (GeoJSON; fallback: GitHub click_that_hood)
- NTA polygons (optional): download in 01; spatial join in 03; outputs `units_by_nta_500m.csv`, `top_bottom_10_nta_500m.csv`. Entrances: set `USE_SUBWAY_ENTRANCES = TRUE` in config; pipeline uses entrances end-to-end (01, 02, 03).

## Outputs

Primary buffer tables: `units_by_buffer_reliable_coords.csv` (HPD + tax-lot only; use for headline metrics). `units_by_borough_500m_reliable.csv` (same subset; use with report). Borough 500 m: `units_by_borough_500m.csv` (all coords). Also: `coord_source_summary.csv`, `coord_source_validation.json`, `coord_source_pct.csv`; `units_by_buffer_by_coord_source.csv`; `sensitivity_by_coord_source.csv`; `borough_summary_500m.csv`. NTA: `units_by_nta_500m.csv`, `top_bottom_10_nta_500m.csv`, `underserved_nta_writing_sample.csv` (bottom 8 NTAs by share within 500 m, min 100 units). Figures: `fig_units_by_buffer.png` (share by buffer; Figure 1 in writing sample), `fig_nta_choropleth.png` (share within 500 m by NTA; in report), plus EDA and distance-band figures.

## Methods and limitations

- **CRS:** WGS84 for ingestion; NAD83 New York Long Island (EPSG:32118) for buffers and distances. Buffers and distance-to-nearest computed in meters.
- **Building coordinates:** Priority: HPD latitude/longitude (in NYC bbox) → latitude_internal/longitude_internal (tax-lot centroid) → borough centroid. Grain: one row per (building_id, project_id); dedupe by max total_units. Check `coord_source_summary.csv` and `coord_source_validation.json` for counts; high fallback share means many CONFIDENTIAL rows without lat/lon.
- **Transit geometry:** Distances are to **station points** (MTA station-level), not entrance-level. Proximity to station center is an approximation; entrance-level would differ for large stations.
- **Distance metric:** Straight-line (Euclidean) in projected CRS, not walking-network distance. 500 m straight-line is a common walkability proxy but does not reflect actual walk time.
