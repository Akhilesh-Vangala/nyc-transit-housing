CRS: WGS84 (EPSG:4326) for ingestion; NAD83 New York Long Island (EPSG:32118) for buffers and distances. Buffers and distance-to-nearest computed in meters.

Building coordinates: HPD latitude/longitude (in NYC bbox) → latitude_internal/longitude_internal (tax-lot centroid) → borough centroid. One row per (building_id, project_id); dedupe by max total_units.

Transit geometry: Distances to station points (MTA station-level). Optional: subway entrances (config USE_SUBWAY_ENTRANCES). Proximity to station center; entrance-level differs for large stations.

Distance metric: Straight-line (Euclidean) in projected CRS, not walking-network distance.

NTA: Neighborhood Tabulation Areas (NYC DCP). Buildings joined to NTA by point-in-polygon when NTA GeoJSON present. Aggregation and choropleth use units_by_nta_500m (pct within 500 m).

Validation: coord_source_validation.json (pct_coords_hpd, pct_coords_taxlot, pct_coords_borough_fallback); validation_*.json (row counts, bbox, critical_fail). Pipeline continues if fallback share exceeds threshold; check outputs/validation. Sensitivity: sensitivity_by_coord_source.csv (hpd_only, hpd_plus_taxlot, all).
