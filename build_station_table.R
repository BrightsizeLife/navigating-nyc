# build_station_table.R
# Standalone script to build canonical NYC subway station table
#
# This script:
# 1. Fetches all NYC subway stations from OpenStreetMap
# 2. Parses relation membership to determine true lines served
# 3. Deduplicates nearby nodes into station complexes
# 4. Saves canonical table to CSV and RDS

library(sf)
library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)

# -------------------------
# Config
# -------------------------
APP_USER_AGENT <- "NYC-Walkability-Shiny (contact: youremail@example.com)"
NYC_BBOX <- c(-74.3, 40.45, -73.65, 40.95)
NYC_LINES <- c("A","C","E","B","D","F","M","G","J","Z","L","N","Q","R","W","1","2","3","4","5","6","7","S")

dir.create("data", showWarnings = FALSE)

# -------------------------
# Helpers
# -------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  if (is.character(x) && all(is.na(x))) return(y)
  x
}

parse_ref_tokens <- function(ref) {
  # Split on non-alphanumeric, keep 1-2 char tokens matching NYC lines
  if (is.null(ref) || !nzchar(ref)) return(character(0))
  toks <- toupper(unlist(strsplit(ref, "[^A-Za-z0-9]+")))
  toks <- toks[nzchar(toks) & nchar(toks) <= 2]
  intersect(toks, NYC_LINES)
}

norm_name <- function(x) {
  # Normalize station names for matching
  tolower(trimws(gsub("\\s+", " ", x %||% "")))
}

haversine_m <- function(lon1, lat1, lon2, lat2) {
  # Haversine distance in meters
  R <- 6371000
  toRad <- pi/180
  dlat <- (lat2 - lat1) * toRad
  dlon <- (lon2 - lon1) * toRad
  a <- sin(dlat/2)^2 + cos(lat1*toRad)*cos(lat2*toRad)*sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  R * c
}

# -------------------------
# Overpass Query
# -------------------------

fetch_all_nyc_stations <- function() {
  cat("Fetching all NYC subway stations from OpenStreetMap...\n")

  # Query all subway relations in NYC bbox
  ref_regex <- paste(NYC_LINES, collapse = "|")
  bbox_str <- paste0(NYC_BBOX[2], ",", NYC_BBOX[1], ",", NYC_BBOX[4], ",", NYC_BBOX[3])

  query <- sprintf(
    '[out:json][timeout:90];
rel["route"="subway"]["ref"~"^(%s)$"](%s);
(._; >;);
out tags geom;',
    ref_regex, bbox_str
  )

  resp <- request("https://overpass-api.de/api/interpreter") |>
    req_user_agent(APP_USER_AGENT) |>
    req_url_query(data = query) |>
    req_timeout(120) |>
    req_retry(max_tries = 3, backoff = ~5) |>
    req_perform()

  dat <- resp |> resp_body_string() |> fromJSON(simplifyVector = FALSE)

  if (is.null(dat$elements) || length(dat$elements) == 0) {
    stop("No elements returned from Overpass")
  }

  rels <- keep(dat$elements, ~ .x$type == "relation")
  nodes <- keep(dat$elements, ~ .x$type == "node")

  cat("Relations fetched:", length(rels), "\n")
  cat("Nodes fetched:", length(nodes), "\n\n")

  # Build node→lines mapping
  cat("Building node→lines mapping...\n")
  node_lines <- new.env(parent = emptyenv())

  for (r in rels) {
    refs <- parse_ref_tokens(r$tags$ref %||% "")
    if (length(refs) == 0) next

    members <- r$members %||% list()
    for (m in members) {
      if (!identical(m$type, "node")) next
      role <- m$role %||% ""
      if (!role %in% c("stop", "platform", "stop_entry_only", "stop_exit_only", "")) next

      k <- as.character(m$ref)
      existing <- get0(k, node_lines, inherits = FALSE, ifnotfound = character(0))
      assign(k, unique(c(existing, refs)), envir = node_lines)
    }
  }

  # Build raw node table
  cat("Building raw node table...\n")
  df_raw <- tibble(
    osm_id = vapply(nodes, function(x) as.character(x$id), character(1)),
    lat = vapply(nodes, function(x) x$lat %||% NA_real_, numeric(1)),
    lon = vapply(nodes, function(x) x$lon %||% NA_real_, numeric(1)),
    name = vapply(nodes, function(x) {
      x$tags$name %||% x$tags$`station:name` %||% NA_character_
    }, character(1))
  ) |> drop_na(lat, lon)

  # Attach lines
  df_raw$lines_list <- lapply(df_raw$osm_id, function(id) {
    node_lines[[id]] %||% character(0)
  })
  df_raw$lines <- vapply(df_raw$lines_list, function(l) {
    if (length(l) == 0) "" else paste(sort(l), collapse = ",")
  }, character(1))
  df_raw$name_norm <- norm_name(df_raw$name)

  # Filter to nodes with at least one line
  df_raw <- df_raw |> filter(nchar(lines) > 0)

  cat("Nodes with line membership:", nrow(df_raw), "\n\n")

  # Deduplicate into complexes
  cat("Deduplicating into station complexes...\n")
  sf_raw <- st_as_sf(df_raw, coords = c("lon", "lat"), crs = 4326)

  # Compute pairwise distances (geodetic)
  coords <- st_coordinates(sf_raw)
  n <- nrow(sf_raw)
  cluster_id <- seq_len(n)
  tol_m <- 35

  # Greedy clustering: within distance + same name
  for (i in seq_len(n)) {
    if (i %% 100 == 0) cat("  Processing node", i, "/", n, "\n")

    for (j in seq_len(i - 1)) {
      if (cluster_id[i] == cluster_id[j]) next  # already same cluster

      # Check distance
      d <- haversine_m(coords[i,1], coords[i,2], coords[j,1], coords[j,2])
      if (d >= tol_m) next

      # Check name match
      if (!identical(sf_raw$name_norm[i], sf_raw$name_norm[j])) next
      if (is.na(sf_raw$name_norm[i])) next  # don't merge unknowns

      # Merge clusters
      old_cluster <- cluster_id[i]
      new_cluster <- cluster_id[j]
      cluster_id[cluster_id == old_cluster] <- new_cluster
    }
  }

  # Group into complexes
  sf_canonical <- sf_raw |>
    mutate(cluster_id = cluster_id) |>
    group_by(cluster_id) |>
    summarise(
      station_id = paste0("complex_", first(cluster_id)),
      name = {
        names_vec <- name[!is.na(name)]
        if (length(names_vec) > 0) first(names_vec) else "Unknown"
      },
      lines = paste(sort(unique(unlist(lines_list))), collapse = ","),
      n_merged = n(),
      .groups = "drop"
    ) |>
    select(-cluster_id)

  coords_final <- st_coordinates(sf_canonical)
  sf_canonical$lon <- coords_final[,1]
  sf_canonical$lat <- coords_final[,2]

  cat("\n=== Summary ===\n")
  cat("Raw nodes:", nrow(sf_raw), "\n")
  cat("Station complexes:", nrow(sf_canonical), "\n")
  cat("Nodes merged:", nrow(sf_raw) - nrow(sf_canonical), "\n\n")

  # Show top merged
  top_merged <- sf_canonical |>
    st_drop_geometry() |>
    arrange(desc(n_merged)) |>
    head(10)

  cat("Top 10 complexes by n_merged:\n")
  print(as.data.frame(top_merged[, c("name", "lines", "n_merged")]), row.names = FALSE)
  cat("\n")

  sf_canonical
}

# -------------------------
# Main
# -------------------------

cat("\n")
cat("==============================================\n")
cat("NYC Subway Station Table Builder\n")
cat("==============================================\n\n")

stations <- fetch_all_nyc_stations()

# Save as RDS (with geometry)
rds_path <- "data/nyc_stations_canonical.rds"
saveRDS(stations, rds_path)
cat("Saved RDS:", rds_path, "\n")

# Save as CSV (without geometry)
csv_data <- stations |>
  st_drop_geometry() |>
  select(station_id, name, lines, lon, lat, n_merged)

csv_path <- "data/nyc_stations_canonical.csv"
write.csv(csv_data, csv_path, row.names = FALSE)
cat("Saved CSV:", csv_path, "\n")

cat("\nDone! ✓\n\n")

# Print preview
cat("Preview (first 20 rows):\n")
print(head(as.data.frame(csv_data), 20))
