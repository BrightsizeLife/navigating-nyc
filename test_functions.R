# test_functions.R
# Test the new nearest station functions

library(sf)
library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)

# Source the helper functions from app.R
source("app.R", local = TRUE)

cat("\n========================================\n")
cat("Testing Nearest Stations Functions\n")
cat("========================================\n\n")

# Test address: Times Square (hardcoded coordinates to avoid Nominatim rate limits)
test_address <- "Times Square, New York, NY"
test_lines <- c("A", "C", "L")

cat("Test Address:", test_address, "\n")
cat("Selected Lines:", paste(test_lines, collapse = ", "), "\n\n")

# Step 1: Use hardcoded coordinates for Times Square
cat("Step 1: Using hardcoded coordinates for Times Square...\n")
geo <- tibble::tibble(
  lat = 40.758,
  lon = -73.9855,
  display_name = "Times Square, Manhattan, New York, NY"
)
cat("  Coordinates:", geo$lon, ",", geo$lat, "\n")
cat("  Display Name:", geo$display_name, "\n\n")

pt <- st_as_sf(geo, coords = c("lon", "lat"), crs = 4326)

# Step 2: Fetch stations for selected lines
cat("Step 2: Fetching stations for lines", paste(test_lines, collapse = ", "), "...\n")
stations <- fetch_osm_stops_for_lines(test_lines, force = TRUE)
cat("  Total stations fetched:", nrow(stations), "\n\n")

if (nrow(stations) > 0) {
  cat("  Sample of stations:\n")
  sample_stations <- stations[1:min(5, nrow(stations)), ]
  coords <- st_coordinates(sample_stations)
  print(tibble::tibble(
    name = sample_stations$name,
    lines = sample_stations$lines,
    lon = round(coords[,1], 4),
    lat = round(coords[,2], 4)
  ))
  cat("\n")
}

# Step 3: Test nearest_stations_by_line
cat("========================================\n")
cat("TABLE 1: Nearest 3 Stations by Line\n")
cat("========================================\n")
table1 <- nearest_stations_by_line(pt, stations, test_lines, n = 3)
if (nrow(table1) > 0) {
  print(table1 |> select(line, name, lines_served, walk_min, distance_m))
} else {
  cat("No results\n")
}
cat("\n")

# Step 4: Test nearest_stations_overall
cat("========================================\n")
cat("TABLE 2: Nearest 3 Stations Overall\n")
cat("========================================\n")
table2 <- nearest_stations_overall(pt, stations, n = 3)
if (nrow(table2) > 0) {
  print(table2 |> select(name, lines_served, walk_min, distance_m))
} else {
  cat("No results\n")
}
cat("\n")

# Step 5: Summary stats
cat("========================================\n")
cat("Summary Statistics\n")
cat("========================================\n")
cat("Table 1 (by line):\n")
cat("  Total rows:", nrow(table1), "\n")
cat("  Unique stations:", length(unique(table1$name)), "\n")
cat("  Walk time range:", min(table1$walk_min, na.rm = TRUE), "-", max(table1$walk_min, na.rm = TRUE), "min\n\n")

cat("Table 2 (overall):\n")
cat("  Total rows:", nrow(table2), "\n")
cat("  Walk time range:", min(table2$walk_min, na.rm = TRUE), "-", max(table2$walk_min, na.rm = TRUE), "min\n\n")

cat("Test complete!\n")
