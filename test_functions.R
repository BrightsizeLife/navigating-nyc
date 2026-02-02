# test_functions.R
# Test the nearest station functions using static MTA data

library(sf)
library(dplyr)

# Source the helper functions from app.R (loads ALL_STATIONS_SF at top level)
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

# Step 2: Filter stations for selected lines
cat("Step 2: Filtering stations for lines", paste(test_lines, collapse = ", "), "...\n")
cat("  Total stations in MTA dataset:", nrow(ALL_STATIONS_SF), "\n")
pattern <- paste0("\\b(", paste(test_lines, collapse = "|"), ")\\b")
stations <- ALL_STATIONS_SF |> filter(grepl(pattern, lines))
cat("  Stations for selected lines:", nrow(stations), "\n\n")

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

# Step 5: Test nearest_stations_from_unselected
cat("========================================\n")
cat("TABLE 3: Nearest 3 on Unselected Lines\n")
cat("========================================\n")
table3 <- nearest_stations_from_unselected(pt, ALL_STATIONS_SF, test_lines, n = 3)
if (nrow(table3) > 0) {
  print(table3 |> select(name, lines_served, walk_min, distance_m))
} else {
  cat("No results\n")
}
cat("\n")

# Step 6: Summary stats
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

cat("Table 3 (unselected lines):\n")
cat("  Total rows:", nrow(table3), "\n")
cat("  Walk time range:", min(table3$walk_min, na.rm = TRUE), "-", max(table3$walk_min, na.rm = TRUE), "min\n\n")

cat("Test complete!\n")
