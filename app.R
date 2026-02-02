# app.R
# NYC Walkability — Subway-focused explorer
# Notes:
# - Station data from MTA static CSV (data/mta_stations.csv) — no API calls for stations.
# - Walk times use Manhattan distance (|Δlat| + |Δlon|) at 1.4 m/s — better for NYC's grid.
# - Address geocoding uses Photon (free, OSM-based, no rate-limit issues).

library(shiny)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(sf)
library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(scales)

options(scipen = 999)

# -------------------------
# Utility
# -------------------------
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  if (is.character(x) && all(is.na(x))) return(y)
  if (is.list(x) && length(x) == 0) return(y)
  x
}

# -------------------------
# Config
# -------------------------
APP_USER_AGENT <- "NYC-Walkability-Shiny/1.0"
NYC_BBOX <- c(-74.3, 40.45, -73.65, 40.95) # (min lon, min lat, max lon, max lat)
DEFAULT_LINES <- c("A","C","E","B","D","F","M","G","J","Z","L","N","Q","R","W","1","2","3","4","5","6","7","S")

# -------------------------
# Helpers
# -------------------------

http_get_json <- function(url, query = list()) {
  req <- request(url) |>
    req_user_agent(APP_USER_AGENT) |>
    req_url_query(!!!query) |>
    req_timeout(60)
  resp <- req_perform(req)
  resp |>
    resp_body_string() |>
    fromJSON(simplifyVector = FALSE)
}

load_mta_stations <- function(path = "data/mta_stations.csv") {
  raw <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)

  stations <- raw |>
    transmute(
      name      = `Stop Name`,
      lines     = gsub(" ", ",", `Daytime Routes`),
      latitude  = `GTFS Latitude`,
      longitude = `GTFS Longitude`
    ) |>
    filter(!is.na(latitude), !is.na(longitude), nchar(name) > 0) |>
    mutate(
      lat_round = round(latitude, 4),
      lon_round = round(longitude, 4)
    ) |>
    group_by(name, lat_round, lon_round) |>
    summarise(
      lines     = paste(sort(unique(unlist(strsplit(lines, ",")))), collapse = ","),
      latitude  = mean(latitude),
      longitude = mean(longitude),
      .groups   = "drop"
    ) |>
    select(-lat_round, -lon_round)

  st_as_sf(stations, coords = c("longitude", "latitude"), crs = 4326)
}

# Load all MTA stations once at app startup
ALL_STATIONS_SF <- load_mta_stations()

# Manhattan distance in meters (|Δlat| + |Δlon| in meters).
# Better walk estimate for NYC's grid than straight-line haversine.
manhattan_m <- function(lon1, lat1, lon2, lat2) {
  m_per_deg_lat <- 111320
  avg_lat_rad <- (lat1 + lat2) / 2 * pi / 180
  m_per_deg_lon <- 111320 * cos(avg_lat_rad)
  abs(lat2 - lat1) * m_per_deg_lat + abs(lon2 - lon1) * m_per_deg_lon
}

walk_minutes <- function(meters, m_per_s = 1.4) {
  meters / m_per_s / 60
}

# Simple grid over NYC for heatmap (lon/lat grid)
make_grid_points <- function(bbox = NYC_BBOX, step_deg = 0.004) {
  # ~0.004 deg ~ 445 m in lat; this is a decent default resolution
  lons <- seq(bbox[1], bbox[3], by = step_deg)
  lats <- seq(bbox[2], bbox[4], by = step_deg)
  expand.grid(lon = lons, lat = lats) |>
    st_as_sf(coords = c("lon","lat"), crs = 4326)
}

# Photon geocoder (free, OSM data, no rate-limit issues for single calls)
geocode_address <- function(address) {
  if (is.null(address) || !nzchar(address)) return(NULL)
  res <- http_get_json(
    "https://photon.komoot.io/api/",
    query = list(q = address, limit = 1)
  )
  feats <- res$features
  if (is.null(feats) || length(feats) == 0) return(NULL)
  coords <- feats[[1]]$geometry$coordinates  # [lon, lat]
  props  <- feats[[1]]$properties
  label  <- paste(c(props$name, props$district, props$city, props$state), collapse = ", ")
  tibble::tibble(
    lat = coords[[2]],
    lon = coords[[1]],
    display_name = if (nzchar(label)) label else address
  )
}

nearest_n_stations <- function(pt_sf, stations_sf, n = 5) {
  if (nrow(stations_sf) == 0) return(tibble::tibble())
  pt <- st_coordinates(pt_sf)[1,]
  s_coords <- st_coordinates(stations_sf)
  d_m <- manhattan_m(pt[1], pt[2], s_coords[,1], s_coords[,2])
  ix <- order(d_m)[seq_len(min(n, length(d_m)))]
  tibble::tibble(
    name = stations_sf$name[ix] %||% NA_character_,
    lon = s_coords[ix,1],
    lat = s_coords[ix,2],
    distance_m = d_m[ix],
    walk_min = round(walk_minutes(d_m[ix]), 1)
  )
}

# NEW: Find n nearest stations for each selected line
nearest_stations_by_line <- function(pt_sf, stations_sf, selected_lines, n = 3) {
  if (nrow(stations_sf) == 0 || length(selected_lines) == 0) {
    return(tibble::tibble())
  }

  pt <- st_coordinates(pt_sf)[1,]
  results <- list()

  for (line in selected_lines) {
    # Filter stations that serve this line
    line_stations <- stations_sf |>
      filter(grepl(paste0("\\b", line, "\\b"), lines))

    if (nrow(line_stations) == 0) next

    # Calculate distances
    s_coords <- st_coordinates(line_stations)
    d_m <- manhattan_m(pt[1], pt[2], s_coords[,1], s_coords[,2])
    ix <- order(d_m)[seq_len(min(n, length(d_m)))]

    results[[line]] <- tibble::tibble(
      line = line,
      name = line_stations$name[ix] %||% NA_character_,
      lines_served = line_stations$lines[ix],
      lon = s_coords[ix,1],
      lat = s_coords[ix,2],
      distance_m = d_m[ix],
      walk_min = round(walk_minutes(d_m[ix]), 1)
    ) |>
      group_by(line, name) |>
      slice(1) |>  # Remove duplicate station names per line
      ungroup()
  }

  bind_rows(results)
}

# NEW: Find n nearest stations overall (any line)
nearest_stations_overall <- function(pt_sf, stations_sf, n = 3) {
  if (nrow(stations_sf) == 0) return(tibble::tibble())

  pt <- st_coordinates(pt_sf)[1,]
  s_coords <- st_coordinates(stations_sf)
  d_m <- manhattan_m(pt[1], pt[2], s_coords[,1], s_coords[,2])
  ix <- order(d_m)[seq_len(min(n, length(d_m)))]

  tibble::tibble(
    name = stations_sf$name[ix] %||% NA_character_,
    lines_served = stations_sf$lines[ix],
    lon = s_coords[ix,1],
    lat = s_coords[ix,2],
    distance_m = d_m[ix],
    walk_min = round(walk_minutes(d_m[ix]), 1)
  ) |>
    group_by(name) |>
    slice(1) |>  # Remove duplicate station names
    ungroup()
}

# Find n nearest stations on lines the user did NOT select (discovery feature)
nearest_stations_from_unselected <- function(pt_sf, all_stations_sf, selected_lines, n = 3) {
  if (nrow(all_stations_sf) == 0 || length(selected_lines) == 0) return(tibble::tibble())

  pattern <- paste0("\\b(", paste(selected_lines, collapse = "|"), ")\\b")
  unselected_stations <- all_stations_sf |> filter(!grepl(pattern, lines))

  if (nrow(unselected_stations) == 0) return(tibble::tibble())

  nearest_stations_overall(pt_sf, unselected_stations, n = n)
}

# -------------------------
# UI
# -------------------------
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  tags$head(tags$title("NYC Walkability • Subway-focused")),
  layout_sidebar(
    sidebar = sidebar(
      h4("NYC Walkability"),
      p("Pick subway lines and explore walkability by foot. Station data from MTA; address geocoding via Photon."),
      hr(),
      h5("Select Subway Lines"),
      selectizeInput(
        "lines", "Subway lines", choices = DEFAULT_LINES,
        selected = c("A","C","L"), multiple = TRUE, options = list(plugins = list("remove_button"))
      ),
      hr(),
      h5("Find Nearest Stations"),
      p(style = "font-size: 11px; color: #666;",
        "Click a test button below, OR type an address and click 'Find Nearest Stations'."),
      actionButton("test_times_sq", "Test: Times Square", style = "font-size: 11px; padding: 3px 8px;"),
      actionButton("test_chelsea", "Test: Chelsea Market", style = "font-size: 11px; padding: 3px 8px; margin-left: 5px;"),
      br(), br(),
      textInput("addr", "Or enter custom address", placeholder = "e.g., Times Square, New York, NY"),
      actionButton("geocode", "Find Nearest Stations", icon = icon("magnifying-glass"),
                   style = "width: 100%; margin-top: 5px;"),
      hr(),
      h5("Visualization Controls"),
      sliderInput("walkCap", "Walk time cap (minutes)", min = 3, max = 30, value = 15, step = 1),
      div(
        sliderInput("gridRes", "Fidelity", min = 0.002, max = 0.01, value = 0.004, step = 0.001),
        style = "position: relative;",
        tags$div(style = "display: flex; justify-content: space-between; margin-top: -10px; font-size: 11px; color: #666;",
                 tags$span("High (slower)"),
                 tags$span("Low (faster)"))
      )
    ),
    card(
      navset_tab(
        nav_panel("Walkability overview",
            p("Continuous raster showing walk time to nearest station. Dark purple = close, bright yellow = far. Areas beyond the walk cap are transparent."),
            leafletOutput("heatmap", height = 600)
        ),
        nav_panel("Nearest Stations",
            p("Enter an address in the sidebar to see nearest stations by selected lines and overall."),
            p(style = "font-size: 11px; color: #888; font-style: italic;",
              "Walk times use Manhattan distance (\u0394lat + \u0394lon) at 1.4 m/s (~3.1 mph). ",
              "This approximates walking on NYC's street grid. Actual times may vary with route, traffic lights, etc. ",
              "Stations beyond your walk time cap are excluded."),
            uiOutput("address_display"),
            hr(),
            h5("Table 1: Nearest Stations by Line"),
            p(style = "font-size: 12px; color: #666;",
              "3 nearest stations for each selected subway line"),
            tableOutput("table_by_line"),
            hr(),
            h5("Table 2: Nearest Stations Overall"),
            p(style = "font-size: 12px; color: #666;",
              "3 nearest stations regardless of line"),
            tableOutput("table_overall"),
            hr(),
            h5("Table 3: Nearest Stations on Other Lines"),
            p(style = "font-size: 12px; color: #666;",
              "3 nearest stations on lines you haven't selected (discover new routes)"),
            tableOutput("table_unselected")
        ),
        nav_panel("Reference Table",
            p("All loaded stations with distances from your address, sorted by distance."),
            uiOutput("reference_address_display"),
            hr(),
            DT::dataTableOutput("reference_table")
        )
      )
    )
  )
)

# -------------------------
# Server
# -------------------------
server <- function(input, output, session) {
  # Filter ALL_STATIONS_SF to only stations serving at least one selected line
  stations_rv <- reactive({
    sel <- input$lines
    if (length(sel) == 0) return(st_sf(name = character(), lines = character(), geometry = st_sfc(crs = 4326)))
    pattern <- paste0("\\b(", paste(sel, collapse = "|"), ")\\b")
    ALL_STATIONS_SF |> filter(grepl(pattern, lines))
  })
  
  # ----------------- Heatmap tab -----------------
  output$heatmap <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE, preferCanvas = TRUE)) |>
      addProviderTiles("CartoDB.Positron") |>
      fitBounds(lng1 = NYC_BBOX[1], lat1 = NYC_BBOX[2], lng2 = NYC_BBOX[3], lat2 = NYC_BBOX[4])
  })
  
  observe({
    req(nrow(stations_rv()) > 0L)

    grid <- make_grid_points(NYC_BBOX, step_deg = input$gridRes)

    # Guardrail: prevent UI freeze on huge grids
    if (nrow(grid) > 30000) {
      showNotification("Grid too large (>30k points); increase gridRes to avoid freeze.",
                       type = "warning", duration = 6)
      return()
    }

    s_coords <- st_coordinates(stations_rv())
    g_coords <- st_coordinates(grid)

    # For each grid point, compute distance to nearest station
    nearest_m <- sapply(seq_len(nrow(g_coords)), function(i) {
      d <- manhattan_m(g_coords[i,1], g_coords[i,2], s_coords[,1], s_coords[,2])
      min(d)
    })

    minutes <- pmin(walk_minutes(nearest_m), input$walkCap)

    # Console diagnostics
    cat("\n=== Heatmap diagnostics ===\n")
    cat("Grid points:", nrow(grid), "\n")
    print(summary(minutes))
    cat("SD:", round(sd(minutes), 2), "\n")
    cat("% within 5 min:", round(100 * mean(minutes <= 5), 1), "%\n")
    cat("% within 10 min:", round(100 * mean(minutes <= 10), 1), "%\n")
    cat("% within 15 min:", round(100 * mean(minutes <= 15), 1), "%\n")
    cat("% within 20 min:", round(100 * mean(minutes <= 20), 1), "%\n")

    # Low variance warning
    if (sd(minutes) < 1) {
      cat("WARNING: Heatmap low variance; try smaller walkCap or finer gridRes.\n")
    }
    cat("===========================\n\n")

    # Calculate grid cell boundaries for continuous raster effect
    half_step <- input$gridRes / 2
    df <- tibble::tibble(
      lon = g_coords[,1],
      lat = g_coords[,2],
      minutes = as.numeric(minutes),
      lng1 = lon - half_step,
      lat1 = lat - half_step,
      lng2 = lon + half_step,
      lat2 = lat + half_step
    )

    # Color palette: dark = near (low minutes), light = far (high minutes)
    # Magma: dark purple (low) → bright yellow (high)
    pal <- colorNumeric(
      palette = "magma",
      domain = c(0, input$walkCap),
      reverse = FALSE  # Don't reverse: dark=near, light=far
    )

    # Pre-compute colors for each grid cell
    df$color <- pal(df$minutes)

    # Make cells at walkCap transparent (outside walk zone)
    # For cells within zone: use transparency so streets show through
    df$opacity <- ifelse(df$minutes >= input$walkCap, 0, 0.4)

    # Legend HTML with color scale
    legend_txt <- paste0(
      "<div style='background: rgba(255,255,255,0.95); padding: 10px; border-radius: 4px; font-size: 12px;'>",
      "<b>Walk time (min)</b><br/>",
      "Min: ", round(min(minutes), 1), "<br/>",
      "Median: ", round(median(minutes), 1), "<br/>",
      "Max: ", round(max(minutes), 1), "<br/>",
      "<div style='margin-top: 5px;'>",
      "<span style='background: #000004; padding: 2px 8px; color: white;'>█</span> Near (0 min)<br/>",
      "<span style='background: #B63679; padding: 2px 8px; color: white;'>█</span> Mid<br/>",
      "<span style='background: #FCFDBF; padding: 2px 8px;'>█</span> Far (", input$walkCap, " min)",
      "</div></div>"
    )

    leafletProxy("heatmap") |>
      clearShapes() |>
      clearMarkers() |>
      clearControls() |>
      addRectangles(
        lng1 = df$lng1, lat1 = df$lat1,
        lng2 = df$lng2, lat2 = df$lat2,
        fillColor = df$color,
        fillOpacity = df$opacity,
        stroke = FALSE,
        weight = 0
      ) |>
      addCircleMarkers(
        data = stations_rv(), radius = 3, color = "#00FFFF", weight = 2,
        fillColor = "#0080FF", opacity = 1, fillOpacity = 0.9,
        popup = ~paste0("<b>", htmltools::htmlEscape(name %||% "Station"), "</b><br/>Lines: ",
                        htmltools::htmlEscape(gsub(",", ", ", lines)))
      ) |>
      addControl(html = legend_txt, position = "bottomleft")
  })
  
  # ----------------- Nearest Stations tab -----------------
  geocoded_address <- reactiveVal(NULL)

  # Test address buttons (hardcoded coords, no API call needed)
  observeEvent(input$test_times_sq, {
    geocoded_address(tibble::tibble(
      lat = 40.758,
      lon = -73.9855,
      display_name = "Times Square, Manhattan, NY (test address)"
    ))
    showNotification("Using test address: Times Square", type = "message", duration = 3)
  })

  observeEvent(input$test_chelsea, {
    geocoded_address(tibble::tibble(
      lat = 40.7425,
      lon = -74.0061,
      display_name = "Chelsea Market, 75 9th Ave, Manhattan, NY (test address)"
    ))
    showNotification("Using test address: Chelsea Market", type = "message", duration = 3)
  })

  observeEvent(input$geocode, {
    req(input$addr, nchar(input$addr) > 3)

    # Single API call: address → lat/lon. Everything else is pure R math.
    geo <- tryCatch({
      geocode_address(input$addr)
    }, error = function(e) {
      showNotification(
        paste("Geocoding failed:", e$message, "— try a test button instead."),
        type = "error", duration = 8
      )
      return(NULL)
    })

    if (is.null(geo)) return()

    geocoded_address(geo)
    showNotification(paste("Found:", geo$display_name), type = "message", duration = 3)
  }, ignoreInit = TRUE)

  # Display the geocoded address
  output$address_display <- renderUI({
    geo <- geocoded_address()
    if (is.null(geo)) {
      return(p(style = "color: #666; font-style: italic;", "No address entered yet. Enter an address in the sidebar and click 'Find Nearest Stations'."))
    }
    tags$div(
      style = "background: #e8f4f8; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
      tags$strong("Address: "), geo$display_name, tags$br(),
      tags$small(paste0("Coordinates: ", round(geo$lat, 4), ", ", round(geo$lon, 4)))
    )
  })

  # Table 1: Nearest stations by line
  output$table_by_line <- renderTable({
    geo <- geocoded_address()
    if (is.null(geo)) return(NULL)

    stns <- stations_rv()
    if (nrow(stns) == 0) return(NULL)

    pt <- st_as_sf(geo, coords = c("lon","lat"), crs = 4326)
    tbl <- nearest_stations_by_line(pt, stns, input$lines, n = 3) |>
      filter(walk_min <= input$walkCap)

    if (nrow(tbl) == 0) {
      return(data.frame(Message = "No stations within walk time cap"))
    }

    tibble::tibble(
      Line = tbl$line,
      Station = tbl$name,
      `Lines Served` = tbl$lines_served,
      `Walk Time (min)` = tbl$walk_min,
      `Distance (m)` = round(tbl$distance_m)
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # Table 2: Nearest stations overall
  output$table_overall <- renderTable({
    geo <- geocoded_address()
    if (is.null(geo)) return(NULL)

    stns <- stations_rv()
    if (nrow(stns) == 0) return(NULL)

    pt <- st_as_sf(geo, coords = c("lon","lat"), crs = 4326)
    tbl <- nearest_stations_overall(pt, stns, n = 3) |>
      filter(walk_min <= input$walkCap)

    if (nrow(tbl) == 0) {
      return(data.frame(Message = "No stations within walk time cap"))
    }

    tibble::tibble(
      Station = tbl$name,
      `Lines Served` = tbl$lines_served,
      `Walk Time (min)` = tbl$walk_min,
      `Distance (m)` = round(tbl$distance_m)
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # Table 3: Nearest stations on lines user did NOT select
  output$table_unselected <- renderTable({
    geo <- geocoded_address()
    if (is.null(geo)) return(NULL)

    pt <- st_as_sf(geo, coords = c("lon", "lat"), crs = 4326)
    tbl <- nearest_stations_from_unselected(pt, ALL_STATIONS_SF, input$lines, n = 3) |>
      filter(walk_min <= input$walkCap)

    if (nrow(tbl) == 0) {
      return(data.frame(Message = "No stations on other lines within walk time cap"))
    }

    tibble::tibble(
      Station = tbl$name,
      `Lines Served` = tbl$lines_served,
      `Walk Time (min)` = tbl$walk_min,
      `Distance (m)` = round(tbl$distance_m)
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # ----------------- Reference Table tab -----------------
  output$reference_address_display <- renderUI({
    geo <- geocoded_address()
    if (is.null(geo)) {
      return(p(style = "color: #666; font-style: italic;", "No address entered yet. Enter an address in the sidebar and click 'Find Nearest Stations'."))
    }
    tags$div(
      style = "background: #e8f4f8; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
      tags$strong("Address: "), geo$display_name, tags$br(),
      tags$small(paste0("Coordinates: ", round(geo$lat, 4), ", ", round(geo$lon, 4)))
    )
  })

  output$reference_table <- DT::renderDataTable({
    geo <- geocoded_address()
    if (is.null(geo)) return(NULL)

    stns <- stations_rv()
    if (nrow(stns) == 0) return(NULL)

    # Calculate distances from address to all stations (no API call)
    pt <- st_coordinates(st_as_sf(geo, coords = c("lon", "lat"), crs = 4326))[1,]
    s_coords <- st_coordinates(stns)
    distances_m <- manhattan_m(pt[1], pt[2], s_coords[,1], s_coords[,2])
    walk_min <- walk_minutes(distances_m)

    # Build reference table with all stations sorted by distance
    ref_tbl <- tibble::tibble(
      Station = stns$name %||% NA_character_,
      Latitude = round(s_coords[,2], 5),
      Longitude = round(s_coords[,1], 5),
      `Distance (m)` = round(distances_m),
      `Walk Time (min)` = round(walk_min, 1),
      `Lines Served` = stns$lines
    ) |>
      arrange(`Distance (m)`)

    DT::datatable(
      ref_tbl,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        order = list(list(3, 'asc'))  # Sort by Distance column
      ),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)

