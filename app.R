# app.R
# NYC Walkability — Subway-focused explorer (free APIs: Overpass + Nominatim)
# Notes:
# - Line selection uses OSM 'route=subway' relations filtered by ref (e.g., A, C, L).
# - Walk times are approximated (straight-line distance / 1.4 m/s) to avoid paid routing APIs.
# - Data are cached under ./cache to reduce API calls.
# - Respect OSM/Nominatim usage policies: include a descriptive user agent + email.

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
dir.create("cache", showWarnings = FALSE)

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
APP_USER_AGENT <- "NYC-Walkability-Shiny (contact: youremail@example.com)"
NYC_BBOX <- c(-74.3, 40.45, -73.65, 40.95) # (min lon, min lat, max lon, max lat)
DEFAULT_LINES <- c("A","C","E","B","D","F","M","G","J","Z","L","N","Q","R","W","1","2","3","4","5","6","7","S")

# -------------------------
# Helpers
# -------------------------

bbox_to_overpass <- function(bbox) {
  # Overpass expects south,west,north,east => minlat, minlon, maxlat, maxlon
  paste0(bbox[2], ",", bbox[1], ",", bbox[4], ",", bbox[3])
}

overpass_query_for_lines <- function(lines, bbox = NYC_BBOX) {
  # Build an Overpass QL query that fetches route=subway relations
  # whose 'ref' matches any of the selected line letters/numbers,
  # then pulls member nodes with role 'stop'/'platform' (stations).
  # We also pull 'station' nodes just in case (belt & suspenders).
  ref_regex <- paste(lines, collapse = "|")
  bbox_str <- bbox_to_overpass(bbox)

  q <- sprintf(
    '[out:json][timeout:60];
rel["route"="subway"]["ref"~"^(%s)$"](%s);
(
  node(r:"stop");
  node(r:"platform");
);
out tags geom;',
    ref_regex, bbox_str
  )
  q
}

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

fetch_osm_stops_for_lines <- function(lines, force = FALSE) {
  key <- paste0("cache/overpass_stops_", digest::digest(sort(lines)), ".rds")
  if (file.exists(key) && !force) {
    return(readRDS(key))
  }

  # NEW: Fetch each line separately to track which stations belong to which line
  all_stations <- list()

  for (line in lines) {
    q <- overpass_query_for_lines(line)
    q <- paste(q, collapse = "\n")

    tryCatch({
      resp <- request("https://overpass-api.de/api/interpreter") |>
        req_user_agent(APP_USER_AGENT) |>
        req_url_query(data = q) |>
        req_timeout(90) |>
        req_retry(max_tries = 3, backoff = ~5) |>
        req_perform()

      dat <- resp |> resp_body_string() |> fromJSON(simplifyVector = FALSE)

      if (!is.null(dat$elements) && length(dat$elements) > 0) {
        nodes <- purrr::keep(dat$elements, ~ .x$type == "node")
        if (length(nodes) > 0) {
          df <- tibble::tibble(
            osm_id = vapply(nodes, function(x) as.character(x$id), character(1)),
            lat = vapply(nodes, function(x) x$lat %||% NA_real_, numeric(1)),
            lon = vapply(nodes, function(x) x$lon %||% NA_real_, numeric(1)),
            name = vapply(nodes, function(x) x$tags$name %||% NA_character_, character(1)),
            line = line  # Track which line this station belongs to
          ) |> tidyr::drop_na(lat, lon)

          all_stations[[line]] <- df
        }
      }
      Sys.sleep(0.5)  # Be nice to Overpass API
    }, error = function(e) {
      warning(paste("Failed to fetch line", line, ":", e$message))
    })
  }

  if (length(all_stations) == 0) {
    return(sf::st_sf(osm_id = character(), name = character(), lines = character(),
                     geometry = st_sfc(), crs = 4326))
  }

  # Combine all stations and aggregate lines per station
  combined <- bind_rows(all_stations) |>
    group_by(osm_id, lat, lon, name) |>
    summarise(lines = paste(sort(unique(line)), collapse = ","), .groups = "drop")

  sf <- st_as_sf(combined, coords = c("lon","lat"), crs = 4326)

  # Deduplicate close nodes by rounded coordinate
  coords <- st_coordinates(sf)
  sf <- sf |>
    mutate(lat_round = round(coords[,2], 5),
           lon_round = round(coords[,1], 5)) |>
    distinct(lat_round, lon_round, .keep_all = TRUE) |>
    select(-lat_round, -lon_round)

  saveRDS(sf, key)
  sf
}

# Haversine distance in meters between two lon/lat points
haversine_m <- function(lon1, lat1, lon2, lat2) {
  R <- 6371000
  toRad <- pi/180
  dlat <- (lat2 - lat1) * toRad
  dlon <- (lon2 - lon1) * toRad
  a <- sin(dlat/2)^2 + cos(lat1*toRad)*cos(lat2*toRad)*sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  R * c
}

approx_walk_minutes <- function(meters, m_per_s = 1.4) {
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

# Nominatim geocoding (free). Respect their ToS: add email in UA and throttle if heavy.
geocode_nominatim <- function(address) {
  if (is.null(address) || !nzchar(address)) return(NULL)
  res <- http_get_json(
    "https://nominatim.openstreetmap.org/search",
    query = list(q = address, format = "jsonv2", limit = 1, addressdetails = 0)
  )
  if (length(res) == 0) return(NULL)
  tibble::tibble(
    lat = as.numeric(res[[1]]$lat),
    lon = as.numeric(res[[1]]$lon),
    display_name = res[[1]]$display_name %||% address
  )
}

nearest_n_stations <- function(pt_sf, stations_sf, n = 5) {
  if (nrow(stations_sf) == 0) return(tibble::tibble())
  pt <- st_coordinates(pt_sf)[1,]
  s_coords <- st_coordinates(stations_sf)
  d_m <- haversine_m(pt[1], pt[2], s_coords[,1], s_coords[,2])
  ix <- order(d_m)[seq_len(min(n, length(d_m)))]
  tibble::tibble(
    name = stations_sf$name[ix] %||% NA_character_,
    lon = s_coords[ix,1],
    lat = s_coords[ix,2],
    distance_m = d_m[ix],
    walk_min = round(approx_walk_minutes(d_m[ix]), 1)
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
    d_m <- haversine_m(pt[1], pt[2], s_coords[,1], s_coords[,2])
    ix <- order(d_m)[seq_len(min(n, length(d_m)))]

    results[[line]] <- tibble::tibble(
      line = line,
      name = line_stations$name[ix] %||% NA_character_,
      lines_served = line_stations$lines[ix],
      lon = s_coords[ix,1],
      lat = s_coords[ix,2],
      distance_m = d_m[ix],
      walk_min = round(approx_walk_minutes(d_m[ix]), 1)
    )
  }

  bind_rows(results)
}

# NEW: Find n nearest stations overall (any line)
nearest_stations_overall <- function(pt_sf, stations_sf, n = 3) {
  if (nrow(stations_sf) == 0) return(tibble::tibble())

  pt <- st_coordinates(pt_sf)[1,]
  s_coords <- st_coordinates(stations_sf)
  d_m <- haversine_m(pt[1], pt[2], s_coords[,1], s_coords[,2])
  ix <- order(d_m)[seq_len(min(n, length(d_m)))]

  tibble::tibble(
    name = stations_sf$name[ix] %||% NA_character_,
    lines_served = stations_sf$lines[ix],
    lon = s_coords[ix,1],
    lat = s_coords[ix,2],
    distance_m = d_m[ix],
    walk_min = round(approx_walk_minutes(d_m[ix]), 1)
  )
}

# -------------------------
# UI
# -------------------------
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  tags$head(tags$title("NYC Walkability • Subway-focused")),
  layout_sidebar(
    sidebar = sidebar(
      h4("NYC Walkability (Free-API MVP)"),
      p("Pick subway lines and explore walkability by foot. Data from OpenStreetMap/Overpass + Nominatim."),
      hr(),
      h5("Common controls"),
      selectizeInput(
        "lines", "Subway lines (OSM 'ref')", choices = DEFAULT_LINES,
        selected = c("A","C","L"), multiple = TRUE, options = list(plugins = list("remove_button"))
      ),
      sliderInput("walkCap", "Walk time cap (minutes)", min = 3, max = 30, value = 15, step = 1),
      div(
        sliderInput("gridRes", "Fidelity", min = 0.002, max = 0.01, value = 0.004, step = 0.001),
        style = "position: relative;",
        tags$div(style = "display: flex; justify-content: space-between; margin-top: -10px; font-size: 11px; color: #666;",
                 tags$span("High (slower)"),
                 tags$span("Low (faster)"))
      ),
      actionButton("refresh", "Fetch / Refresh Lines", icon = icon("rotate"))
    ),
    card(
      navset_tab(
        nav_panel("Walkability overview",
            p("Continuous raster showing walk time to nearest station. Dark purple = close, bright yellow = far. Areas beyond the walk cap are transparent."),
            leafletOutput("heatmap", height = 600)
        ),
        nav_panel("Address specificity",
            p("Enter an address; see the 5 nearest stations (by selected lines) and walking times."),
            textInput("addr", "Address (NYC)", placeholder = "e.g., 1 Centre St, New York, NY"),
            actionButton("geocode", "Find stations", icon = icon("magnifying-glass")),
            br(), br(),
            leafletOutput("specific", height = 600),
            br(),
            tableOutput("nearest_tbl")
        )
      )
    )
  )
)

# -------------------------
# Server
# -------------------------
server <- function(input, output, session) {
  lines_rv <- reactiveVal(character())
  stations_rv <- reactiveVal(st_sf(osm_id = character(), name = character(), geometry = st_sfc(crs = 4326)))
  
  fetch_lines <- function() {
    req(length(input$lines) > 0)
    showNotification("Fetching stations for selected lines from Overpass…", type = "message", duration = 4)
    sf_pts <- fetch_osm_stops_for_lines(input$lines)
    if (nrow(sf_pts) == 0) {
      showNotification("No stations found for those lines (OSM). Try different lines.", type = "error", duration = 5)
    }
    stations_rv(sf_pts)
    lines_rv(input$lines)
  }
  
  observeEvent(input$refresh, fetch_lines(), ignoreInit = FALSE)
  
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
      d <- haversine_m(g_coords[i,1], g_coords[i,2], s_coords[,1], s_coords[,2])
      min(d)
    })

    minutes <- pmin(approx_walk_minutes(nearest_m), input$walkCap)

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
                        htmltools::htmlEscape(paste(lines_rv(), collapse = ", ")))
      ) |>
      addControl(html = legend_txt, position = "bottomleft")
  })
  
  # ----------------- Specificity tab -----------------
  observeEvent(input$geocode, {
    req(input$addr, nchar(input$addr) > 3)
    # Geocode
    geo <- geocode_nominatim(input$addr)
    if (is.null(geo)) {
      showNotification("Address not found via Nominatim.", type = "error", duration = 5)
      return()
    }
    pt <- st_as_sf(geo, coords = c("lon","lat"), crs = 4326)
    stns <- stations_rv()
    if (nrow(stns) == 0) {
      showNotification("No stations loaded yet. Click 'Fetch / Refresh Lines' first.", type = "warning", duration = 6)
      return()
    }
    tbl <- nearest_n_stations(pt, stns, n = 5)
    output$nearest_tbl <- renderTable({
      if (nrow(tbl) == 0) return(NULL)
      tibble::tibble(
        Station = tbl$name %||% "Unknown",
        `Walk (min)` = tbl$walk_min,
        `Distance (m)` = round(tbl$distance_m),
        Lon = round(tbl$lon, 5),
        Lat = round(tbl$lat, 5)
      )
    })
    
    # Map
    leafletProxy("specific") |>
      clearMarkers() |>
      clearShapes() |>
      addProviderTiles("CartoDB.Positron") |>
      fitBounds(min(tbl$lon, geo$lon), min(tbl$lat, geo$lat),
                max(tbl$lon, geo$lon), max(tbl$lat, geo$lat)) |>
      addAwesomeMarkers(lng = geo$lon, lat = geo$lat, icon = awesomeIcons(icon = "home", markerColor = "blue"),
                        popup = htmltools::HTML(paste0("<b>Address</b><br/>", htmltools::htmlEscape(geo$display_name)))) |>
      addCircleMarkers(data = st_as_sf(tbl, coords = c("lon","lat"), crs = 4326),
                       radius = 6, color = "#2A9D8F", fillOpacity = 0.9,
                       popup = ~paste0("<b>", htmltools::htmlEscape(name %||% "Station"), "</b><br/>Walk: ",
                                       htmltools::htmlEscape(as.character(walk_min %||% NA)), " min"))
  }, ignoreInit = TRUE)
  
  output$specific <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE, preferCanvas = TRUE)) |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = -73.9851, lat = 40.758, zoom = 12)
  })
}

shinyApp(ui, server)

