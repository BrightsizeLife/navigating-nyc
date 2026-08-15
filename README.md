# navigating-nyc

Two ways to explore how walkable New York City is from the subway, sharing one dataset.

| | Shiny app | Static page |
|---|---|---|
| Entry point | `app.R` | `docs/index.html` |
| Needs | R, Shiny, sf, leaflet | a browser |
| Runs on | shinyapps.io / local R | GitHub Pages, or any static host, or `file://` |
| Docs | — | [`docs/README.md`](docs/README.md) |

Both offer the same four views — a walkability heatmap, nearest-station tables, a
travel-time heatmap, and a station reference table — over the same MTA station data in
`data/mta_stations.csv`, using the same distance and travel-time formulas.

## Running the Shiny app

```r
shiny::runApp()
Rscript test_functions.R    # nearest-station sanity check
```

## Running the static page

```sh
open docs/index.html                        # no server needed
python3 -m http.server 8000 --directory docs # or serve it
node tools/test_geo.js                      # 90 checks on the maths
node tools/e2e_page_test.mjs                # browser suite (needs playwright)
```

The page is styled as a Cheap Sensationalism data piece (light mode); the token
system and its light-mode derivation live in `docs/assets/`.

After editing `data/mta_stations.csv`, regenerate the page's bundled copy with
`python3 tools/build_stations_data.py`. See [`docs/README.md`](docs/README.md) for how the
two versions line up and where they deliberately differ.

## How walk times are calculated

Manhattan distance (|Δlat| + |Δlon|, converted to metres using the mean latitude) at
1.4 m/s — about 3.1 mph. That approximates walking New York's street grid better than a
straight line, but it ignores actual routes, traffic lights and hills.
