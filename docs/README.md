# navigating nyc — static web page

A no-Shiny version of `app.R`, presented as a Cheap Sensationalism data piece in light
mode. Same four views, same maths, but it runs entirely in the browser: no R process,
no server, no API keys, no build step.

```
docs/
├── index.html              markup for the whole page (CSP + privacy notes live here)
├── assets/
│   ├── app.js              UI, maps, tables, network calls
│   ├── geo.js              pure distance/travel-time maths (also runs in Node)
│   ├── colors_and_type.css canonical Cheap Sensationalism tokens (fonts self-hosted)
│   ├── cs-light.css        documented light-mode derivation of those tokens
│   └── styles.css          layout + components, built on the tokens
├── data/stations.js        493 stations, generated from ../data/mta_stations.csv
└── vendor/
    ├── leaflet/            Leaflet 1.9.4, vendored (BSD-2-Clause, LICENSE included)
    └── fonts/              Space Mono woff2, self-hosted (SIL OFL 1.1, LICENSE included)
```

## Design

The page runs in the design system's `.mode-data` (Space Mono chrome, data-signal
accents) with a light-mode token layer derived in `assets/cs-light.css`: the canonical
dark ground and warm off-white text swap roles, fills keep canonical values, and text
gets darker `-ink` companions that all measure ≥ 4.5:1 (WCAG AA) against their
backgrounds. Two color families are deliberately not brand tokens: official MTA route
colors and the magma walk-time ramp — they carry outside semantics.

## Privacy

No cookies, no local storage, no analytics, no third-party fonts. A `Content-Security-
Policy` meta tag pins scripts/styles/fonts to the page's own origin and network calls to
exactly three free services: CARTO (tiles), Photon/Nominatim (geocoding, only when
[locate] is pressed), and OSRM (only when the road-network box is ticked). The page never
sends a referrer; API calls send origin-only so the OSM services can identify the app per
their usage policies. The URL hash stores the chosen origin to make views shareable —
that's client-side only, but a shared link shares the address. GitHub Pages cannot set
response headers, which is why the CSP ships as a meta tag (everything but
`frame-ancestors` works that way).

## Running it

Open `docs/index.html` in a browser. That is the whole procedure — station data is loaded
via a plain `<script>` tag rather than `fetch()`, so it works over `file://` too.

To serve it locally instead:

```sh
python3 -m http.server 8000 --directory docs
# → http://localhost:8000
```

## Deploying it

**GitHub Pages (primary, automatic).** `.github/workflows/deploy-pages.yml` publishes
`docs/` on every push to `main` and enables Pages by itself on its first run — merging
is the entire deploy. The site lives at `https://brightsizelife.github.io/navigating-nyc/`
and rolling back is `git revert` plus a push.

**Vercel (optional alternative).** `vercel.json` at the repo root is ready: import the
repo at vercel.com (Hobby tier, free for non-commercial), keep every default, deploy.
It serves the same `docs/` folder with no build, and adds what Pages can't: real
response headers (the CSP moves up a layer and gains `frame-ancestors`, plus `nosniff`
and `Permissions-Policy`) and a preview URL per PR. Both hosts can run at once — the
page is identical static files either way.

Any other static host works the same way — copy `docs/` and you are done.

## Where the data comes from

| Thing | Source | Cost |
|---|---|---|
| Station locations, routes, borough, ADA | `data/mta_stations.csv`, bundled at build time | free, offline |
| Walk / travel times | computed in the browser (`geo.js`) | free, offline |
| Basemap tiles | CARTO Positron over OpenStreetMap | free, no key |
| Address lookup | Photon, falling back to Nominatim | free, no key |
| Optional road-network times | public OSRM instances (FOSSGIS) | free, no key |

Nothing here needs an account, a token or a credit card. The Google Distance Matrix branch
in `app.R` — the one part that required a paid `GOOGLE_MAPS_API_KEY` — has no equivalent
here; the OSRM option below covers the same ground for free.

Both geocoders are volunteer-run and rate-limited. The page calls them once per lookup and
never in a loop, and the four quick-place buttons need no network at all.

## Regenerating the station data

After editing `data/mta_stations.csv`:

```sh
python3 tools/build_stations_data.py
```

It groups rows by station name plus lat/lon rounded to 4 decimals, unions the daytime
routes and averages the coordinates — the same reduction `load_mta_stations()` performs in
`app.R`, which is why both produce 493 stations.

## Tests

```sh
node tools/test_geo.js          # 90 checks on the maths in geo.js (no dependencies)
node tools/e2e_page_test.mjs    # 71 browser checks (needs playwright; see file header)
```

CI (`.github/workflows/checks.yml`) runs the math suite plus a drift check that fails if
`docs/data/stations.js` no longer matches a fresh regeneration from the CSV.

The suite pins the formulas to the ones in `app.R` (Manhattan distance, 1.4 m/s walking,
per-mode speeds, rush-hour multipliers, the three-leg metro model) and asserts that the
optimised raster loop returns exactly what a naive per-pair loop returns.

## Parity with `app.R`

Identical: the Manhattan-distance formula and its mean-latitude longitude term, the 1.4 m/s
walk speed, all five mode speeds and rush-hour multipliers, the three-leg metro model, the
NYC bounding box, the walk-cap and fidelity ranges and defaults, the A/C/L default
selection, the magma colour ramp, the 0.4 raster opacity, and the rule that cells at or past
the cap are drawn transparent.

Deliberately different:

- **Nearest-station tables stay sorted by distance.** `app.R` de-duplicates with
  `group_by(name) |> slice(1)`, which silently re-orders the rows alphabetically, so the
  "3 nearest" tables were not actually in distance order. Here they are.
- **Coverage stats describe the map you are looking at.** `app.R` printed
  `summary()` to the R console *after* clamping every value to the walk cap, which makes
  "% within 15 min" read 100% at a 15-minute cap. This page shows the stats in the UI, takes
  the threshold percentages from uncapped times, and takes the median and spread from only
  the cells inside the cap. Both are labelled on the page, because the bounding box includes
  open water and part of New Jersey.
- **The raster is one image, not thousands of rectangles.** Grid rows are spaced evenly in
  Web Mercator and painted to a canvas, so it drops onto the map with no projection stretch
  and redraws fast enough to make the sliders interactive. `app.R` emitted one Leaflet
  rectangle per cell.
- **The line picker offers every route in the data (24), not a hardcoded 22.** `S` and `SIR`
  are matched exactly, so selecting `S` does not pull in the Staten Island Railway — the
  same thing `app.R`'s `\bS\b` word boundary achieved.
- **Additions with no `app.R` equivalent:** shareable URLs that restore lines, cap, fidelity,
  origin and tab; hover readout of the exact minutes under the cursor; borough and ADA
  columns; client-side sort, filter, paging and CSV export on the reference table; and the
  optional OSRM pass described below.

## The optional OSRM pass

The travel-time tab's straight-line estimates inherit `app.R`'s own caveat: only walking is
really trustworthy. Ticking **Use road network (OSRM)** sends one request to a free public
OSRM instance for real durations to ~80 sampled points, converts each into a
real-over-estimate ratio, and interpolates that ratio field across the grid — one HTTP call
instead of one per cell.

It is off by default and best-effort. If the service is slow, rate-limited or down, the page
says so and keeps the straight-line estimates. It covers walk, bike and car only; the public
instances carry no transit data, so bus and metro always use the estimate model.

## Known limits

- Travel times other than walking are approximations unless the OSRM pass succeeds. Metro
  ignores which lines actually connect to each other, exactly as `app.R` does.
- Coverage percentages are shares of the bounding box, not of New York's land area.
- The page needs JavaScript; the maths runs in the main thread, so the finest fidelity
  setting takes a moment on a slow machine (a busy overlay covers the map while it works).
