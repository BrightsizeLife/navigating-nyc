# navigating-nyc — agent entry notes

## Purpose

Two implementations of one tool: explore how walkable NYC is from the subway.
`app.R` is the original Shiny app; `docs/` is a static web page with the same
four views, styled as a Cheap Sensationalism data piece (light mode). Both read
the same MTA station data.

## Constraints (load-bearing — don't optimize against these)

- **Free-only.** The page uses no paid or keyed service, ever. Tiles (CARTO),
  geocoding (Photon → Nominatim fallback), and routing (public OSRM) are all
  free and keyless. Do not add an API that needs a key or a card.
- **Privacy stance.** No cookies, no storage, no analytics, no third-party
  fonts. The page's CSP (meta tag in `docs/index.html`) pins scripts/styles/
  fonts to self and network calls to exactly three hosts — adding an outbound
  call means updating the CSP *and* the visible privacy note in the sidebar,
  or it will be blocked and dishonest respectively.
- **Everything computes client-side.** `docs/data/stations.js` is generated,
  not fetched; the page must keep working over `file://`.
- **Parity with app.R.** `docs/assets/geo.js` mirrors app.R's formulas
  (Manhattan distance, 1.4 m/s walk, mode speeds, rush multipliers, three-leg
  metro). Deliberate divergences are documented in `docs/README.md` — don't
  add silent ones.
- **Design system.** `docs/assets/colors_and_type.css` is the canonical
  Cheap Sensationalism token file (only edit: self-hosted fonts);
  `cs-light.css` is the documented light-mode derivation. Build on tokens.
  Two color families stay hex on purpose: official MTA route colors and the
  magma ramp — they carry outside semantics.

## Commands

```sh
python3 tools/build_stations_data.py   # regenerate docs/data/stations.js after CSV edits
node tools/test_geo.js                 # 90 math checks (no deps)
node tools/e2e_page_test.mjs           # browser suite (needs playwright + chromium)
npx http-server docs -p 8099           # serve locally (any static server works)
```

## Landmines

- Never hand-edit `docs/data/stations.js` — it's generated; the header says so.
- `docs/vendor/` is vendored third-party code (Leaflet BSD-2, Space Mono OFL,
  licenses included) — upgrade by re-vendoring, not by editing.
- The DOM builder `h()` in `app.js` deliberately has no innerHTML path; keep
  it that way — that's the XSS story for geocoder/URL-hash input.
- e2e assertions track user-facing copy; if you change strings, update
  `tools/e2e_page_test.mjs` in the same commit.
