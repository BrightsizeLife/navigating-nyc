#!/usr/bin/env node
/* Checks docs/assets/geo.js against the formulas in app.R.
 *
 * The important one is the raster test: walkTimeRaster hoists the cos() term
 * out of its inner loop for speed, and this asserts that shortcut returns
 * exactly what a naive per-pair manhattanM() loop returns.
 *
 * Usage:  node tools/test_geo.js
 */
'use strict';

const path = require('path');
const NYC = require(path.join(__dirname, '..', 'docs', 'assets', 'geo.js'));
const RAW = require(path.join(__dirname, '..', 'docs', 'data', 'stations.js'));

let passed = 0;
const failures = [];

function ok(name, cond, detail) {
  if (cond) { passed++; return; }
  failures.push(detail ? `${name} — ${detail}` : name);
}

function near(name, actual, expected, tol) {
  const t = tol == null ? 1e-9 : tol;
  ok(name, Math.abs(actual - expected) <= t, `expected ${expected}, got ${actual}`);
}

// ---------------------------------------------------------------- distance

near('1 degree of latitude is 111320 m', NYC.manhattanM(-74, 40, -74, 41), 111320, 1e-6);
near('same point is 0 m', NYC.manhattanM(-73.9855, 40.758, -73.9855, 40.758), 0);
ok('distance is symmetric',
  Math.abs(NYC.manhattanM(-74, 40.7, -73.9, 40.8) - NYC.manhattanM(-73.9, 40.8, -74, 40.7)) < 1e-9);

// |Δlon| leg uses cos of the mean latitude, exactly as app.R does.
near('longitude leg uses mean-latitude cosine',
  NYC.manhattanM(-74, 40.7, -73.9, 40.7),
  0.1 * 111320 * Math.cos((40.7 * Math.PI) / 180), 1e-6);

near('walk speed is 1.4 m/s', NYC.walkMinutes(1400), 1400 / 1.4 / 60);

// ------------------------------------------------------- travel estimates

const MODES = { walk: 1.4, bike: 4.5, bus: 3.5, metro: 8.9, car: 6.7 };
Object.keys(MODES).forEach((mode) => {
  near(`${mode} speed`, NYC.travelMinutesEstimate(1000, mode, false), 1000 / MODES[mode] / 60);
});

const RUSH = { walk: 1.0, bike: 1.1, bus: 1.5, metro: 1.2, car: 2.0 };
Object.keys(RUSH).forEach((mode) => {
  near(`${mode} rush-hour multiplier`,
    NYC.travelMinutesEstimate(1000, mode, true),
    (1000 / MODES[mode] / 60) * RUSH[mode], 1e-12);
});

ok('unknown mode falls back to walking speed',
  NYC.travelMinutesEstimate(1000, 'hovercraft', false) === NYC.travelMinutesEstimate(1000, 'walk', false));
near('rush hour shrinks effective car speed', NYC.effectiveSpeed('car', true), 6.7 / 2);

// -------------------------------------------------------------- projection

[40.45, 40.7128, 40.95].forEach((lat) => {
  near(`mercator round-trip at ${lat}`, NYC.mercYToLat(NYC.latToMercY(lat)), lat, 1e-10);
});
ok('mercator y increases with latitude', NYC.latToMercY(40.95) > NYC.latToMercY(40.45));

// ---------------------------------------------------------------- stations

const stations = NYC.parseStations(RAW);
ok('stations loaded', stations.length > 400, `got ${stations.length}`);
ok('every station has coordinates',
  stations.every((s) => isFinite(s.lat) && isFinite(s.lon) && s.name));
ok('every station has at least one route', stations.every((s) => s.routes.length > 0));
ok('station names are de-duplicated per location',
  new Set(stations.map((s) => `${s.name}|${s.lat.toFixed(4)}|${s.lon.toFixed(4)}`)).size === stations.length);

// Route matching is exact, so "S" must not pick up the Staten Island Railway —
// this is what app.R's \bS\b word boundary achieves.
const sLine = NYC.filterByLines(stations, ['S']);
ok('route S excludes SIR stations', sLine.every((s) => s.routes.indexOf('SIR') === -1 || s.routes.indexOf('S') !== -1));
ok('route S matches the shuttles', sLine.length > 0 && sLine.every((s) => s.routes.indexOf('S') !== -1));

const acl = NYC.filterByLines(stations, ['A', 'C', 'L']);
ok('A/C/L filter returns only A, C or L stations',
  acl.length > 50 && acl.every((s) => ['A', 'C', 'L'].some((r) => s.routes.indexOf(r) !== -1)));

const notAcl = NYC.filterExcludingLines(stations, ['A', 'C', 'L']);
ok('selected and unselected partition the network', acl.length + notAcl.length === stations.length);
ok('unselected set excludes A/C/L',
  notAcl.every((s) => !['A', 'C', 'L'].some((r) => s.routes.indexOf(r) !== -1)));
ok('empty selection selects nothing', NYC.filterByLines(stations, []).length === 0);

// ---------------------------------------------------- nearest-N, Times Square

// Same fixture as test_functions.R.
const TIMES_SQ = { lat: 40.758, lon: -73.9855 };
const SELECTED = ['A', 'C', 'L'];

const overall = NYC.nearestOverall(TIMES_SQ, acl, 3);
ok('overall returns 3 rows', overall.length === 3, `got ${overall.length}`);
ok('overall is sorted by distance',
  overall[0].distance_m <= overall[1].distance_m && overall[1].distance_m <= overall[2].distance_m);
ok('overall has no duplicate station names', new Set(overall.map((r) => r.name)).size === overall.length);
ok('nearest A/C/L stop to Times Sq is walkable', overall[0].walk_min < 12,
  `${overall[0].name} at ${overall[0].walk_min} min`);
ok('nearest A/C/L stop actually serves A, C or L',
  ['A', 'C', 'L'].some((r) => overall[0].routes.indexOf(r) !== -1), overall[0].lines);

const byLine = NYC.nearestByLine(TIMES_SQ, acl, SELECTED, 3);
ok('by-line returns 3 rows per selected route', byLine.length === 9, `got ${byLine.length}`);
ok('by-line rows follow the selected order',
  byLine.slice(0, 3).every((r) => r.line === 'A') && byLine.slice(6).every((r) => r.line === 'L'));
ok('every by-line row serves its route', byLine.every((r) => r.routes.indexOf(r.line) !== -1));
SELECTED.forEach((route) => {
  const rows = byLine.filter((r) => r.line === route);
  ok(`route ${route} rows are distinct stations`, new Set(rows.map((r) => r.name)).size === rows.length);
  ok(`route ${route} rows are sorted by distance`,
    rows.every((r, i) => i === 0 || rows[i - 1].distance_m <= r.distance_m));
});

const unselected = NYC.nearestFromUnselected(TIMES_SQ, stations, SELECTED, 3);
ok('unselected returns 3 rows', unselected.length === 3, `got ${unselected.length}`);
ok('unselected rows avoid A/C/L',
  unselected.every((r) => !['A', 'C', 'L'].some((x) => r.routes.indexOf(x) !== -1)));

// walk_min is a rounded view of distance_m, not an independent number.
ok('walk_min matches distance_m',
  overall.every((r) => Math.abs(r.walk_min - Math.round(NYC.walkMinutes(r.distance_m) * 10) / 10) < 1e-12));

ok('nearest-N handles an empty station list', NYC.nearestOverall(TIMES_SQ, [], 3).length === 0);
ok('nearest-N handles n greater than the list', NYC.nearestOverall(TIMES_SQ, acl, 1e6).length <= acl.length);

// ------------------------------------------------------------------ raster

const grid = NYC.makeRasterGrid(NYC.NYC_BBOX, 0.02);
ok('grid covers the bbox', grid.cols > 10 && grid.rows > 10, `${grid.cols}x${grid.rows}`);
ok('grid longitudes stay inside the bbox',
  grid.lons[0] > NYC.NYC_BBOX[0] && grid.lons[grid.cols - 1] < NYC.NYC_BBOX[2]);
ok('grid latitudes run top-down', grid.lats[0] > grid.lats[grid.rows - 1]);
ok('grid rows are evenly spaced in mercator space', (() => {
  const step = NYC.latToMercY(grid.lats[0]) - NYC.latToMercY(grid.lats[1]);
  for (let i = 1; i < grid.rows - 1; i++) {
    const d = NYC.latToMercY(grid.lats[i]) - NYC.latToMercY(grid.lats[i + 1]);
    if (Math.abs(d - step) > 1e-12) return false;
  }
  return true;
})());

const fine = NYC.makeRasterGrid(NYC.NYC_BBOX, 0.0001, 5000);
ok('grid honours the cell cap', fine.cols * fine.rows <= 5000, `${fine.cols}x${fine.rows}`);

// The optimised raster must equal a naive per-pair loop, exactly.
const fast = NYC.walkTimeRaster(grid, acl);
let maxDelta = 0;
for (let i = 0; i < grid.rows; i++) {
  for (let j = 0; j < grid.cols; j++) {
    let best = Infinity;
    for (const s of acl) {
      const d = NYC.manhattanM(grid.lons[j], grid.lats[i], s.lon, s.lat);
      if (d < best) best = d;
    }
    maxDelta = Math.max(maxDelta, Math.abs(fast[i * grid.cols + j] - NYC.walkMinutes(best)));
  }
}
ok('optimised raster equals the naive computation', maxDelta < 1e-9, `max delta ${maxDelta}`);

const emptyRaster = NYC.walkTimeRaster(grid, []);
ok('raster with no stations is all Infinity', emptyRaster.every((v) => v === Infinity));

// A cell sitting on a station should read ~0 minutes.
const onStation = NYC.makeRasterGrid(
  [acl[0].lon - 0.001, acl[0].lat - 0.001, acl[0].lon + 0.001, acl[0].lat + 0.001], 0.0005);
ok('cells at a station are near zero minutes', Math.min(...NYC.walkTimeRaster(onStation, acl)) < 0.5);

// ------------------------------------------------------------ travel raster

const local = NYC.makeRasterGrid([-74.02, 40.73, -73.95, 40.78], 0.004);
const walkT = NYC.travelTimeRaster(local, TIMES_SQ, { mode: 'walk', rush: false });
const carT = NYC.travelTimeRaster(local, TIMES_SQ, { mode: 'car', rush: false });
ok('driving is faster than walking everywhere', walkT.every((v, i) => v >= carT[i] - 1e-9));

const carRush = NYC.travelTimeRaster(local, TIMES_SQ, { mode: 'car', rush: true });
ok('rush hour doubles car times', carRush.every((v, i) => Math.abs(v - carT[i] * 2) < 1e-9));

const walkRush = NYC.travelTimeRaster(local, TIMES_SQ, { mode: 'walk', rush: true });
ok('rush hour leaves walking alone', walkRush.every((v, i) => Math.abs(v - walkT[i]) < 1e-12));

const metroT = NYC.travelTimeRaster(local, TIMES_SQ, { mode: 'metro', rush: false, stations: acl });
ok('metro includes both walking legs', metroT.every((v) => v > 0));
ok('metro without stations is unreachable',
  NYC.travelTimeRaster(local, TIMES_SQ, { mode: 'metro', stations: [] }).every((v) => v === Infinity));

// Manual three-leg check for one cell, against app.R's model.
(() => {
  const r = 3, c = 4;
  const glat = local.lats[r], glon = local.lons[c];
  let board = null, boardD = Infinity;
  for (const s of acl) {
    const d = NYC.manhattanM(TIMES_SQ.lon, TIMES_SQ.lat, s.lon, s.lat);
    if (d < boardD) { boardD = d; board = s; }
  }
  let alight = null, alightD = Infinity;
  for (const s of acl) {
    const d = NYC.manhattanM(glon, glat, s.lon, s.lat);
    if (d < alightD) { alightD = d; alight = s; }
  }
  const expected = NYC.walkMinutes(boardD)
    + NYC.travelMinutesEstimate(NYC.manhattanM(board.lon, board.lat, alight.lon, alight.lat), 'metro', false)
    + NYC.walkMinutes(alightD);
  near('metro three-leg model matches app.R', metroT[r * local.cols + c], expected, 1e-9);
})();

// ------------------------------------------------------- ratio calibration

(() => {
  const base = NYC.travelTimeRaster(local, TIMES_SQ, { mode: 'car', rush: false });
  const flat = NYC.applyRatioField(local, base.slice(), NYC.sampleGridPoints(local, 16).map((p) => ({
    lat: p.lat, lon: p.lon, ratio: 1.5
  })));
  ok('a uniform ratio field scales every cell', flat.every((v, i) => Math.abs(v - base[i] * 1.5) < 1e-9));
  ok('too few samples leaves the raster untouched', (() => {
    const copy = base.slice();
    return NYC.applyRatioField(local, copy, [{ lat: 40.75, lon: -73.98, ratio: 9 }])
      .every((v, i) => v === base[i]);
  })());
  const pts = NYC.sampleGridPoints(local, 25);
  ok('sample points land inside the grid',
    pts.length > 0 && pts.every((p) => p.row >= 0 && p.row < local.rows && p.col >= 0 && p.col < local.cols));
})();

// ------------------------------------------------------------ colour + stats

ok('magma starts near-black', NYC.magmaHex(0) === '#000004');
ok('magma ends pale yellow', NYC.magmaHex(1) === '#fcfdbf');
ok('magma clamps out-of-range input', NYC.magmaHex(-5) === '#000004' && NYC.magmaHex(5) === '#fcfdbf');
ok('magma brightens monotonically', (() => {
  let prev = -1;
  for (let t = 0; t <= 1.0001; t += 0.05) {
    const [r, g, b] = NYC.magma(t);
    const lum = 0.2126 * r + 0.7152 * g + 0.0722 * b;
    if (lum < prev - 1) return false;
    prev = lum;
  }
  return true;
})());

(() => {
  const px = NYC.rasterToPixels(Float64Array.from([0, 5, 10, 15, 20, Infinity]), 15, 0.4);
  ok('cells inside the cap are painted', px[3] === 102 && px[7] === 102);
  ok('cells at the cap are transparent', px[3 * 4 + 3] === 0);
  ok('cells beyond the cap are transparent', px[4 * 4 + 3] === 0);
  ok('unreachable cells are transparent', px[5 * 4 + 3] === 0);
  ok('the nearest cell is the darkest', px[0] === 0 && px[1] === 0 && px[2] === 4);
})();

(() => {
  const s = NYC.summarise(Float64Array.from([1, 2, 3, 4, 5]), [2, 4]);
  near('summary min', s.min, 1);
  near('summary max', s.max, 5);
  near('summary median', s.median, 3);
  near('summary mean', s.mean, 3);
  near('summary q1 matches R type-7', s.q1, 2);
  near('summary q3 matches R type-7', s.q3, 4);
  near('summary sd is the sample sd', s.sd, Math.sqrt(2.5));
  near('summary threshold share', s.within[0].pct, 40);
  ok('summary ignores unreachable cells',
    NYC.summarise(Float64Array.from([1, Infinity, 3])).n === 2);
  ok('summary of nothing is null', NYC.summarise(Float64Array.from([])) === null);

  // The limit argument is what lets the page describe only the coloured-in area.
  const capped = NYC.summarise(Float64Array.from([1, 2, 3, 40, 90]), [], 10);
  ok('limit drops cells past the cap', capped.n === 3, `kept ${capped.n}`);
  near('limit changes the median', capped.median, 2);
  ok('limit past everything keeps all cells',
    NYC.summarise(Float64Array.from([1, 2, 3]), [], 999).n === 3);
  ok('limit below everything yields null',
    NYC.summarise(Float64Array.from([40, 90]), [], 10) === null);
})();

// ----------------------------------------------------------------- results

if (failures.length) {
  console.error(`\n✗ ${failures.length} failed, ${passed} passed\n`);
  failures.forEach((f) => console.error(`  ✗ ${f}`));
  process.exit(1);
}
console.log(`✓ all ${passed} geo.js checks passed`);
