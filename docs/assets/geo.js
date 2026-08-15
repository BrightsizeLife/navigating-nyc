/* geo.js — the distance/travel-time math behind the static page.
 *
 * Everything here is a pure function of stored station data, so the page needs
 * no server and no API key. The formulas mirror app.R so the Shiny app and this
 * page produce the same numbers; the few deliberate differences are called out
 * in docs/README.md.
 *
 * Loads as a plain <script> in the browser (globals on `window.NYC`) and as a
 * CommonJS module in Node (`node tools/test_geo.js`).
 */
(function (root, factory) {
  var api = factory();
  if (typeof module !== 'undefined' && module.exports) module.exports = api;
  if (root) root.NYC = api;
})(typeof window !== 'undefined' ? window : null, function () {
  'use strict';

  // (min lon, min lat, max lon, max lat) — same box as app.R
  var NYC_BBOX = [-74.3, 40.45, -73.65, 40.95];

  var M_PER_DEG_LAT = 111320;
  var WALK_M_PER_S = 1.4;

  // Rough NYC averages, in m/s. Walk is the trustworthy one; the rest are
  // straight-line approximations unless road-network calibration is applied.
  var MODE_SPEEDS = { walk: 1.4, bike: 4.5, bus: 3.5, metro: 8.9, car: 6.7 };
  var RUSH_MULT = { walk: 1.0, bike: 1.1, bus: 1.5, metro: 1.2, car: 2.0 };
  var MODE_LABELS = { walk: 'Walk', bike: 'Bike', bus: 'Bus', metro: 'Metro', car: 'Car' };

  // ---------------------------------------------------------------- distance

  // Manhattan distance in metres (|Δlat| + |Δlon|), a better walking estimate
  // than straight-line haversine on NYC's street grid.
  function manhattanM(lon1, lat1, lon2, lat2) {
    var mPerDegLon = M_PER_DEG_LAT * Math.cos(((lat1 + lat2) / 2) * Math.PI / 180);
    return Math.abs(lat2 - lat1) * M_PER_DEG_LAT + Math.abs(lon2 - lon1) * mPerDegLon;
  }

  function walkMinutes(meters, mPerS) {
    return meters / (mPerS || WALK_M_PER_S) / 60;
  }

  function travelMinutesEstimate(meters, mode, rushHour) {
    var speed = MODE_SPEEDS[mode] || WALK_M_PER_S;
    var mult = rushHour ? (RUSH_MULT[mode] || 1) : 1;
    return (meters / speed / 60) * mult;
  }

  // Effective speed once rush hour is taken into account — used to size the
  // local grid so it still covers the requested time cap.
  function effectiveSpeed(mode, rushHour) {
    var speed = MODE_SPEEDS[mode] || WALK_M_PER_S;
    return rushHour ? speed / (RUSH_MULT[mode] || 1) : speed;
  }

  // ----------------------------------------------------------------- Web Mercator

  function latToMercY(lat) {
    return Math.log(Math.tan(Math.PI / 4 + (lat * Math.PI) / 360));
  }

  function mercYToLat(y) {
    return (Math.atan(Math.exp(y)) - Math.PI / 4) * (360 / Math.PI);
  }

  // ---------------------------------------------------------------- stations

  // Expand the generated compact records into something readable.
  function parseStations(raw) {
    var list = (raw && raw.stations) || [];
    return list.map(function (s) {
      return {
        name: s.n,
        lines: s.l,
        routes: s.l ? s.l.split(',') : [],
        lat: s.y,
        lon: s.x,
        borough: s.b || '',
        ada: s.a || 0
      };
    });
  }

  var BOROUGHS = { M: 'Manhattan', Bk: 'Brooklyn', Q: 'Queens', Bx: 'Bronx', SI: 'Staten Island' };
  function boroughName(code) { return BOROUGHS[code] || code || ''; }

  function servesAny(station, selected) {
    for (var i = 0; i < station.routes.length; i++) {
      if (selected.indexOf(station.routes[i]) !== -1) return true;
    }
    return false;
  }

  // Stations serving at least one selected route (app.R's stations_rv()).
  function filterByLines(stations, selected) {
    if (!selected || !selected.length) return [];
    return stations.filter(function (s) { return servesAny(s, selected); });
  }

  // Stations serving none of the selected routes (app.R's "other lines" table).
  function filterExcludingLines(stations, selected) {
    if (!selected || !selected.length) return [];
    return stations.filter(function (s) { return !servesAny(s, selected); });
  }

  // ---------------------------------------------------------------- nearest-N

  function withDistances(origin, stations) {
    return stations.map(function (s) {
      var d = manhattanM(origin.lon, origin.lat, s.lon, s.lat);
      return {
        name: s.name,
        lines: s.lines,
        routes: s.routes,
        lat: s.lat,
        lon: s.lon,
        borough: s.borough,
        ada: s.ada,
        distance_m: d,
        walk_min: Math.round(walkMinutes(d) * 10) / 10
      };
    });
  }

  function byDistance(a, b) { return a.distance_m - b.distance_m; }

  // Nearest n, one row per station name. Unlike app.R the result stays sorted
  // by distance rather than falling back to alphabetical order.
  function nearestOverall(origin, stations, n) {
    var rows = withDistances(origin, stations).sort(byDistance);
    var seen = Object.create(null);
    var out = [];
    for (var i = 0; i < rows.length && out.length < n; i++) {
      if (seen[rows[i].name]) continue;
      seen[rows[i].name] = true;
      out.push(rows[i]);
    }
    return out;
  }

  // Nearest n stations for each selected route, in the order the routes were given.
  function nearestByLine(origin, stations, selected, n) {
    var out = [];
    (selected || []).forEach(function (route) {
      var onRoute = stations.filter(function (s) { return s.routes.indexOf(route) !== -1; });
      nearestOverall(origin, onRoute, n).forEach(function (row) {
        out.push(Object.assign({ line: route }, row));
      });
    });
    return out;
  }

  function nearestFromUnselected(origin, allStations, selected, n) {
    return nearestOverall(origin, filterExcludingLines(allStations, selected), n);
  }

  // ------------------------------------------------------------------ raster

  // A raster whose rows are evenly spaced in Web Mercator, so it drops onto a
  // Leaflet image overlay with no projection stretch. `stepDeg` sets resolution
  // in degrees, matching the Shiny app's fidelity slider.
  function makeRasterGrid(bbox, stepDeg, maxCells) {
    var minLon = bbox[0], minLat = bbox[1], maxLon = bbox[2], maxLat = bbox[3];
    var cols = Math.max(2, Math.ceil((maxLon - minLon) / stepDeg));
    var rows = Math.max(2, Math.ceil((maxLat - minLat) / stepDeg));

    // Keep the canvas bounded on very fine settings.
    var cap = maxCells || 400000;
    if (cols * rows > cap) {
      var shrink = Math.sqrt(cap / (cols * rows));
      cols = Math.max(2, Math.floor(cols * shrink));
      rows = Math.max(2, Math.floor(rows * shrink));
    }

    var lons = new Float64Array(cols);
    for (var j = 0; j < cols; j++) lons[j] = minLon + ((j + 0.5) / cols) * (maxLon - minLon);

    // Row 0 is the top of the image, i.e. the highest latitude.
    var yTop = latToMercY(maxLat), yBottom = latToMercY(minLat);
    var lats = new Float64Array(rows);
    for (var i = 0; i < rows; i++) lats[i] = mercYToLat(yTop - ((i + 0.5) / rows) * (yTop - yBottom));

    return { cols: cols, rows: rows, lons: lons, lats: lats, bbox: bbox };
  }

  // Minutes on foot from every grid cell to its nearest station.
  //
  // The cos() term in manhattanM depends only on the row's latitude and the
  // station's, so it is hoisted out of the inner loop: ~500 cos calls per row
  // instead of one per cell/station pair, with identical results.
  function walkTimeRaster(grid, stations) {
    var n = stations.length;
    var out = new Float64Array(grid.cols * grid.rows);
    if (!n) { out.fill(Infinity); return out; }

    var sLat = new Float64Array(n), sLon = new Float64Array(n);
    for (var k = 0; k < n; k++) { sLat[k] = stations[k].lat; sLon[k] = stations[k].lon; }

    var dLatM = new Float64Array(n), mLon = new Float64Array(n);
    var perMin = 1 / (WALK_M_PER_S * 60);

    for (var i = 0; i < grid.rows; i++) {
      var lat = grid.lats[i];
      for (var s = 0; s < n; s++) {
        dLatM[s] = Math.abs(sLat[s] - lat) * M_PER_DEG_LAT;
        mLon[s] = M_PER_DEG_LAT * Math.cos(((lat + sLat[s]) / 2) * Math.PI / 180);
      }
      var rowOff = i * grid.cols;
      for (var j = 0; j < grid.cols; j++) {
        var lon = grid.lons[j], best = Infinity;
        for (var t = 0; t < n; t++) {
          var d = dLatM[t] + Math.abs(sLon[t] - lon) * mLon[t];
          if (d < best) best = d;
        }
        out[rowOff + j] = best * perMin;
      }
    }
    return out;
  }

  // Minutes from one origin to every grid cell.
  //
  // "metro" mirrors app.R's three-leg model: walk to the origin's nearest
  // station, ride to the station nearest the destination, walk the last mile.
  function travelTimeRaster(grid, origin, opts) {
    var mode = opts.mode || 'walk';
    var rush = !!opts.rush;
    var out = new Float64Array(grid.cols * grid.rows);

    if (mode !== 'metro') {
      for (var i = 0; i < grid.rows; i++) {
        var lat = grid.lats[i], rowOff = i * grid.cols;
        for (var j = 0; j < grid.cols; j++) {
          out[rowOff + j] = travelMinutesEstimate(
            manhattanM(origin.lon, origin.lat, grid.lons[j], lat), mode, rush);
        }
      }
      return out;
    }

    var stations = opts.stations || [];
    if (!stations.length) { out.fill(Infinity); return out; }

    var n = stations.length;
    var sLat = new Float64Array(n), sLon = new Float64Array(n);
    for (var k = 0; k < n; k++) { sLat[k] = stations[k].lat; sLon[k] = stations[k].lon; }

    // Origin's boarding station.
    var boardIdx = 0, boardDist = Infinity;
    for (var b = 0; b < n; b++) {
      var db = manhattanM(origin.lon, origin.lat, sLon[b], sLat[b]);
      if (db < boardDist) { boardDist = db; boardIdx = b; }
    }
    var originWalkMin = walkMinutes(boardDist);
    var boardLat = sLat[boardIdx], boardLon = sLon[boardIdx];

    var dLatM = new Float64Array(n), mLon = new Float64Array(n);
    for (var r = 0; r < grid.rows; r++) {
      var glat = grid.lats[r];
      for (var s = 0; s < n; s++) {
        dLatM[s] = Math.abs(sLat[s] - glat) * M_PER_DEG_LAT;
        mLon[s] = M_PER_DEG_LAT * Math.cos(((glat + sLat[s]) / 2) * Math.PI / 180);
      }
      var off = r * grid.cols;
      for (var c = 0; c < grid.cols; c++) {
        var glon = grid.lons[c], best = Infinity, bestIdx = 0;
        for (var t = 0; t < n; t++) {
          var d = dLatM[t] + Math.abs(sLon[t] - glon) * mLon[t];
          if (d < best) { best = d; bestIdx = t; }
        }
        var rideM = manhattanM(boardLon, boardLat, sLon[bestIdx], sLat[bestIdx]);
        out[off + c] = originWalkMin + travelMinutesEstimate(rideM, 'metro', rush) + walkMinutes(best);
      }
    }
    return out;
  }

  // Scale a straight-line raster by a sparse field of measured/estimated
  // ratios, smoothed with inverse-distance weighting. Used to bend the
  // estimates toward real road-network durations without one request per cell.
  function applyRatioField(grid, minutes, samples, power) {
    if (!samples || samples.length < 3) return minutes;
    var p = power || 2;
    var n = samples.length;
    var sLat = new Float64Array(n), sLon = new Float64Array(n), sRatio = new Float64Array(n);
    for (var k = 0; k < n; k++) {
      sLat[k] = samples[k].lat; sLon[k] = samples[k].lon; sRatio[k] = samples[k].ratio;
    }

    for (var i = 0; i < grid.rows; i++) {
      var lat = grid.lats[i], off = i * grid.cols;
      for (var j = 0; j < grid.cols; j++) {
        var lon = grid.lons[j], num = 0, den = 0, exact = -1;
        for (var t = 0; t < n; t++) {
          var dx = lon - sLon[t], dy = lat - sLat[t];
          var d2 = dx * dx + dy * dy;
          if (d2 < 1e-12) { exact = t; break; }
          var w = 1 / Math.pow(d2, p / 2);
          num += w * sRatio[t];
          den += w;
        }
        minutes[off + j] *= exact >= 0 ? sRatio[exact] : (den ? num / den : 1);
      }
    }
    return minutes;
  }

  // Evenly spread sample points across a grid, for the ratio field above.
  function sampleGridPoints(grid, count) {
    var total = grid.cols * grid.rows;
    var want = Math.min(count, total);
    var side = Math.max(2, Math.floor(Math.sqrt(want)));
    var pts = [];
    for (var i = 0; i < side; i++) {
      for (var j = 0; j < side; j++) {
        var r = Math.min(grid.rows - 1, Math.round(((i + 0.5) / side) * grid.rows));
        var c = Math.min(grid.cols - 1, Math.round(((j + 0.5) / side) * grid.cols));
        pts.push({ lat: grid.lats[r], lon: grid.lons[c], row: r, col: c });
      }
    }
    return pts;
  }

  // ------------------------------------------------------------------ colour

  // magma(10) from viridisLite, linearly interpolated. Dark = near, bright = far.
  var MAGMA = [
    [0, 0, 4], [24, 15, 61], [68, 15, 118], [114, 31, 129], [158, 47, 127],
    [205, 64, 113], [241, 96, 93], [253, 150, 104], [254, 202, 141], [252, 253, 191]
  ];

  function magma(t) {
    var x = t <= 0 ? 0 : t >= 1 ? 1 : t;
    var pos = x * (MAGMA.length - 1);
    var i = Math.min(MAGMA.length - 2, Math.floor(pos));
    var f = pos - i, a = MAGMA[i], b = MAGMA[i + 1];
    return [
      Math.round(a[0] + (b[0] - a[0]) * f),
      Math.round(a[1] + (b[1] - a[1]) * f),
      Math.round(a[2] + (b[2] - a[2]) * f)
    ];
  }

  function magmaHex(t) {
    var c = magma(t);
    return '#' + [c[0], c[1], c[2]].map(function (v) {
      return ('0' + v.toString(16)).slice(-2);
    }).join('');
  }

  // RGBA pixels for a minutes raster. Cells at or beyond the cap go fully
  // transparent so the basemap reads through, as in the Shiny version.
  function rasterToPixels(minutes, cap, alpha) {
    var a = Math.round((alpha == null ? 0.4 : alpha) * 255);
    var px = new Uint8ClampedArray(minutes.length * 4);
    var lut = new Array(257);
    for (var q = 0; q <= 256; q++) lut[q] = magma(q / 256);

    for (var i = 0; i < minutes.length; i++) {
      var m = minutes[i], o = i * 4;
      if (!(m < cap)) continue; // also catches Infinity/NaN → transparent
      var c = lut[Math.round((m / cap) * 256)];
      px[o] = c[0]; px[o + 1] = c[1]; px[o + 2] = c[2]; px[o + 3] = a;
    }
    return px;
  }

  // ------------------------------------------------------------------- stats

  function quantile(sorted, p) {
    if (!sorted.length) return NaN;
    var h = (sorted.length - 1) * p;
    var lo = Math.floor(h), hi = Math.ceil(h);
    return sorted[lo] + (sorted[hi] - sorted[lo]) * (h - lo);
  }

  // Coverage summary for a minutes raster; app.R printed the same figures to
  // the R console, this page shows them in the UI.
  //
  // `limit` restricts the summary to cells at or under that many minutes, which
  // is how the page describes just the coloured-in part of the map — averaging
  // over the whole bounding box would mostly be measuring open water.
  function summarise(minutes, thresholds, limit) {
    var vals = [];
    for (var i = 0; i < minutes.length; i++) {
      if (!isFinite(minutes[i])) continue;
      if (limit != null && minutes[i] > limit) continue;
      vals.push(minutes[i]);
    }
    if (!vals.length) return null;
    vals.sort(function (a, b) { return a - b; });

    var sum = 0;
    for (var j = 0; j < vals.length; j++) sum += vals[j];
    var mean = sum / vals.length;

    var sq = 0;
    for (var k = 0; k < vals.length; k++) sq += (vals[k] - mean) * (vals[k] - mean);
    var sd = vals.length > 1 ? Math.sqrt(sq / (vals.length - 1)) : 0;

    var within = (thresholds || [5, 10, 15, 20]).map(function (t) {
      var c = 0;
      for (var m = 0; m < vals.length; m++) if (vals[m] <= t) c++;
      return { minutes: t, pct: (100 * c) / vals.length };
    });

    return {
      n: vals.length,
      min: vals[0],
      q1: quantile(vals, 0.25),
      median: quantile(vals, 0.5),
      mean: mean,
      q3: quantile(vals, 0.75),
      max: vals[vals.length - 1],
      sd: sd,
      within: within
    };
  }

  return {
    NYC_BBOX: NYC_BBOX,
    M_PER_DEG_LAT: M_PER_DEG_LAT,
    WALK_M_PER_S: WALK_M_PER_S,
    MODE_SPEEDS: MODE_SPEEDS,
    RUSH_MULT: RUSH_MULT,
    MODE_LABELS: MODE_LABELS,
    manhattanM: manhattanM,
    walkMinutes: walkMinutes,
    travelMinutesEstimate: travelMinutesEstimate,
    effectiveSpeed: effectiveSpeed,
    latToMercY: latToMercY,
    mercYToLat: mercYToLat,
    parseStations: parseStations,
    boroughName: boroughName,
    servesAny: servesAny,
    filterByLines: filterByLines,
    filterExcludingLines: filterExcludingLines,
    withDistances: withDistances,
    nearestOverall: nearestOverall,
    nearestByLine: nearestByLine,
    nearestFromUnselected: nearestFromUnselected,
    makeRasterGrid: makeRasterGrid,
    walkTimeRaster: walkTimeRaster,
    travelTimeRaster: travelTimeRaster,
    applyRatioField: applyRatioField,
    sampleGridPoints: sampleGridPoints,
    magma: magma,
    magmaHex: magmaHex,
    rasterToPixels: rasterToPixels,
    summarise: summarise
  };
});
