/* app.js — UI for the static NYC Walkability page.
 *
 * Mirrors the Shiny app in app.R without a server: station data ships with the
 * page, all distance and travel-time maths runs in the browser (see geo.js),
 * and the only network calls are map tiles, address lookup, and the optional
 * OSRM road-network pass. No API keys anywhere.
 */
(function () {
  'use strict';

  var NYC = window.NYC;
  var DATA = window.MTA_STATIONS;

  if (!NYC || !DATA || !window.L) {
    document.body.insertAdjacentHTML('afterbegin',
      '<p class="noscript">Could not load the map libraries or station data. ' +
      'Make sure vendor/, data/ and assets/ sit next to index.html.</p>');
    return;
  }

  var STATIONS = NYC.parseStations(DATA);
  var DEFAULT_LINES = ['A', 'C', 'L'];

  // MTA trunk-line colours. Yellow and light grey need dark text.
  var ROUTE_COLORS = {
    '1': '#EE352E', '2': '#EE352E', '3': '#EE352E',
    '4': '#00933C', '5': '#00933C', '6': '#00933C',
    '7': '#B933AD',
    A: '#0039A6', C: '#0039A6', E: '#0039A6',
    B: '#FF6319', D: '#FF6319', F: '#FF6319', M: '#FF6319',
    G: '#6CBE45',
    J: '#996633', Z: '#996633',
    L: '#A7A9AC',
    N: '#FCCC0A', Q: '#FCCC0A', R: '#FCCC0A', W: '#FCCC0A',
    S: '#808183',
    SIR: '#0039A6'
  };
  var DARK_TEXT_ROUTES = { N: 1, Q: 1, R: 1, W: 1, L: 1 };

  var QUICK_PLACES = [
    { label: 'Times Square', lat: 40.758, lon: -73.9855, name: 'Times Square, Manhattan, NY' },
    { label: 'Chelsea Market', lat: 40.7425, lon: -74.0061, name: 'Chelsea Market, 75 9th Ave, Manhattan, NY' },
    { label: 'Grand Army Plaza', lat: 40.6725, lon: -73.97, name: 'Grand Army Plaza, Brooklyn, NY' },
    { label: 'Co-op City', lat: 40.874, lon: -73.829, name: 'Co-op City, Bronx, NY' }
  ];

  var TILE_URL = 'https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png';
  var TILE_ATTR = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> ' +
    'contributors &copy; <a href="https://carto.com/attributions">CARTO</a>';

  // Public OSRM instances (FOSSGIS). Free, keyless, rate-limited, best-effort.
  var OSRM = {
    walk: { base: 'https://routing.openstreetmap.de/routed-foot', profile: 'foot' },
    bike: { base: 'https://routing.openstreetmap.de/routed-bike', profile: 'bike' },
    car: { base: 'https://routing.openstreetmap.de/routed-car', profile: 'car' }
  };
  var OSRM_SAMPLES = 80; // + 1 source, comfortably under the usual 100-coordinate cap

  // --------------------------------------------------------------- utilities

  function $(id) { return document.getElementById(id); }

  // Small DOM builder — everything user- or API-supplied goes in as text.
  function h(tag, props, children) {
    var node = document.createElement(tag);
    if (props) {
      Object.keys(props).forEach(function (k) {
        if (k === 'class') node.className = props[k];
        else if (k === 'text') node.textContent = props[k];
        else if (k === 'html') node.innerHTML = props[k];
        else if (k === 'style') node.setAttribute('style', props[k]);
        else if (k.slice(0, 2) === 'on') node.addEventListener(k.slice(2), props[k]);
        else if (props[k] === true) node.setAttribute(k, '');
        else if (props[k] !== false && props[k] != null) node.setAttribute(k, props[k]);
      });
    }
    (children || []).forEach(function (c) {
      if (c == null || c === false) return;
      node.appendChild(typeof c === 'string' || typeof c === 'number'
        ? document.createTextNode(String(c)) : c);
    });
    return node;
  }

  function clear(node) { while (node.firstChild) node.removeChild(node.firstChild); }

  function fmt(x, digits) {
    if (!isFinite(x)) return '—';
    return x.toFixed(digits == null ? 1 : digits);
  }

  function fmtInt(x) {
    return isFinite(x) ? Math.round(x).toLocaleString() : '—';
  }

  function debounce(fn, ms) {
    var t;
    return function () {
      var args = arguments, self = this;
      clearTimeout(t);
      t = setTimeout(function () { fn.apply(self, args); }, ms);
    };
  }

  function toast(message, kind, ms) {
    var box = $('toasts');
    var node = h('div', { class: 'toast' + (kind ? ' toast--' + kind : ''), role: 'status', text: message });
    box.appendChild(node);
    setTimeout(function () {
      node.style.opacity = '0';
      setTimeout(function () { if (node.parentNode) node.parentNode.removeChild(node); }, 250);
    }, ms || 4200);
  }

  // fetch with a hard timeout, so a slow public API can't hang the button.
  function fetchJSON(url, ms) {
    var ctrl = typeof AbortController !== 'undefined' ? new AbortController() : null;
    var timer = setTimeout(function () { if (ctrl) ctrl.abort(); }, ms || 12000);
    return fetch(url, { signal: ctrl ? ctrl.signal : undefined, headers: { Accept: 'application/json' } })
      .then(function (res) {
        if (!res.ok) throw new Error('HTTP ' + res.status);
        return res.json();
      })
      .finally(function () { clearTimeout(timer); });
  }

  // ------------------------------------------------------------ route badges

  function routeColor(route) { return ROUTE_COLORS[route] || '#4b5563'; }
  function routeInk(route) { return DARK_TEXT_ROUTES[route] ? '#111827' : '#ffffff'; }

  function routeDot(route) {
    return h('span', {
      class: 'route-dot' + (route.length > 1 ? ' route-dot--wide' : ''),
      style: 'background:' + routeColor(route) + ';color:' + routeInk(route),
      text: route
    });
  }

  function routeList(linesStr) {
    return h('span', { class: 'route-list' }, (linesStr || '').split(',').filter(Boolean).map(routeDot));
  }

  // ------------------------------------------------------------------- state

  var state = {
    lines: DEFAULT_LINES.slice(),
    walkCap: 15,
    gridRes: 0.004,
    origin: null,
    tab: 'walk',
    travel: { mode: 'walk', cap: 20, rush: false, osrm: false }
  };

  function readStateFromUrl() {
    var raw = location.hash.replace(/^#/, '');
    if (!raw) return;
    var q = new URLSearchParams(raw);
    var known = {};
    DATA.routes.forEach(function (r) { known[r] = true; });

    if (q.has('lines')) {
      var picked = q.get('lines').split(',').map(function (s) { return s.trim().toUpperCase(); })
        .filter(function (s) { return known[s]; });
      state.lines = picked;
    }
    var cap = parseInt(q.get('cap'), 10);
    if (isFinite(cap) && cap >= 3 && cap <= 30) state.walkCap = cap;

    var res = parseFloat(q.get('res'));
    if (isFinite(res) && res >= 0.002 && res <= 0.01) state.gridRes = res;

    var lat = parseFloat(q.get('lat')), lon = parseFloat(q.get('lon'));
    if (isFinite(lat) && isFinite(lon)) {
      state.origin = { lat: lat, lon: lon, label: q.get('place') || (fmt(lat, 4) + ', ' + fmt(lon, 4)) };
    }
    if (['walk', 'nearest', 'travel', 'reference'].indexOf(q.get('tab')) !== -1) state.tab = q.get('tab');

    var mode = q.get('mode');
    if (NYC.MODE_LABELS[mode]) state.travel.mode = mode;
    var tcap = parseInt(q.get('tcap'), 10);
    if (isFinite(tcap) && tcap >= 5 && tcap <= 60) state.travel.cap = tcap;
    state.travel.rush = q.get('rush') === '1';
  }

  var writeStateToUrl = debounce(function () {
    var q = new URLSearchParams();
    q.set('lines', state.lines.join(','));
    q.set('cap', String(state.walkCap));
    q.set('res', String(state.gridRes));
    q.set('tab', state.tab);
    q.set('mode', state.travel.mode);
    q.set('tcap', String(state.travel.cap));
    if (state.travel.rush) q.set('rush', '1');
    if (state.origin) {
      q.set('lat', state.origin.lat.toFixed(5));
      q.set('lon', state.origin.lon.toFixed(5));
      q.set('place', state.origin.label);
    }
    history.replaceState(null, '', '#' + q.toString());
  }, 350);

  function selectedStations() { return NYC.filterByLines(STATIONS, state.lines); }

  // -------------------------------------------------------------- route picker

  function renderRoutePicker() {
    var box = $('route-picker');
    clear(box);
    DATA.routes.forEach(function (route) {
      var on = state.lines.indexOf(route) !== -1;
      box.appendChild(h('button', {
        type: 'button',
        class: 'route' + (route.length > 1 ? ' route--wide' : ''),
        style: '--route-color:' + routeColor(route) + ';--route-ink:' + routeInk(route),
        'aria-pressed': on ? 'true' : 'false',
        'aria-label': 'Line ' + route,
        title: 'Line ' + route,
        text: route,
        onclick: function () { toggleLine(route); }
      }));
    });
    renderLinesSummary();
  }

  function renderLinesSummary() {
    var n = selectedStations().length;
    $('lines-summary').textContent = state.lines.length
      ? state.lines.length + ' line' + (state.lines.length === 1 ? '' : 's') + ' · ' + n + ' stations'
      : 'No lines selected — pick at least one.';
  }

  function toggleLine(route) {
    var i = state.lines.indexOf(route);
    if (i === -1) state.lines.push(route); else state.lines.splice(i, 1);
    // Keep the picker's display order so tables read predictably.
    state.lines.sort(function (a, b) { return DATA.routes.indexOf(a) - DATA.routes.indexOf(b); });
    renderRoutePicker();
    onLinesChanged();
  }

  function setLines(list) {
    state.lines = list.slice();
    renderRoutePicker();
    onLinesChanged();
  }

  function onLinesChanged() {
    walkCache = null;
    refreshActiveTab();
    writeStateToUrl();
  }

  // -------------------------------------------------------------------- tabs

  var TABS = ['walk', 'nearest', 'travel', 'reference'];

  function showTab(name) {
    state.tab = name;
    TABS.forEach(function (t) {
      var tab = $('tab-' + t), panel = $('panel-' + t), on = t === name;
      tab.classList.toggle('is-active', on);
      tab.setAttribute('aria-selected', on ? 'true' : 'false');
      panel.classList.toggle('is-active', on);
      panel.hidden = !on;
    });
    refreshActiveTab();
    writeStateToUrl();
  }

  function refreshActiveTab() {
    renderLinesSummary();
    if (state.tab === 'walk') { ensureWalkMap(); scheduleWalkRaster(); }
    if (state.tab === 'nearest') renderNearest();
    if (state.tab === 'travel') { ensureTravelMap(); renderTravelOrigin(); renderTravelMethod(); }
    if (state.tab === 'reference') renderReference();
  }

  // -------------------------------------------------------------------- maps

  function baseMap(id, opts) {
    var map = L.map(id, Object.assign({ preferCanvas: true, zoomControl: true }, opts || {}));
    L.tileLayer(TILE_URL, { attribution: TILE_ATTR, maxZoom: 19 }).addTo(map);
    // Own pane between tiles (200) and overlays (400), so redrawing the raster
    // can never stack it on top of the station markers.
    map.createPane('raster').style.zIndex = 350;
    map.getPane('raster').style.pointerEvents = 'none';
    return map;
  }

  function legendControl(position) {
    var Legend = L.Control.extend({
      options: { position: position || 'bottomleft' },
      onAdd: function () {
        this._div = L.DomUtil.create('div', 'map-legend');
        L.DomEvent.disableClickPropagation(this._div);
        return this._div;
      },
      set: function (title, cap, footer) {
        clear(this._div);
        var stops = [];
        for (var i = 0; i <= 10; i++) stops.push(NYC.magmaHex(i / 10));
        this._div.appendChild(h('b', { text: title }));
        this._div.appendChild(h('div', {
          class: 'map-legend__ramp',
          style: 'background:linear-gradient(to right,' + stops.join(',') + ')'
        }));
        this._div.appendChild(h('div', { class: 'map-legend__scale' }, [
          h('span', { text: '0 min' }),
          h('span', { text: cap + ' min+' })
        ]));
        if (footer) this._div.appendChild(h('div', { class: 'map-legend__foot', text: footer }));
      }
    });
    return new Legend();
  }

  // Paint a minutes raster into a Leaflet image overlay. The grid rows are
  // spaced evenly in Web Mercator (see geo.js), so the image lines up with the
  // basemap without any stretching.
  function drawRaster(map, layerRef, grid, minutes, cap) {
    var canvas = document.createElement('canvas');
    canvas.width = grid.cols;
    canvas.height = grid.rows;
    var ctx = canvas.getContext('2d');
    var img = ctx.createImageData(grid.cols, grid.rows);
    img.data.set(NYC.rasterToPixels(minutes, cap, 0.4));
    ctx.putImageData(img, 0, 0);

    var bounds = L.latLngBounds(
      L.latLng(grid.bbox[1], grid.bbox[0]),
      L.latLng(grid.bbox[3], grid.bbox[2])
    );
    if (layerRef.layer) map.removeLayer(layerRef.layer);
    layerRef.layer = L.imageOverlay(canvas.toDataURL('image/png'), bounds, {
      opacity: 1, interactive: false, className: 'raster-overlay', pane: 'raster'
    }).addTo(map);
  }

  // Minutes at a lat/lng, for the hover readout.
  function sampleRaster(grid, minutes, latlng) {
    var b = grid.bbox;
    if (latlng.lng < b[0] || latlng.lng > b[2] || latlng.lat < b[1] || latlng.lat > b[3]) return null;
    var col = Math.floor(((latlng.lng - b[0]) / (b[2] - b[0])) * grid.cols);
    var yTop = NYC.latToMercY(b[3]), yBot = NYC.latToMercY(b[1]);
    var row = Math.floor(((yTop - NYC.latToMercY(latlng.lat)) / (yTop - yBot)) * grid.rows);
    if (col < 0 || col >= grid.cols || row < 0 || row >= grid.rows) return null;
    return minutes[row * grid.cols + col];
  }

  function busyOverlay(wrapEl, on, label) {
    var existing = wrapEl.querySelector('.map-busy');
    if (!on) { if (existing) existing.remove(); return; }
    if (existing) { existing.textContent = label; return; }
    wrapEl.appendChild(h('div', { class: 'map-busy', text: label }));
  }

  function originMarker(map, ref, origin) {
    if (ref.marker) { map.removeLayer(ref.marker); ref.marker = null; }
    if (!origin) return;
    ref.marker = L.circleMarker([origin.lat, origin.lon], {
      radius: 8, color: '#111827', weight: 3, fillColor: '#f1605d', fillOpacity: 1
    }).addTo(map);
    ref.marker.bindPopup(h('div', { class: 'station-popup' }, [
      h('b', { text: 'Your location' }),
      h('span', { text: origin.label })
    ]));
  }

  // ------------------------------------------------------- walkability tab

  var walkMap = null, walkLegend = null;
  var walkRasterRef = {}, walkOriginRef = {};
  var walkStationLayer = null;
  var walkCache = null; // {key, grid, minutes}

  function ensureWalkMap() {
    if (walkMap) { walkMap.invalidateSize(); return; }
    walkMap = baseMap('map-walk');
    walkMap.fitBounds([[NYC.NYC_BBOX[1], NYC.NYC_BBOX[0]], [NYC.NYC_BBOX[3], NYC.NYC_BBOX[2]]]);
    walkLegend = legendControl().addTo(walkMap);

    var readout = $('walk-readout');
    walkMap.on('mousemove', function (e) {
      if (!walkCache) return;
      var m = sampleRaster(walkCache.grid, walkCache.minutes, e.latlng);
      if (m == null || !isFinite(m)) { readout.hidden = true; return; }
      readout.hidden = false;
      readout.textContent = m >= state.walkCap
        ? 'Over ' + state.walkCap + ' min from a station on your lines'
        : fmt(m, 1) + ' min walk to the nearest station';
    });
    walkMap.on('mouseout', function () { readout.hidden = true; });
  }

  var scheduleWalkRaster = debounce(function () { renderWalkRaster(); }, 160);

  function renderWalkRaster() {
    if (!walkMap) return;
    var wrap = $('map-walk').parentNode;
    var stations = selectedStations();

    if (!stations.length) {
      if (walkRasterRef.layer) { walkMap.removeLayer(walkRasterRef.layer); walkRasterRef.layer = null; }
      if (walkStationLayer) { walkMap.removeLayer(walkStationLayer); walkStationLayer = null; }
      walkCache = null;
      walkLegend.set('Walk time', state.walkCap, 'No lines selected');
      renderWalkStats(null);
      return;
    }

    var key = state.lines.join(',') + '|' + state.gridRes;
    var needsCompute = !walkCache || walkCache.key !== key;

    busyOverlay(wrap, true, needsCompute ? 'Computing walk times…' : 'Repainting…');

    // Let the browser paint the busy state before the synchronous number crunch.
    requestAnimationFrame(function () {
      requestAnimationFrame(function () {
        try {
          if (needsCompute) {
            var grid = NYC.makeRasterGrid(NYC.NYC_BBOX, state.gridRes);
            var t0 = performance.now();
            var minutes = NYC.walkTimeRaster(grid, stations);
            walkCache = { key: key, grid: grid, minutes: minutes };
            console.log('[walkability] %d cells × %d stations in %s ms',
              grid.cols * grid.rows, stations.length, (performance.now() - t0).toFixed(0));
          }

          drawRaster(walkMap, walkRasterRef, walkCache.grid, walkCache.minutes, state.walkCap);
          drawWalkStations(stations);
          originMarker(walkMap, walkOriginRef, state.origin);
          walkLegend.set('Walk time to nearest station', state.walkCap,
            walkCache.grid.cols + ' × ' + walkCache.grid.rows + ' cells');
          renderWalkStats(stations.length);
        } finally {
          busyOverlay(wrap, false);
        }
      });
    });
  }

  function drawWalkStations(stations) {
    if (walkStationLayer) walkMap.removeLayer(walkStationLayer);
    walkStationLayer = L.layerGroup(stations.map(function (s) {
      var marker = L.circleMarker([s.lat, s.lon], {
        radius: 3.5, color: '#0b1220', weight: 1, opacity: .8,
        fillColor: '#22d3ee', fillOpacity: .95
      });
      marker.bindPopup(h('div', { class: 'station-popup' }, [
        h('b', { text: s.name }),
        routeList(s.lines),
        h('span', { text: ' · ' + NYC.boroughName(s.borough) + (s.ada ? ' · ADA accessible' : '') })
      ]));
      return marker;
    })).addTo(walkMap);
  }

  function renderWalkStats(stationCount) {
    var box = $('walk-stats');
    clear(box);

    if (!walkCache) {
      box.appendChild(h('p', { class: 'hint', text: 'Select at least one line to see coverage stats.' }));
      return;
    }

    var cap = state.walkCap;
    var all = NYC.summarise(walkCache.minutes, [5, 10, 15, 20]);
    var covered = NYC.summarise(walkCache.minutes, [], cap);

    // Averages over the whole bounding box would be dominated by water and
    // out-of-city land, so the shape stats describe only the cells inside the
    // cap — the part of the map that is actually painted.
    var tiles = [
      {
        label: 'Area within ' + cap + ' min',
        value: covered ? fmt((100 * covered.n) / all.n, 1) : '0',
        unit: '%'
      },
      { label: 'Median walk there', value: covered ? fmt(covered.median, 1) : '—', unit: 'min' },
      {
        label: 'Middle 50% there',
        value: covered ? fmt(covered.q1, 1) + '–' + fmt(covered.q3, 1) : '—',
        unit: 'min'
      },
      { label: 'Stations mapped', value: String(stationCount), unit: '' }
    ];
    all.within.forEach(function (w) {
      tiles.push({ label: 'Within ' + w.minutes + ' min', value: fmt(w.pct, 1), unit: '%' });
    });

    tiles.forEach(function (t) {
      box.appendChild(h('div', { class: 'stat' }, [
        h('div', { class: 'stat__label', text: t.label }),
        h('div', { class: 'stat__value' }, [t.value, t.unit ? ' ' : '', t.unit ? h('small', { text: t.unit }) : null])
      ]));
    });
  }

  // --------------------------------------------------------------- geocoding

  function photonLabel(props) {
    var street = [props.housenumber, props.street].filter(Boolean).join(' ');
    var parts = [props.name, street, props.district, props.city, props.state].filter(Boolean);
    // Drop consecutive duplicates, e.g. name === street.
    return parts.filter(function (p, i) { return i === 0 || p !== parts[i - 1]; }).join(', ');
  }

  // Photon first (free, OSM-based, CORS-friendly, no key), Nominatim as backup.
  function geocode(query) {
    var bias = '&lat=40.7128&lon=-74.0060&lang=en';
    var photon = 'https://photon.komoot.io/api/?q=' + encodeURIComponent(query) + '&limit=1' + bias;

    return fetchJSON(photon).then(function (res) {
      var f = res && res.features && res.features[0];
      if (!f) throw new Error('no match');
      return {
        lat: f.geometry.coordinates[1],
        lon: f.geometry.coordinates[0],
        label: photonLabel(f.properties || {}) || query,
        via: 'Photon'
      };
    }).catch(function () {
      var nominatim = 'https://nominatim.openstreetmap.org/search?format=json&limit=1&q=' +
        encodeURIComponent(query);
      return fetchJSON(nominatim).then(function (rows) {
        if (!rows || !rows.length) throw new Error('no match');
        return {
          lat: parseFloat(rows[0].lat),
          lon: parseFloat(rows[0].lon),
          label: rows[0].display_name || query,
          via: 'Nominatim'
        };
      });
    });
  }

  function setOrigin(origin, note) {
    var first = !state.origin;
    state.origin = origin;
    var status = $('geocode-status');
    status.className = 'status';
    status.textContent = note || origin.label;

    // Distances only become sortable once there is an origin.
    if (first && !refState.userSorted) { refState.sort = 'distance_m'; refState.dir = 1; }

    if (walkMap) originMarker(walkMap, walkOriginRef, origin);
    if (travelMap) originMarker(travelMap, travelOriginRef, origin);

    var b = NYC.NYC_BBOX;
    if (origin.lon < b[0] || origin.lon > b[2] || origin.lat < b[1] || origin.lat > b[3]) {
      toast('That address is outside the New York City map area — results will be thin.', 'warn', 6000);
    }
    refreshActiveTab();
    writeStateToUrl();
  }

  function runGeocode() {
    var query = $('addr').value.trim();
    if (query.length < 3) {
      toast('Type at least a few characters of an address.', 'warn');
      return;
    }
    var btn = $('geocode'), status = $('geocode-status');
    btn.disabled = true;
    status.className = 'status status--busy';
    status.textContent = 'Looking up “' + query + '”…';

    geocode(query).then(function (origin) {
      setOrigin(origin, 'Found: ' + origin.label + ' (via ' + origin.via + ')');
      if (state.tab === 'walk' && walkMap) walkMap.flyTo([origin.lat, origin.lon], 14);
    }).catch(function (err) {
      status.className = 'status status--error';
      var offline = err && (err.name === 'AbortError' || err.message === 'Failed to fetch');
      status.textContent = offline
        ? 'Geocoder unreachable. Use a quick place, or check your connection.'
        : 'No match for that address. Try adding the borough, or use a quick place.';
      toast(status.textContent, 'error', 6000);
    }).finally(function () { btn.disabled = false; });
  }

  function renderOriginBanner(id, emptyText) {
    var box = $(id);
    clear(box);
    if (!state.origin) {
      box.className = 'origin-banner origin-banner--empty';
      box.textContent = emptyText;
      return;
    }
    box.className = 'origin-banner';
    box.appendChild(h('strong', { text: 'Origin: ' }));
    box.appendChild(document.createTextNode(state.origin.label));
    box.appendChild(h('small', { text: state.origin.lat.toFixed(4) + ', ' + state.origin.lon.toFixed(4) }));
  }

  // --------------------------------------------------------- nearest stations

  function buildTable(columns, rows, emptyText) {
    if (!rows.length) return h('p', { class: 'table-empty', text: emptyText });
    var thead = h('thead', null, [h('tr', null, columns.map(function (c) {
      return h('th', { class: c.num ? 'num' : null, scope: 'col', text: c.label });
    }))]);
    var tbody = h('tbody', null, rows.map(function (row) {
      return h('tr', null, columns.map(function (c) {
        return h('td', { class: c.num ? 'num' : null }, [c.render(row)]);
      }));
    }));
    return h('table', { class: 'data' }, [thead, tbody]);
  }

  function renderNearest() {
    renderOriginBanner('nearest-origin',
      'No address yet — enter one in the sidebar, or pick a quick place.');

    var byLineBox = $('table-by-line'), overallBox = $('table-overall'), otherBox = $('table-unselected');
    [byLineBox, overallBox, otherBox].forEach(clear);

    if (!state.origin) {
      var msg = 'Set an address to fill this table.';
      byLineBox.appendChild(h('p', { class: 'table-empty', text: msg }));
      overallBox.appendChild(h('p', { class: 'table-empty', text: msg }));
      otherBox.appendChild(h('p', { class: 'table-empty', text: msg }));
      return;
    }

    var stations = selectedStations();
    var cap = state.walkCap;
    var withinCap = function (r) { return r.walk_min <= cap; };
    var noneMsg = 'No stations within the ' + cap + '-minute walk cap.';

    byLineBox.appendChild(buildTable([
      { label: 'Line', render: function (r) { return routeDot(r.line); } },
      { label: 'Station', render: function (r) { return r.name; } },
      { label: 'Lines served', render: function (r) { return routeList(r.lines); } },
      { label: 'Walk', num: true, render: function (r) { return fmt(r.walk_min, 1) + ' min'; } },
      { label: 'Distance', num: true, render: function (r) { return fmtInt(r.distance_m) + ' m'; } }
    ], NYC.nearestByLine(state.origin, stations, state.lines, 3).filter(withinCap),
      state.lines.length ? noneMsg : 'Select at least one line.'));

    var overallCols = [
      { label: 'Station', render: function (r) { return r.name; } },
      { label: 'Lines served', render: function (r) { return routeList(r.lines); } },
      { label: 'Borough', render: function (r) { return NYC.boroughName(r.borough); } },
      { label: 'Walk', num: true, render: function (r) { return fmt(r.walk_min, 1) + ' min'; } },
      { label: 'Distance', num: true, render: function (r) { return fmtInt(r.distance_m) + ' m'; } }
    ];

    overallBox.appendChild(buildTable(overallCols,
      NYC.nearestOverall(state.origin, stations, 3).filter(withinCap),
      state.lines.length ? noneMsg : 'Select at least one line.'));

    otherBox.appendChild(buildTable(overallCols,
      NYC.nearestFromUnselected(state.origin, STATIONS, state.lines, 3).filter(withinCap),
      state.lines.length ? 'No stations on other lines within the cap.' : 'Select at least one line.'));
  }

  // ------------------------------------------------------------- travel time

  var travelMap = null, travelLegend = null;
  var travelRasterRef = {}, travelOriginRef = {};
  var travelCache = null;

  function ensureTravelMap() {
    if (travelMap) { travelMap.invalidateSize(); return; }
    travelMap = baseMap('map-travel');
    travelMap.setView([40.75, -73.98], 12);
    travelLegend = legendControl().addTo(travelMap);
    originMarker(travelMap, travelOriginRef, state.origin);

    var readout = $('travel-readout');
    travelMap.on('mousemove', function (e) {
      if (!travelCache) return;
      var m = sampleRaster(travelCache.grid, travelCache.minutes, e.latlng);
      if (m == null || !isFinite(m)) { readout.hidden = true; return; }
      readout.hidden = false;
      var label = NYC.MODE_LABELS[travelCache.mode] || 'Travel';
      readout.textContent = m >= travelCache.cap
        ? 'Over ' + travelCache.cap + ' min by ' + label.toLowerCase()
        : fmt(m, 1) + ' min by ' + label.toLowerCase();
    });
    travelMap.on('mouseout', function () { readout.hidden = true; });
  }

  function renderTravelOrigin() {
    renderOriginBanner('travel-origin',
      'No address yet — set one in the sidebar, then generate a heatmap.');
  }

  function renderTravelMethod() {
    var mode = state.travel.mode;
    var canOsrm = !!OSRM[mode];
    var osrmBox = $('use-osrm');
    osrmBox.disabled = !canOsrm;

    var parts = ['Straight-line Manhattan distance at mode-specific NYC speeds (' +
      NYC.MODE_SPEEDS[mode] + ' m/s for ' + NYC.MODE_LABELS[mode].toLowerCase() + ').'];

    if (mode === 'metro') {
      parts.push('Metro adds three legs: walk to your nearest station, ride, then walk from the ' +
        'station nearest your destination. It ignores which lines actually connect.');
    }
    if (state.travel.rush) {
      parts.push('Rush hour multiplies times by ' + NYC.RUSH_MULT[mode] + '×.');
    }
    if (canOsrm) {
      parts.push(osrmBox.checked
        ? 'Road-network mode samples real OSRM durations and stretches the estimate to match — ' +
          'free public service, so it can be slow or unavailable.'
        : 'Tick “Use road network” to calibrate against real OSRM routing.');
    } else {
      parts.push('Road-network routing is not available for this mode (the free OSRM instances ' +
        'carry no transit data), so estimates are used.');
    }
    $('travel-method').textContent = parts.join(' ');
  }

  // Ask OSRM for real durations to a sample of grid points, and turn them into
  // a ratio field the straight-line raster can be stretched by.
  function osrmRatios(grid, origin, baseMinutes, mode) {
    var cfg = OSRM[mode];
    if (!cfg) return Promise.resolve(null);

    var pts = NYC.sampleGridPoints(grid, OSRM_SAMPLES);
    var coords = [origin.lon.toFixed(6) + ',' + origin.lat.toFixed(6)].concat(pts.map(function (p) {
      return p.lon.toFixed(6) + ',' + p.lat.toFixed(6);
    }));
    var url = cfg.base + '/table/v1/' + cfg.profile + '/' + coords.join(';') +
      '?sources=0&annotations=duration';

    return fetchJSON(url, 20000).then(function (res) {
      if (!res || res.code !== 'Ok' || !res.durations || !res.durations[0]) {
        throw new Error(res && res.message ? res.message : 'unexpected OSRM response');
      }
      var durations = res.durations[0];
      var out = [];
      pts.forEach(function (p, i) {
        var seconds = durations[i + 1];
        if (seconds == null || !isFinite(seconds)) return;
        var estimate = baseMinutes[p.row * grid.cols + p.col];
        if (!(estimate > 0.05)) return; // too close to the origin to form a ratio
        var ratio = (seconds / 60) / estimate;
        if (!isFinite(ratio)) return;
        out.push({ lat: p.lat, lon: p.lon, ratio: Math.min(6, Math.max(0.2, ratio)) });
      });
      return out.length >= 3 ? out : null;
    });
  }

  function generateTravelHeatmap() {
    if (!state.origin) {
      toast('Set an address in the sidebar first.', 'warn');
      return;
    }
    var stations = selectedStations();
    if (state.travel.mode === 'metro' && !stations.length) {
      toast('Metro mode needs at least one subway line selected.', 'warn');
      return;
    }

    ensureTravelMap();
    var mode = state.travel.mode, cap = state.travel.cap, rush = state.travel.rush;
    var origin = state.origin;
    var wantOsrm = state.travel.osrm && !!OSRM[mode];
    var btn = $('generate-travel'), wrap = $('map-travel').parentNode;

    // Size the local grid so it still reaches the time cap at this mode's speed.
    var maxRangeM = cap * 60 * NYC.effectiveSpeed(mode, rush);
    var degRange = (maxRangeM / NYC.M_PER_DEG_LAT) * 1.2;
    var bbox = [origin.lon - degRange, origin.lat - degRange, origin.lon + degRange, origin.lat + degRange];
    var grid = NYC.makeRasterGrid(bbox, Math.max(0.002, degRange / 60));

    btn.disabled = true;
    busyOverlay(wrap, true, 'Computing ' + NYC.MODE_LABELS[mode].toLowerCase() + ' times…');

    requestAnimationFrame(function () {
      requestAnimationFrame(function () {
        var minutes = NYC.travelTimeRaster(grid, origin, { mode: mode, rush: rush, stations: stations });
        var ratios = null;

        var finish = function (note) {
          NYC.applyRatioField(grid, minutes, ratios);
          travelCache = { grid: grid, minutes: minutes, cap: cap, mode: mode };
          drawRaster(travelMap, travelRasterRef, grid, minutes, cap);
          originMarker(travelMap, travelOriginRef, origin);
          travelLegend.set(NYC.MODE_LABELS[mode] + ' time from origin', cap, note);
          travelMap.flyTo([origin.lat, origin.lon], mode === 'walk' ? 14 : 13);
          busyOverlay(wrap, false);
          btn.disabled = false;
        };

        if (!wantOsrm) {
          finish(rush ? 'Estimate · rush hour' : 'Estimate');
          return;
        }

        busyOverlay(wrap, true, 'Asking OSRM for road-network times…');
        osrmRatios(grid, origin, minutes, mode).then(function (r) {
          ratios = r;
          if (r) {
            var mean = r.reduce(function (a, b) { return a + b.ratio; }, 0) / r.length;
            finish('OSRM-calibrated · ' + r.length + ' samples · ' + fmt(mean, 2) + '× straight line');
            toast('Calibrated against ' + r.length + ' OSRM road-network samples.', null, 4000);
          } else {
            finish('Estimate (OSRM returned nothing usable)');
            toast('OSRM had no usable routes here — showing straight-line estimates.', 'warn', 6000);
          }
        }).catch(function (err) {
          finish('Estimate (OSRM unavailable)');
          toast('Road-network routing unavailable (' + (err.message || 'request failed') +
            '). Showing straight-line estimates.', 'warn', 7000);
        });
      });
    });
  }

  // --------------------------------------------------------- reference table

  var refState = { sort: 'distance_m', dir: 1, page: 0, size: 25, query: '', userSorted: false };

  function referenceRows() {
    var stations = selectedStations();
    var rows = state.origin
      ? NYC.withDistances(state.origin, stations)
      : stations.map(function (s) {
        return Object.assign({ distance_m: NaN, walk_min: NaN }, s);
      });

    if (refState.query) {
      var q = refState.query.toLowerCase();
      rows = rows.filter(function (r) {
        return r.name.toLowerCase().indexOf(q) !== -1 ||
          r.lines.toLowerCase().indexOf(q) !== -1 ||
          NYC.boroughName(r.borough).toLowerCase().indexOf(q) !== -1;
      });
    }

    var key = refState.sort, dir = refState.dir;
    rows.sort(function (a, b) {
      var x = a[key], y = b[key];
      if (typeof x === 'string') return dir * x.localeCompare(y);
      if (isNaN(x) && isNaN(y)) return 0;
      if (isNaN(x)) return 1;
      if (isNaN(y)) return -1;
      return dir * (x - y);
    });
    return rows;
  }

  function renderReference() {
    renderOriginBanner('reference-origin',
      'No address yet — the table lists every station on your lines; set an address to add distances.');

    var box = $('reference-table');
    clear(box);

    var hasOrigin = !!state.origin;
    if (!hasOrigin && (refState.sort === 'distance_m' || refState.sort === 'walk_min')) {
      refState.sort = 'name';
      refState.dir = 1;
    }

    var columns = [
      { key: 'name', label: 'Station', render: function (r) { return r.name; } },
      { key: 'lines', label: 'Lines', render: function (r) { return routeList(r.lines); } },
      { key: 'borough', label: 'Borough', render: function (r) { return NYC.boroughName(r.borough); } },
      {
        key: 'ada', label: 'ADA', render: function (r) {
          return h('span', {
            class: 'ada' + (r.ada ? '' : ' ada--none'),
            title: r.ada === 2 ? 'Partially accessible' : r.ada ? 'Accessible' : 'Not accessible',
            text: r.ada === 2 ? 'Partial' : r.ada ? 'Yes' : '—'
          });
        }
      }
    ];
    if (hasOrigin) {
      columns.push({
        key: 'distance_m', label: 'Distance (m)', num: true,
        render: function (r) { return fmtInt(r.distance_m); }
      });
      columns.push({
        key: 'walk_min', label: 'Walk (min)', num: true,
        render: function (r) { return fmt(r.walk_min, 1); }
      });
    }
    columns.push({ key: 'lat', label: 'Latitude', num: true, render: function (r) { return r.lat.toFixed(5); } });
    columns.push({ key: 'lon', label: 'Longitude', num: true, render: function (r) { return r.lon.toFixed(5); } });

    var rows = referenceRows();
    var pages = Math.max(1, Math.ceil(rows.length / refState.size));
    refState.page = Math.min(refState.page, pages - 1);
    var slice = rows.slice(refState.page * refState.size, (refState.page + 1) * refState.size);

    $('ref-count').textContent = rows.length
      ? rows.length.toLocaleString() + ' station' + (rows.length === 1 ? '' : 's')
      : 'no matches';

    if (!rows.length) {
      box.appendChild(h('p', {
        class: 'table-empty',
        text: state.lines.length ? 'Nothing matches that filter.' : 'Select at least one line.'
      }));
      clear($('ref-pager'));
      return;
    }

    var thead = h('thead', null, [h('tr', null, columns.map(function (c) {
      var active = refState.sort === c.key;
      return h('th', {
        class: 'sortable' + (c.num ? ' num' : ''),
        scope: 'col',
        'aria-sort': active ? (refState.dir === 1 ? 'ascending' : 'descending') : 'none',
        text: c.label,
        onclick: function () {
          if (active) refState.dir *= -1;
          else { refState.sort = c.key; refState.dir = 1; }
          refState.userSorted = true;
          refState.page = 0;
          renderReference();
        }
      });
    }))]);

    var tbody = h('tbody', null, slice.map(function (r) {
      return h('tr', null, columns.map(function (c) {
        var v = c.render(r);
        return h('td', { class: c.num ? 'num' : null }, [v]);
      }));
    }));

    box.appendChild(h('table', { class: 'data' }, [thead, tbody]));
    renderPager(rows.length, pages);
  }

  function renderPager(total, pages) {
    var pager = $('ref-pager');
    clear(pager);
    var from = refState.page * refState.size + 1;
    var to = Math.min(total, (refState.page + 1) * refState.size);

    pager.appendChild(h('button', {
      type: 'button', class: 'btn btn--ghost', text: '‹ Prev',
      disabled: refState.page === 0,
      onclick: function () { refState.page--; renderReference(); }
    }));
    pager.appendChild(h('span', {
      text: from.toLocaleString() + '–' + to.toLocaleString() + ' of ' + total.toLocaleString() +
        ' · page ' + (refState.page + 1) + ' of ' + pages
    }));
    pager.appendChild(h('button', {
      type: 'button', class: 'btn btn--ghost', text: 'Next ›',
      disabled: refState.page >= pages - 1,
      onclick: function () { refState.page++; renderReference(); }
    }));
  }

  function downloadReferenceCsv() {
    var rows = referenceRows();
    if (!rows.length) { toast('Nothing to download.', 'warn'); return; }

    var header = ['station', 'lines', 'borough', 'ada', 'latitude', 'longitude'];
    if (state.origin) header.push('distance_m', 'walk_min');

    var esc = function (v) {
      var s = String(v == null ? '' : v);
      return /[",\n]/.test(s) ? '"' + s.replace(/"/g, '""') + '"' : s;
    };
    var lines = [header.join(',')];
    rows.forEach(function (r) {
      var cells = [r.name, r.lines, NYC.boroughName(r.borough), r.ada, r.lat.toFixed(6), r.lon.toFixed(6)];
      if (state.origin) cells.push(Math.round(r.distance_m), r.walk_min.toFixed(1));
      lines.push(cells.map(esc).join(','));
    });

    var blob = new Blob([lines.join('\n')], { type: 'text/csv;charset=utf-8' });
    var url = URL.createObjectURL(blob);
    var a = h('a', { href: url, download: 'nyc-stations-' + state.lines.join('') + '.csv' });
    document.body.appendChild(a);
    a.click();
    a.remove();
    setTimeout(function () { URL.revokeObjectURL(url); }, 1000);
  }

  // -------------------------------------------------------------------- wire

  function renderQuickPlaces() {
    var box = $('quick-places');
    clear(box);
    QUICK_PLACES.forEach(function (p) {
      box.appendChild(h('button', {
        type: 'button', class: 'chip', text: p.label,
        onclick: function () {
          $('addr').value = '';
          setOrigin({ lat: p.lat, lon: p.lon, label: p.name }, 'Using ' + p.label + '.');
          if (state.tab === 'walk' && walkMap) walkMap.flyTo([p.lat, p.lon], 14);
        }
      }));
    });
  }

  function syncControlsFromState() {
    $('walk-cap').value = state.walkCap;
    $('walk-cap-out').textContent = state.walkCap + ' min';
    $('grid-res').value = state.gridRes;
    $('grid-res-out').textContent = state.gridRes.toFixed(3) + '°';
    $('travel-mode').value = state.travel.mode;
    $('travel-cap').value = state.travel.cap;
    $('travel-cap-out').textContent = state.travel.cap + ' min';
    $('rush-hour').checked = state.travel.rush;
    $('use-osrm').checked = state.travel.osrm;
  }

  function bindEvents() {
    TABS.forEach(function (t) {
      $('tab-' + t).addEventListener('click', function () { showTab(t); });
    });

    // Arrow-key navigation across the tab strip, per the ARIA tabs pattern.
    $('tab-walk').parentNode.addEventListener('keydown', function (e) {
      var i = TABS.indexOf(state.tab);
      if (e.key === 'ArrowRight') i = (i + 1) % TABS.length;
      else if (e.key === 'ArrowLeft') i = (i - 1 + TABS.length) % TABS.length;
      else return;
      e.preventDefault();
      showTab(TABS[i]);
      $('tab-' + TABS[i]).focus();
    });

    $('lines-all').addEventListener('click', function () { setLines(DATA.routes); });
    $('lines-none').addEventListener('click', function () { setLines([]); });
    $('lines-default').addEventListener('click', function () { setLines(DEFAULT_LINES); });

    $('geocode').addEventListener('click', runGeocode);
    $('addr').addEventListener('keydown', function (e) {
      if (e.key === 'Enter') { e.preventDefault(); runGeocode(); }
    });

    $('walk-cap').addEventListener('input', function () {
      state.walkCap = parseInt(this.value, 10);
      $('walk-cap-out').textContent = state.walkCap + ' min';
      if (state.tab === 'walk') scheduleWalkRaster();
      else if (state.tab === 'nearest') renderNearest();
      writeStateToUrl();
    });

    $('grid-res').addEventListener('input', function () {
      state.gridRes = parseFloat(this.value);
      $('grid-res-out').textContent = state.gridRes.toFixed(3) + '°';
      walkCache = null;
      if (state.tab === 'walk') scheduleWalkRaster();
      writeStateToUrl();
    });

    $('travel-mode').addEventListener('change', function () {
      state.travel.mode = this.value;
      renderTravelMethod();
      writeStateToUrl();
    });
    $('travel-cap').addEventListener('input', function () {
      state.travel.cap = parseInt(this.value, 10);
      $('travel-cap-out').textContent = state.travel.cap + ' min';
      writeStateToUrl();
    });
    $('rush-hour').addEventListener('change', function () {
      state.travel.rush = this.checked;
      renderTravelMethod();
      writeStateToUrl();
    });
    $('use-osrm').addEventListener('change', function () {
      state.travel.osrm = this.checked;
      renderTravelMethod();
    });
    $('generate-travel').addEventListener('click', generateTravelHeatmap);

    $('ref-search').addEventListener('input', debounce(function () {
      refState.query = $('ref-search').value.trim();
      refState.page = 0;
      renderReference();
    }, 180));

    $('ref-page-size').addEventListener('change', function () {
      refState.size = parseInt(this.value, 10);
      refState.page = 0;
      renderReference();
    });
    $('ref-download').addEventListener('click', downloadReferenceCsv);

    window.addEventListener('resize', debounce(function () {
      if (walkMap && state.tab === 'walk') walkMap.invalidateSize();
      if (travelMap && state.tab === 'travel') travelMap.invalidateSize();
    }, 200));
  }

  function init() {
    readStateFromUrl();
    syncControlsFromState();
    renderRoutePicker();
    renderQuickPlaces();
    bindEvents();

    $('data-credit').textContent = 'Station data: ' + DATA.count + ' stations across ' +
      DATA.routes.length + ' routes, bundled from ' + DATA.source + '.';

    if (state.origin) {
      $('geocode-status').textContent = state.origin.label;
    }
    showTab(state.tab);
  }

  init();
})();
