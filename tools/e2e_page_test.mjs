// Browser end-to-end suite for docs/index.html.
//
// Needs playwright (not a repo dependency — install on demand):
//   npm i -D playwright && npx playwright install chromium
// Serve docs/ first (any static server):
//   npx http-server docs -p 8099
// Then:
//   node tools/e2e_page_test.mjs
//
// Env: BASE (default http://127.0.0.1:8099), PW_EXECUTABLE (chromium binary,
// optional), PW_MODULE (path to a playwright module, when it isn't installed
// locally), SHOTS (screenshot dir, default .e2e-shots — gitignored).
import { mkdirSync } from 'node:fs';

const { chromium } = await import(process.env.PW_MODULE || 'playwright');

const BASE = process.env.BASE || 'http://127.0.0.1:8099';
const SHOTS = process.env.SHOTS || '.e2e-shots';
mkdirSync(SHOTS, { recursive: true });

const results = [];
const check = (name, cond, detail = '') => {
  results.push({ name, ok: !!cond, detail });
  console.log(`${cond ? '  ok  ' : ' FAIL '} ${name}${detail ? ' — ' + detail : ''}`);
};

const browser = await chromium.launch({
  executablePath: process.env.PW_EXECUTABLE || undefined,
  args: process.env.CI || process.env.PW_EXECUTABLE ? ['--no-sandbox'] : []
});
const page = await browser.newPage({ viewport: { width: 1440, height: 1000 } });

const pageErrors = [];
const consoleErrors = [];
page.on('pageerror', (e) => pageErrors.push(String(e)));
page.on('console', (m) => {
  if (m.type() !== 'error') return;
  const t = m.text();
  // Tile + geocoder hosts are blocked in this sandbox; those failures are expected.
  if (/basemaps\.cartocdn|photon\.komoot|nominatim|openstreetmap|ERR_|Failed to load resource/.test(t)) return;
  consoleErrors.push(t);
});

// Stub out the network the sandbox can't reach so the page renders offline.
await page.route('**://*.basemaps.cartocdn.com/**', (r) => r.abort());

console.log('\n— load —');
await page.goto(BASE + '/index.html', { waitUntil: 'load' });
await page.waitForTimeout(1200);

check('page title', (await page.title()).includes('navigating nyc'), await page.title());
check('no uncaught JS errors', pageErrors.length === 0, pageErrors.join(' | '));
check('no unexpected console errors', consoleErrors.length === 0, consoleErrors.join(' | '));

console.log('\n— hardening —');
const cspViolations = await page.evaluate(() => new Promise((resolve) => {
  const hits = [];
  document.addEventListener('securitypolicyviolation', (e) => hits.push(e.violatedDirective + ':' + e.blockedURI));
  setTimeout(() => resolve(hits), 400);
}));
check('no CSP violations from own assets', cspViolations.length === 0, cspViolations.join(' | '));
const fontOk = await page.evaluate(() => document.fonts.check('700 16px "Space Mono"'));
check('self-hosted Space Mono is active', fontOk);
const bodyFont = await page.evaluate(() => getComputedStyle(document.body).fontFamily);
check('body runs in mono (mode-data)', /Space Mono/i.test(bodyFont), bodyFont);
const bodyBg = await page.evaluate(() => getComputedStyle(document.body).backgroundColor);
check('light-mode paper ground', bodyBg === 'rgb(242, 240, 233)', bodyBg);
check('referrer policy is no-referrer', await page.evaluate(() => document.referrer !== undefined &&
  document.querySelector('meta[name="referrer"]').content === 'no-referrer'));

console.log('\n— sidebar —');
const routeCount = await page.locator('#route-picker .route').count();
check('route picker renders all routes', routeCount === 24, `${routeCount} buttons`);
const pressed = await page.locator('#route-picker .route[aria-pressed="true"]').allTextContents();
check('defaults to A/C/L', pressed.join('') === 'ACL', pressed.join(','));
check('lines summary counts stations',
  /3 lines · 90 stations/.test(await page.locator('#lines-summary').textContent()),
  await page.locator('#lines-summary').textContent());
check('data credit shown', (await page.locator('#data-credit').textContent()).includes('493 stations'));

console.log('\n— walkability tab —');
await page.waitForSelector('img.raster-overlay', { timeout: 8000 });
const rasterSrc = await page.locator('img.raster-overlay').getAttribute('src');
check('raster overlay is a rendered PNG', rasterSrc.startsWith('data:image/png'), `${rasterSrc.length} chars`);
check('raster sits in its own pane below markers',
  await page.locator('.leaflet-raster-pane img.raster-overlay').count() === 1);
const statCount = await page.locator('#walk-stats .stat').count();
check('coverage stats render', statCount === 8, `${statCount} tiles`);
const tileTexts = await page.locator('#walk-stats .stat').allTextContents();
const tiles = tileTexts.map((t) => t.replace(/\s+/g, ' ').trim());
check('first tile is the share inside the cap', /^areawithin15min[\d.]+%$/i.test(tiles[0].replace(/\s/g, '')), tiles[0]);
const share = parseFloat(tiles[0].match(/([\d.]+)\s*%/)[1]);
check('share inside cap is a small but real fraction', share > 0.5 && share < 25, share + '%');
const med = parseFloat(tiles[1].match(/([\d.]+)/)[1]);
check('median inside the cap is under the cap', med > 0 && med < 15, tiles[1]);
check('station count tile matches the selection', /^stationsmapped90$/i.test(tiles[3].replace(/\s/g, '')), tiles[3]);
const within5 = parseFloat(tiles[4].match(/([\d.]+)\s*%/)[1]);
const within20 = parseFloat(tiles[7].match(/([\d.]+)\s*%/)[1]);
check('threshold shares increase with time', within5 < within20, `5min=${within5}% 20min=${within20}%`);
console.log('    tiles:', tiles.join(' | '));
const stationDots = await page.locator('#map-walk canvas').count();
check('station markers use a canvas renderer', stationDots >= 1);

await page.screenshot({ path: `${SHOTS}/1-walkability.png` });

console.log('\n— line selection —');
await page.locator('#lines-all').click();
await page.waitForTimeout(900);
check('select-all picks every route',
  await page.locator('#route-picker .route[aria-pressed="true"]').count() === 24);
check('summary updates for all lines',
  (await page.locator('#lines-summary').textContent()).includes('493 stations'),
  await page.locator('#lines-summary').textContent());
await page.screenshot({ path: `${SHOTS}/2-all-lines.png` });

await page.locator('#lines-none').click();
await page.waitForTimeout(500);
check('clearing lines is handled gracefully',
  (await page.locator('#lines-summary').textContent()).toLowerCase().includes('no lines selected'));
check('empty selection removes the raster', await page.locator('img.raster-overlay').count() === 0);
check('empty selection still has no JS errors', pageErrors.length === 0, pageErrors.join(' | '));

await page.locator('#lines-default').click();
await page.waitForTimeout(900);
check('reset restores A/C/L',
  (await page.locator('#route-picker .route[aria-pressed="true"]').allTextContents()).join('') === 'ACL');

console.log('\n— walk cap + fidelity —');
await page.locator('#walk-cap').fill('8');
await page.waitForTimeout(700);
check('walk cap output updates', (await page.locator('#walk-cap-out').textContent()) === '8 min');
await page.locator('#grid-res').fill('0.002');
await page.waitForTimeout(2500);
check('fidelity output updates', (await page.locator('#grid-res-out').textContent()) === '0.002°');
check('finest grid still renders', await page.locator('img.raster-overlay').count() === 1);
check('fine grid produced no errors', pageErrors.length === 0, pageErrors.join(' | '));
await page.screenshot({ path: `${SHOTS}/3-fine-8min.png` });
await page.locator('#grid-res').fill('0.004');
await page.locator('#walk-cap').fill('15');
await page.waitForTimeout(900);

console.log('\n— origin via quick place —');
await page.locator('#quick-places .chip', { hasText: 'Times Square' }).click();
await page.waitForTimeout(1200);
check('geocode status reports the quick place',
  (await page.locator('#geocode-status').textContent()).includes('Times Square'));
check('url carries shareable state', (await page.evaluate(() => location.hash)).includes('lat=40.758'),
  await page.evaluate(() => location.hash));

console.log('\n— nearest stations tab —');
await page.locator('#tab-nearest').click();
await page.waitForTimeout(400);
check('origin banner filled', (await page.locator('#nearest-origin').textContent()).includes('Times Square'));
const byLineRows = await page.locator('#table-by-line tbody tr').count();
check('by-line table has rows', byLineRows > 0, `${byLineRows} rows`);
const firstOverall = await page.locator('#table-overall tbody tr').first().textContent();
check('nearest overall is Port Authority',
  firstOverall.includes('42 St-Port Authority Bus Terminal'), firstOverall.replace(/\s+/g, ' ').trim());
const otherRows = await page.locator('#table-unselected tbody tr').count();
check('other-lines table has rows', otherRows === 3, `${otherRows} rows`);
check('route bullets render in tables', await page.locator('#table-overall .route-dot').count() > 0);
await page.screenshot({ path: `${SHOTS}/4-nearest.png`, fullPage: true });

console.log('\n— walk cap filters the tables —');
await page.locator('#walk-cap').fill('6');
await page.waitForTimeout(500);
const cappedRows = await page.locator('#table-by-line tbody tr').count();
check('tighter cap drops far stations', cappedRows < byLineRows, `${cappedRows} rows at 6 min vs ${byLineRows} at 15`);
await page.locator('#walk-cap').fill('15');
await page.waitForTimeout(500);

console.log('\n— travel time tab —');
await page.locator('#tab-travel').click();
await page.waitForTimeout(400);
check('travel method text explains the model',
  (await page.locator('#travel-method').textContent()).includes('manhattan distance'));
await page.locator('#generate-travel').click();
await page.waitForTimeout(2500);
check('travel raster rendered', await page.locator('#map-travel img.raster-overlay').count() === 1);
await page.screenshot({ path: `${SHOTS}/5-travel-walk.png` });

await page.selectOption('#travel-mode', 'metro');
await page.waitForTimeout(200);
check('metro explains the three-leg model',
  (await page.locator('#travel-method').textContent()).includes('three legs'));
check('osrm disabled for metro', await page.locator('#use-osrm').isDisabled());
await page.locator('#travel-cap').fill('40');
await page.locator('#generate-travel').click();
await page.waitForTimeout(3500);
check('metro raster rendered', await page.locator('#map-travel img.raster-overlay').count() === 1);
check('travel legend labels the mode',
  (await page.locator('#map-travel .map-legend').textContent()).includes('metro time'),
  (await page.locator('#map-travel .map-legend').textContent()).replace(/\s+/g, ' ').trim());
await page.screenshot({ path: `${SHOTS}/6-travel-metro.png` });

await page.selectOption('#travel-mode', 'car');
await page.locator('#rush-hour').check();
await page.waitForTimeout(200);
check('rush hour multiplier surfaced',
  (await page.locator('#travel-method').textContent()).includes('2×'));
check('osrm re-enabled for car', !(await page.locator('#use-osrm').isDisabled()));

console.log('\n— reference table —');
await page.locator('#tab-reference').click();
await page.waitForTimeout(500);
const refRows = await page.locator('#reference-table tbody tr').count();
check('reference table paginates to 25', refRows === 25, `${refRows} rows`);
check('reference count shows total',
  (await page.locator('#ref-count').textContent()).includes('90'),
  await page.locator('#ref-count').textContent());
const firstRef = await page.locator('#reference-table tbody tr').first().textContent();
check('sorted by distance from origin',
  firstRef.includes('42 St-Port Authority'), firstRef.replace(/\s+/g, ' ').trim());

await page.locator('#ref-search').fill('brooklyn');
await page.waitForTimeout(500);
const brooklynRows = await page.locator('#reference-table tbody tr').count();
check('filter narrows the table', brooklynRows > 0 && brooklynRows <= 25, `${brooklynRows} rows`);
const boroughCells = await page.locator('#reference-table tbody tr td:nth-child(3)').allTextContents();
check('filter matches on borough', boroughCells.every((c) => c === 'Brooklyn'), boroughCells.slice(0, 3).join(','));
await page.locator('#ref-search').fill('');
await page.waitForTimeout(400);

await page.locator('#reference-table th', { hasText: 'Station' }).click();
await page.waitForTimeout(300);
check('clicking a header sorts',
  (await page.locator('#reference-table th').first().getAttribute('aria-sort')) === 'ascending');
await page.locator('#reference-table th').first().click();
await page.waitForTimeout(300);
check('clicking again reverses',
  (await page.locator('#reference-table th').first().getAttribute('aria-sort')) === 'descending');

await page.locator('#ref-pager button', { hasText: 'Next' }).click();
await page.waitForTimeout(300);
check('pager advances', (await page.locator('#ref-pager span').textContent()).includes('page 2 of'),
  await page.locator('#ref-pager span').textContent());
await page.selectOption('#ref-page-size', '50');
await page.waitForTimeout(300);
check('page size applies', await page.locator('#reference-table tbody tr').count() === 50);
await page.screenshot({ path: `${SHOTS}/7-reference.png` });

console.log('\n— csv download —');
const [download] = await Promise.all([
  page.waitForEvent('download', { timeout: 5000 }),
  page.locator('#ref-download').click()
]);
check('csv download starts', download.suggestedFilename().endsWith('.csv'), download.suggestedFilename());

console.log('\n— shareable url reload —');
const hash = await page.evaluate(() => location.hash);
const page2 = await browser.newPage({ viewport: { width: 1280, height: 900 } });
const errs2 = [];
page2.on('pageerror', (e) => errs2.push(String(e)));
await page2.route('**://*.basemaps.cartocdn.com/**', (r) => r.abort());
await page2.goto(BASE + '/index.html' + hash, { waitUntil: 'load' });
await page2.waitForTimeout(1500);
check('state restores from url: tab', await page2.locator('#tab-reference').getAttribute('aria-selected') === 'true');
check('state restores from url: origin',
  (await page2.locator('#reference-origin').textContent()).includes('Times Square'));
check('state restores from url: lines',
  (await page2.locator('#route-picker .route[aria-pressed="true"]').allTextContents()).join('') === 'ACL');
check('reload has no JS errors', errs2.length === 0, errs2.join(' | '));

console.log('\n— mobile layout —');
const page3 = await browser.newPage({ viewport: { width: 390, height: 844 }, isMobile: true, hasTouch: true });
const errs3 = [];
page3.on('pageerror', (e) => errs3.push(String(e)));
await page3.route('**://*.basemaps.cartocdn.com/**', (r) => r.abort());
await page3.goto(BASE + '/index.html', { waitUntil: 'load' });
await page3.waitForTimeout(2000);
const overflow = await page3.evaluate(() =>
  document.documentElement.scrollWidth - document.documentElement.clientWidth);
check('no horizontal overflow on mobile', overflow <= 0, `${overflow}px`);
check('mobile renders the raster', await page3.locator('img.raster-overlay').count() === 1);
check('mobile has no JS errors', errs3.length === 0, errs3.join(' | '));
await page3.screenshot({ path: `${SHOTS}/8-mobile.png`, fullPage: true });

console.log('\n— geocoder failure path —');
await page.route('**photon.komoot.io**', (r) => r.abort());
await page.route('**nominatim.openstreetmap.org**', (r) => r.abort());
await page.locator('#tab-walk').click();
await page.locator('#addr').fill('123 Fake Street, Brooklyn');
await page.locator('#geocode').click();
await page.waitForTimeout(2500);
const status = await page.locator('#geocode-status').textContent();
check('geocoder failure is reported, not silent', /unreachable|No match/i.test(status), status.trim());
check('geocode button re-enables after failure', !(await page.locator('#geocode').isDisabled()));
check('failure path throws no JS errors', pageErrors.length === 0, pageErrors.join(' | '));

console.log('\n— osrm failure path —');
await page.route('**routing.openstreetmap.de**', (r) => r.abort());
await page.locator('#tab-travel').click();
await page.locator('#use-osrm').check();
await page.selectOption('#travel-mode', 'walk');
await page.locator('#generate-travel').click();
await page.waitForTimeout(4000);
check('osrm failure falls back to estimates',
  (await page.locator('#map-travel .map-legend').textContent()).includes('osrm unavailable'),
  (await page.locator('#map-travel .map-legend').textContent()).replace(/\s+/g, ' ').trim());
check('fallback still draws a raster', await page.locator('#map-travel img.raster-overlay').count() === 1);
check('generate button re-enabled', !(await page.locator('#generate-travel').isDisabled()));
check('osrm failure throws no JS errors', pageErrors.length === 0, pageErrors.join(' | '));

await browser.close();

const failed = results.filter((r) => !r.ok);
console.log(`\n${'='.repeat(60)}`);
console.log(`${results.length - failed.length}/${results.length} checks passed`);
if (failed.length) {
  failed.forEach((f) => console.log(`  FAIL: ${f.name} — ${f.detail}`));
  process.exit(1);
}
