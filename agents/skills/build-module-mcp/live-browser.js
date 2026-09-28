// Keep one headless browser page on a module so MCP input updates can
// round-trip through it. The page stays attached (it renders), unlike a
// headless Chrome with no client.
//
// Usage:
//   node live-browser.js <url> [control_dir]
//     <url>          e.g. http://127.0.0.1:17299/?module=reference_module
//     [control_dir]  default: current directory
//   touch <control_dir>/shot   -> screenshot to <control_dir>/screenshot.png
//   touch <control_dir>/stop   -> close the browser and exit
//
// Environment overrides:
//   PLAYWRIGHT_CORE  path to a playwright-core package directory
//   CHROMIUM_PATH    path to a Chromium / Chrome executable

const fs = require('fs');
const os = require('os');
const path = require('path');

function firstExisting(candidates) {
  return candidates.find(p => p && fs.existsSync(p));
}

function listDirs(dir) {
  try {
    return fs.readdirSync(dir).map(d => path.join(dir, d));
  } catch (e) {
    return [];
  }
}

function findPlaywrightCore() {
  if (process.env.PLAYWRIGHT_CORE) return process.env.PLAYWRIGHT_CORE;
  const npx = listDirs(path.join(os.homedir(), '.npm', '_npx'))
    .map(d => path.join(d, 'node_modules', 'playwright-core'));
  return firstExisting(npx);
}

function findChromium() {
  if (process.env.CHROMIUM_PATH) return process.env.CHROMIUM_PATH;
  const caches = [
    path.join(os.homedir(), 'Library', 'Caches', 'ms-playwright'),
    path.join(os.homedir(), '.cache', 'ms-playwright')
  ];
  const candidates = [];
  for (const cache of caches) {
    for (const dir of listDirs(cache)) {
      if (!/chromium-\d+$/.test(dir)) continue;
      candidates.push(
        path.join(dir, 'chrome-mac-arm64', 'Google Chrome for Testing.app',
                  'Contents', 'MacOS', 'Google Chrome for Testing'),
        path.join(dir, 'chrome-mac', 'Chromium.app', 'Contents', 'MacOS', 'Chromium'),
        path.join(dir, 'chrome-linux', 'chrome')
      );
    }
  }
  return firstExisting(candidates);
}

const url = process.argv[2];
const controlDir = process.argv[3] || process.cwd();
if (!url) {
  console.error('Usage: node live-browser.js <url> [control_dir]');
  process.exit(2);
}

const corePath = findPlaywrightCore();
const chromiumPath = findChromium();
if (!corePath || !chromiumPath) {
  console.error('Cannot find playwright-core (' + corePath + ') or Chromium (' +
                chromiumPath + '). Set PLAYWRIGHT_CORE / CHROMIUM_PATH.');
  process.exit(2);
}
const { chromium } = require(corePath);

(async () => {
  const browser = await chromium.launch({ headless: true, executablePath: chromiumPath });
  const page = await browser.newPage({ viewport: { width: 1600, height: 1000 } });
  page.on('console', msg => { if (msg.type() === 'error') console.log('[console]', msg.text()); });
  page.on('pageerror', err => console.log('[pageerror]', err.message));
  await page.goto(url, { waitUntil: 'load' });
  console.log('opened', url);

  const stopFile = path.join(controlDir, 'stop');
  const shotFile = path.join(controlDir, 'shot');
  while (!fs.existsSync(stopFile)) {
    if (fs.existsSync(shotFile)) {
      fs.unlinkSync(shotFile);
      await page.screenshot({ path: path.join(controlDir, 'screenshot.png') });
      console.log('screenshot taken');
    }
    await page.waitForTimeout(500);
  }
  fs.unlinkSync(stopFile);
  await browser.close();
  console.log('closed');
})().catch(e => { console.error(e); process.exit(1); });
