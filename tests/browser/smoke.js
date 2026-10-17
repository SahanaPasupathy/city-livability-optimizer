// Smoke test of the real Shiny app with its original interface: the logo,
// questionnaire, presets, results (numbers compared with values computed in R),
// tabs, ranking table, map, low-confidence switch, report, invalid input, and the
// added tabs (Trade-offs, Robustness, What if, States & UTs, News).
// It checks that the app and its calculations work, not how the interface looks.
const { chromium } = require('playwright');
const fs = require('fs');
const URL = process.env.APP_URL || 'http://127.0.0.1:7654/'; const OUT = process.argv[2] || require('os').tmpdir();
const res = []; const ok = (n, c, x = '') => res.push(`${c ? 'PASS' : 'FAIL'}  ${n}${x ? '  -  ' + x : ''}`);
const idle = async (p) => { await p.waitForTimeout(150); await p.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy'), null, { timeout: 20000 }); await p.waitForTimeout(300); };
const names = (p) => p.$$eval('.tc-name', e => e.map(x => x.textContent.trim()));
const answerAll = async (p, values) => { for (const v of values) { await p.click(`.scale-btn >> nth=${v - 1}`); await idle(p); } };

(async () => {
  const b = await chromium.launch();
  const ctx = await b.newContext({ viewport: { width: 1280, height: 900 }, acceptDownloads: true });
  const p = await ctx.newPage(); const errors = [];
  p.on('console', m => { if (m.type() === 'error') errors.push(m.text()); }); p.on('pageerror', e => errors.push(String(e)));
  await p.goto(URL); await p.waitForSelector('.hero h1'); await idle(p);

  ok('welcome: original title and the custom logo', (await p.textContent('.hero h1')) === 'City Livability Optimizer' &&
     (await p.getAttribute('.hero .logo img', 'src')) === 'img/logo.svg' && await p.$eval('.hero .logo img', i => i.complete && i.naturalWidth > 0));
  ok('welcome: logo is also the browser-tab icon', (await p.getAttribute('link[rel="icon"]', 'href')) === 'img/logo.svg');
  ok('welcome: original layout (Start button, three presets, skip link)', (await p.$$('.btn-main')).length === 1 &&
     (await p.$$('.preset-btn')).length === 3 && (await p.textContent('.skip-link')).includes('Skip the questions'));
  ok('redesign removed: no redesigned header, nav, hero photo, catalogue, atlas or news', (await p.$$('.site-header, .site-nav, .nav-btn, .hero-img, .cat-grid, .pulse, .whatif')).length === 0);
  ok('original styles loaded', await p.$eval('body', e => getComputedStyle(e).backgroundImage.includes('gradient')));

  // Questionnaire: a double click must answer only the current question.
  await p.click('.btn-main'); await idle(p);
  ok('questions: first question is Education', (await p.textContent('.fc-title')).includes('Education'));
  await p.dblclick('.scale-btn >> nth=3'); await idle(p);
  ok('questions: a double click answers only the current question', (await p.textContent('.fc-title')).includes('Safety'));
  await answerAll(p, [3, 5, 2, 4, 1, 3]);
  ok('questions: seven answers open the results', (await p.textContent('.result-head h2')).includes('Your livability matches'));

  // Results for answers 4,3,5,2,4,1,3, compared with R.
  const n1 = await names(p); const m = await p.$$eval('.match-label', e => e.map(x => x.textContent));
  ok('results: top 3 match R (Delhi, Kerala, Chandigarh)', n1.join('|') === 'Delhi|Kerala|Chandigarh', n1.join(', '));
  ok('results: match scores match R (0.791, 0.790, 0.784)', m[0].includes('0.791') && m[1].includes('0.790') && m[2].includes('0.784'));
  ok('results: original tabs plus the added ones', (await p.$$eval('#result_tabs .nav-link', e => e.map(x => x.textContent.trim()))).join('|') ===
     'Top 3|Map|Full ranking|My weights|Trade-offs|Robustness|What if|States & UTs|News|How it works|About and limits');
  await p.click('#result_tabs .nav-link >> text=My weights'); await idle(p);
  const w = await p.$$eval('.w-row', r => r.map(x => x.textContent.replace(/\s+/g, ' ').trim()));
  ok('weights: AHP weights match R (Health 39.2%, Environment 2.3%)', w[0].endsWith('Health 39.2%') && w[6].endsWith('Environment 2.3%'), w[0] + ' / ' + w[6]);
  ok('weights: consistency ratio matches R (0.036)', (await p.textContent('.cr-box')).includes('0.036'));
  await p.click('#result_tabs .nav-link >> text=Full ranking'); await idle(p);
  const rows = () => p.$$eval('#rank-table tbody tr', r => r.filter(x => x.style.display !== 'none').length);
  ok('ranking: 36 rows', (await rows()) === 36);
  await p.fill('.table-search', 'pradesh'); await p.dispatchEvent('.table-search', 'input');
  ok('ranking: search filters (5 Pradesh)', (await rows()) === 5);
  await p.fill('.table-search', ''); await p.dispatchEvent('.table-search', 'input');
  await p.click('#rank-table th >> nth=1');
  ok('ranking: sorting by name works', (await p.$eval('#rank-table tbody tr td:nth-child(2)', e => e.textContent.trim())) === 'A&N Islands');
  await p.click('#result_tabs .nav-link >> text=Map'); await idle(p);
  await p.click('path[data-state="Kerala"]'); await idle(p);
  ok('map: selecting Kerala shows rank 2 of 36 (matches R)', (await p.textContent('.map-detail h4')).includes('Kerala: rank 2 of 36'));
  await p.evaluate(() => Shiny.setInputValue('map_selected', 'Atlantis', { priority: 'event' })); await idle(p);
  ok('map: an unknown name sent from the browser is ignored', (await p.textContent('.map-detail h4')).includes('Kerala'));
  await p.evaluate(() => Shiny.setInputValue('answer', 9, { priority: 'event' })); await idle(p);
  ok('invalid answer sent from the browser is ignored (results unchanged)', (await p.textContent('.result-head h2')).includes('Your livability matches'));

  // -- Trade-offs: Pareto sets compared with pareto_front() in R --
  const tab = async (name) => { await p.click(`#result_tabs .nav-link >> text="${name}"`); await idle(p); };
  const eff = () => p.$$eval('.atlas-pt.is-efficient', e => e.map(x => x.getAttribute('data-state')).sort().join('|'));
  await tab('Trade-offs');
  ok('trade-offs: Safety vs Economy efficient set matches R', (await eff()) === 'Chandigarh|Ladakh|Lakshadweep|Nagaland|Tripura');
  ok('trade-offs: 36 points and an explanation for your #1 (Delhi)', (await p.$$('.atlas-pt')).length === 36 && (await p.textContent('#atlas')).includes('Your #1 match, Delhi'));
  await p.selectOption('#atlas_x', 'Health'); await p.selectOption('#atlas_y', 'Education'); await idle(p);
  ok('trade-offs: Health vs Education efficient set matches R', (await eff()) === 'Delhi|Kerala|Lakshadweep|Maharashtra|Punjab');
  await p.selectOption('#atlas_y', 'Health'); await idle(p);
  ok('trade-offs: the same criterion twice gives a message', (await p.textContent('#atlas')).includes('Choose two different criteria'));
  await p.selectOption('#atlas_y', 'Education'); await idle(p);

  // -- Robustness: shares compared with robustness_analysis() in R --
  await tab('Robustness');
  await p.waitForSelector('#robust-table');
  ok('robustness: default run matches R (Delhi first 46%, top 3 65%, 1,000 simulations)',
     (await p.textContent('#robustness')).includes('stays first in 46% and in your top 3 in 65% of 1,000'));
  const rrow = await p.$$eval('#robust-table tbody tr', r => r.map(x => [...x.cells].map(c => c.textContent.trim())));
  ok('robustness: table order and top-3 shares match R (Chandigarh 88%, Kerala 77%, Delhi 65%)', rrow.slice(0, 3).map(r => r[0] + ' ' + r[4]).join('|') === 'Chandigarh 88%|Kerala 77%|Delhi 65%');
  const delhi = rrow.find(r => r[0] === 'Delhi');
  ok('robustness: baseline rank and score shown apart from simulated rank (Delhi 1, 0.791, 2 (1-10))', delhi[1] === '1' && delhi[2] === '0.791' && delhi[7] === '2 (1-10)', delhi.join(' / '));
  ok('robustness: data notes and assumptions shown', (await p.textContent('#robust-table')).includes('low confidence') && (await p.textContent('#robustness')).includes('Data uncertainty is not modelled'));
  await p.selectOption('#rb_level', 'high'); await p.selectOption('#rb_n', '500'); await p.fill('#rb_seed', '7');
  await p.click('#rb_run'); await idle(p);
  ok('robustness: other settings match R (not sure, 500, seed 7: 34% / 49%)', (await p.textContent('#robustness')).includes('stays first in 34% and in your top 3 in 49% of 500'));
  const [csv] = await Promise.all([p.waitForEvent('download'), p.click('#rb_csv')]);
  const cp = OUT + '/robust.csv'; await csv.saveAs(cp);
  ok('robustness: CSV download has all 36 States/UTs', fs.readFileSync(cp, 'utf8').trim().split('\n').length === 37);

  // -- What if: compared with run_scenario() in R --
  await tab('What if');
  await p.selectOption('#wi_crit', 'Safety'); await idle(p);
  ok('what-if: the current rating is preselected (Safety 3)', (await p.inputValue('#wi_value')) === '3' && (await p.textContent('#whatif')).includes('Choose a rating different'));
  await p.selectOption('#wi_value', '5'); await idle(p);
  const wrow = await p.$$eval('#whatif-table tbody tr', r => r.map(x => [...x.cells].map(c => c.textContent.trim()).join(' ')));
  ok('what-if: Safety 5 matches R (#1 changes from Delhi to Lakshadweep, up 4)', (await p.textContent('#whatif')).includes('change from Delhi to Lakshadweep') && wrow[0] === '1 Lakshadweep 5 up 4', wrow[0]);

  // -- States & UTs: catalogue, data notes, comparison --
  await tab('States & UTs');
  const crows = () => p.$$eval('#catalogue-table tbody tr', r => r.length);
  ok('catalogue: 36 States/UTs with ranks from the current ranking', (await crows()) === 36 && (await p.$eval('#catalogue-table tbody tr', r => r.cells[1].textContent.trim())) === 'Delhi');
  await p.fill('#cat_search', 'pradesh'); await p.waitForTimeout(700); await idle(p);
  ok('catalogue: search filters (5 Pradesh)', (await crows()) === 5);
  await p.fill('#cat_search', 'zzz'); await p.waitForTimeout(700); await idle(p);
  ok('catalogue: no-match message', (await p.textContent('#catalogue')).includes('No State or UT matches'));
  await p.fill('#cat_search', ''); await p.waitForTimeout(700); await idle(p);
  await p.selectOption('#cat_pick', 'Lakshadweep'); await idle(p);
  const det = await p.textContent('#cat_detail');
  ok('catalogue: data notes from the dataset (Lakshadweep: 4 missing, no Connectivity data)', det.includes('4 of 17 indicators are missing') && det.includes('No indicator is available for Connectivity'));
  await p.evaluate(() => { const z = $('#cmp_pick')[0].selectize; z.addItem('Kerala'); z.addItem('Goa'); }); await idle(p);
  const tot = await p.$eval('#compare-table tbody tr:last-child', r => r.textContent.replace(/\s+/g, ' '));
  ok('comparison: match scores and ranks agree with the ranking (Kerala 0.790 rank 2)', tot.includes('0.790 (2)'), tot);

  // -- News: real headlines or an honest unavailable message --
  await tab('News');
  await p.waitForFunction(() => !/Loading headlines/.test(document.getElementById('news').textContent), null, { timeout: 40000 });
  const links = await p.$$eval('#news-list a', a => a.map(x => ({ h: x.href, t: x.target, r: x.rel })));
  const unavailable = (await p.textContent('#news')).includes('unavailable');
  ok('news: real headlines with safe external links, or the unavailable message', unavailable || (links.length > 0 && links.length <= 4 && links.every(l => /^https?:\/\//.test(l.h) && l.t === '_blank' && /noopener/.test(l.r))),
     unavailable ? 'feeds unavailable' : links.length + ' headlines');

  // Report.
  const [dl] = await Promise.all([p.waitForEvent('download'), p.click('text=Download my report')]);
  const rp = OUT + '/report.html'; await dl.saveAs(rp); const rep = fs.readFileSync(rp, 'utf8');
  ok('report: downloads with the same top 3 and 36 ranking rows', rep.includes('1. Delhi') && rep.includes('2. Kerala') && (rep.match(/<tr>\s*<td>/g) || []).length === 36);
  ok('report: includes the comparison, the what-if scenario and the Pareto section', rep.includes('My comparison') && rep.includes('What-if scenario (hypothetical)') &&
     rep.includes('Pareto-efficient places') && /<td>Lakshadweep<\/td>\s*<td>5<\/td>/.test(rep));

  // Low-confidence switch.
  await p.click('#result_tabs .nav-link >> text=Top 3'); await idle(p);
  await p.check('#exclude_low'); await idle(p);
  ok('low-confidence switch: top 3 without them matches R (Delhi, Kerala, Punjab)', (await names(p)).join('|') === 'Delhi|Kerala|Punjab');
  await p.click('#result_tabs .nav-link >> text=Full ranking'); await idle(p);
  ok('low-confidence switch: 32 ranked', (await rows()) === 32);
  await p.uncheck('#exclude_low'); await idle(p);

  // Presets and equal weights.
  await p.click('text=Start over'); await idle(p);
  await p.click('.preset-btn >> nth=1'); await idle(p);
  for (let i = 0; i < 7; i++) { await p.click('text=Keep this answer'); await idle(p); }
  ok('preset: Family top 3 matches R (Himachal Pradesh, Chandigarh, Punjab)', (await names(p)).join('|') === 'Himachal Pradesh|Chandigarh|Punjab');
  await p.click('text=Start over'); await idle(p);
  await p.click('.skip-link button'); await idle(p);
  ok('equal weights: top 3 matches R (Chandigarh, Himachal Pradesh, Goa)', (await names(p)).join('|') === 'Chandigarh|Himachal Pradesh|Goa');

  ok('no Shiny output errors', (await p.$$('.shiny-output-error')).length === 0);
  ok('no JavaScript errors', errors.length === 0, errors.slice(0, 3).join(' | '));
  console.log(res.join('\n')); process.exitCode = res.some(r => r.startsWith('FAIL')) ? 1 : 0;
  console.log(`\n${res.filter(r => r.startsWith('PASS')).length} passed, ${res.filter(r => r.startsWith('FAIL')).length} failed`);
  await b.close();
})().catch(e => { console.log(res.join('\n')); console.error('ABORTED:', e.message.split('\n')[0]); process.exit(1); });
