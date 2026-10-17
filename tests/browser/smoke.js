// Smoke test of the real Shiny app with its original interface: the logo,
// questionnaire, presets, results (numbers compared with values computed in R),
// tabs, ranking table, map, low-confidence switch, report, invalid input, and the
// tabs (What if?, Compare, Stability, States & UTs; News is off by default).
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
  const tab = async (name) => { await p.click(`#result_tabs .nav-link >> text="${name}"`); await idle(p); };
  const n1 = await names(p); const m = await p.$$eval('.match-label', e => e.map(x => x.textContent));
  const t3 = await p.textContent('#top3');
  ok('results: top 3 match R (Delhi, Kerala, Chandigarh)', n1.join('|') === 'Delhi|Kerala|Chandigarh', n1.join(', '));
  ok('results: relative match to 2 decimals (0.79, 0.79, 0.78)', m[0].includes('Relative match 0.79') && m[1].includes('0.79') && m[2].includes('0.78'));
  ok('results: plain tabs, What if? second, no Robustness/Trade-offs/News', (await p.$$eval('#result_tabs .nav-link', e => e.map(x => x.textContent.trim()))).join('|') ===
     'Top 3|What if?|Compare|Stability|Map|What matters to you|Full ranking|States & UTs|How it works|About and limits');
  ok('results: cards show why it matches and a trade-off, no medals', t3.includes('Why it matches') && (t3.match(/Trade-off:/g) || []).length === 3 && !(await p.$$('.top-card.gold')).length);
  ok('results: stability label, coverage warning and alternative top 3', t3.includes('Close call') && t3.includes('Limited data: Housing based on few indicators') &&
     t3.includes('Without places with limited data: Delhi, Kerala, Punjab'));
  ok('results: indicator data collapsed behind "Show the data"', (await p.$$('.top-card details:not([open])')).length === 3);
  ok('results: no technical controls or advanced sections anywhere', (await p.$$('#rb_n, #rb_seed, #rb_method, #rb_run, #atlas_x')).length === 0 && (await p.$$('#tc_x')).length === 1 &&
     !/advanced users/i.test(await p.textContent('body')));

  await tab('What matters to you');
  const w = await p.$$eval('.w-row', r => r.map(x => x.textContent.replace(/\s+/g, ' ').trim()));
  ok('weights: match R (Health 39%, Environment 2%) in plain words', w[0].endsWith('Health 39%') && w[6].endsWith('Environment 2%') && !(await p.textContent('#weights')).includes('Consistency ratio'), w[0] + ' / ' + w[6]);
  await tab('Full ranking');
  const rows = () => p.$$eval('#rank-table tbody tr', r => r.filter(x => x.style.display !== 'none').length);
  ok('ranking: 36 rows, no D+/D- columns', (await rows()) === 36 && !(await p.textContent('#rank-table')).includes('D+'));
  await p.fill('.table-search', 'pradesh'); await p.dispatchEvent('.table-search', 'input');
  ok('ranking: search filters (5 Pradesh)', (await rows()) === 5);
  await p.fill('.table-search', ''); await p.dispatchEvent('.table-search', 'input');
  await p.click('#rank-table th >> nth=1');
  ok('ranking: sorting by name works', (await p.$eval('#rank-table tbody tr td:nth-child(2)', e => e.textContent.trim())) === 'A&N Islands');
  await tab('Map');
  await p.click('path[data-state="Kerala"]'); await idle(p);
  ok('map: selecting Kerala shows rank 2 of 36 with a card (matches R)', (await p.textContent('.map-detail h4')).includes('Kerala: rank 2 of 36') && (await p.textContent('.map-detail')).includes('Why it matches'));
  await p.evaluate(() => Shiny.setInputValue('map_selected', 'Atlantis', { priority: 'event' })); await idle(p);
  ok('map: an unknown name sent from the browser is ignored', (await p.textContent('.map-detail h4')).includes('Kerala'));
  await p.evaluate(() => Shiny.setInputValue('answer', 9, { priority: 'event' })); await idle(p);
  ok('invalid answer sent from the browser is ignored (results unchanged)', (await p.textContent('.result-head h2')).includes('Your livability matches'));

  // -- Compare: trade-off explorer (chart + search, synchronised) --
  await tab('Compare');
  await p.waitForSelector('.tc-pt');
  ok('compare: axes start on your top two criteria (Health, Education)', (await p.inputValue('#tc_x')) === 'Health' && (await p.inputValue('#tc_y')) === 'Education');
  ok('compare: 36 points, your top 3 selected and labelled', (await p.$$('.tc-pt')).length === 36 && (await p.$$('.tc-pt.is-selected')).length === 3);
  ok('compare: cards match R (Delhi #1 of 36, relative match 0.79, Health 0.89 5th)', (await p.textContent('#tc_cards')).includes('#1 of 36 · relative match 0.79') &&
     (await p.textContent('#tc_cards')).includes('Health 0.89 (5th)'));
  await p.click('.tc-pt[data-state="Goa"]'); await idle(p);
  ok('compare: clicking a point adds it to the search box and the cards (earliest dropped)',
     (await p.$$eval('#cmp_places option', o => o.map(x => x.value))).join('|') === 'Kerala|Chandigarh|Goa' && (await p.textContent('#tc_cards')).includes('Goa'));
  await p.click('.tc-pt[data-state="Goa"]'); await idle(p);
  ok('compare: clicking it again removes it', !(await p.textContent('#tc_cards')).includes('Goa'));
  await p.selectOption('#tc_y', 'Safety'); await idle(p);
  ok('compare: changing an axis keeps the selection', (await p.$$('.tc-pt.is-selected')).length === 2 && (await p.textContent('#tc_chart')).includes('Safety (relative score)'));
  await p.selectOption('#tc_y', 'Health'); await idle(p);
  ok('compare: choosing the same criterion twice swaps the axes', (await p.inputValue('#tc_x')) !== (await p.inputValue('#tc_y')));
  ok('compare: details collapsed, no Pareto wording', (await p.$$('#compare_more details:not([open])')).length === 1 && !(await p.textContent('body')).includes('Pareto'));
  await p.click('text=Use my top 3'); await idle(p);

  // -- Stability: compact visual answer, one question --
  await tab('Stability');
  const stab = await p.textContent('#stability');
  ok('stability: label, headline and sentence match R (Close call, Delhi 48%)', stab.includes('Close call') && stab.includes('Delhi stays your #1 in 48% of the answer variations') && stab.includes('can change your #1'));
  ok('stability: bars for the main places, no tables, caveat once', (await p.$$('#stability .cmp-row')).length === 5 && (await p.$$('#stability table')).length === 0 &&
     (stab.match(/not the chance that a place is best/g) || []).length === 1);
  await p.selectOption('#rb_level', 'high'); await idle(p);
  ok('stability: "Not sure" matches R (38%)', (await p.textContent('#stability')).includes('in 38% of the answer variations'));
  await p.selectOption('#rb_level', 'medium'); await idle(p);

  // -- What if?: several ratings, before/after, back to the original --
  await tab('What if?');
  ok('what if: current ratings preselected and original answers shown', (await p.inputValue('#pref_Safety')) === '3' && (await p.inputValue('#pref_Environment')) === '1' &&
     (await p.textContent('#whatif_result')).includes('viewing your original answers'));
  await p.selectOption('#pref_Safety', '5'); await p.selectOption('#pref_Environment', '4');
  await p.click('text=Show results with these priorities'); await idle(p);
  const wi = await p.textContent('#whatif_result');
  ok('what if: two changes give R\'s result (#1 Delhi -> Lakshadweep, before/after lists)', wi.includes('your #1 changes from Delhi to Lakshadweep') && wi.includes('Original top 3') &&
     wi.includes('With these priorities') && wi.includes('leaves top 3') && wi.includes('Safety now counts 29%'));
  ok('what if: the header says a what-if is shown', (await p.textContent('#mode_note')).includes('What if:'));
  ok('what if: the Top 3 tab shows the new results', (await names(p)).join('|') === 'Lakshadweep|Himachal Pradesh|Ladakh');
  await p.click('.whatif-actions >> text=Back to my original answers'); await idle(p);
  ok('what if: back to the original answers restores the ranking', (await names(p)).join('|') === 'Delhi|Kerala|Chandigarh' && (await p.inputValue('#pref_Safety')) === '3');

  // -- States & UTs: catalogue and details with original values --
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
  ok('catalogue: data notes from the dataset (Lakshadweep: 4 missing, Connectivity estimated, raw values)', det.includes('4 of 17 indicators are missing') && det.includes('estimated: no indicator available') && det.includes('Indicator values (original data)'));

  // Report.
  const [dl] = await Promise.all([p.waitForEvent('download'), p.click('text=Download my report')]);
  const rp = OUT + '/report.html'; await dl.saveAs(rp); const rep = fs.readFileSync(rp, 'utf8');
  ok('report: downloads with the same top 3 and 36 ranking rows', rep.includes('1. Delhi') && rep.includes('2. Kerala') && (rep.match(/<tr>\s*<td>/g) || []).length === 36);
  ok('report: includes the comparison of your top 3 and per-place data notes (no Pareto list)', rep.includes('My comparison') &&
     rep.includes('Data notes for the places in this report') && !rep.includes('Pareto-efficient places') && rep.includes('Appendix: full ranking'));

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
