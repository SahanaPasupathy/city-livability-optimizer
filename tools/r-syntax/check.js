// Syntax check for R files without R: parses each file with the tree-sitter R
// grammar and reports ERROR or MISSING nodes. This only proves the code parses;
// it does not run it. Usage (from the project root):
//   npm ci --ignore-scripts --prefix tools/r-syntax
//   node tools/r-syntax/check.js            (checks app/, R/, analysis/, tests/)
//   node tools/r-syntax/check.js file.R ...
'use strict';
const fs = require('fs');
const path = require('path');
const Parser = require('web-tree-sitter');

const ROOT = path.resolve(__dirname, '..', '..');
const DIRS = ['app', 'R', 'analysis', 'tests'];

function rFiles(dir) {
  const out = [];
  if (!fs.existsSync(dir)) return out;
  for (const e of fs.readdirSync(dir, { withFileTypes: true })) {
    const p = path.join(dir, e.name);
    if (e.isDirectory()) out.push(...rFiles(p));
    else if (/\.R$/i.test(e.name)) out.push(p);
  }
  return out;
}

(async () => {
  await Parser.init();
  const wasm = require.resolve('@davisvaughan/tree-sitter-r/tree-sitter-r.wasm');
  const R = await Parser.Language.load(wasm);
  const parser = new Parser();
  parser.setLanguage(R);
  const files = process.argv.length > 2
    ? process.argv.slice(2)
    : DIRS.flatMap((d) => rFiles(path.join(ROOT, d))).sort();
  let bad = 0;
  for (const f of files) {
    const src = fs.readFileSync(f, 'utf8');
    const errs = [];
    (function walk(n) {
      if (n.type === 'ERROR' || n.isMissing) errs.push(n);
      for (let i = 0; i < n.childCount; i++) walk(n.child(i));
    })(parser.parse(src).rootNode);
    const rel = path.relative(ROOT, path.resolve(f));
    if (errs.length === 0) { console.log(`OK    ${rel}`); continue; }
    bad++;
    for (const e of errs.slice(0, 5)) {
      const snippet = src.slice(e.startIndex, Math.min(e.endIndex, e.startIndex + 60)).replace(/\n/g, '\\n');
      console.log(`ERROR ${rel}:${e.startPosition.row + 1}:${e.startPosition.column + 1} ${e.isMissing ? 'missing ' + e.type : snippet}`);
    }
  }
  console.log(`\n${files.length - bad} of ${files.length} R files parse without syntax errors.`);
  process.exit(bad ? 1 : 0);
})().catch((err) => { console.error(err); process.exit(2); });
