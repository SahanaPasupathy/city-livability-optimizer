#!/usr/bin/env bash
# Smoke test of the REAL Shiny app (needs R with the app's packages, and Node.js).
# Usage, from the project root:
#   npm ci --prefix tests/browser                              # once (installs Playwright)
#   npx --prefix tests/browser playwright install chromium     # once
#   bash tests/browser/run.sh [screenshot_dir]
# Starts the app on port 7654 (override with APP_PORT), runs the smoke suite and
# stops the app again. Exit code is non-zero if any check fails.
set -u
cd "$(dirname "$0")/../.."
PORT="${APP_PORT:-7654}"
OUT="${1:-$(mktemp -d)}"
mkdir -p "$OUT"
export APP_URL="http://127.0.0.1:${PORT}/"
export NODE_PATH="$PWD/tests/browser/node_modules"

Rscript -e "shiny::runApp('app', port = ${PORT}, launch.browser = FALSE)" > "$OUT/app.log" 2>&1 &
APP_PID=$!
trap 'kill "$APP_PID" 2>/dev/null' EXIT
for _ in $(seq 1 60); do curl -s -o /dev/null "$APP_URL" && break; sleep 1; done
if ! curl -s -o /dev/null "$APP_URL"; then echo "The app did not start; see $OUT/app.log"; exit 2; fi

status=0
echo "== smoke"
node tests/browser/smoke.js "$OUT" || status=1
echo "Screenshots and app log: $OUT"
exit $status
