// Search box and click-to-sort for the full-ranking table (no extra packages).
function ilmFilterTable(input) {
  var q = input.value.toLowerCase();
  var rows = document.querySelectorAll('#rank-table tbody tr');
  rows.forEach(function (r) {
    r.style.display = r.cells[1].textContent.toLowerCase().indexOf(q) === -1 ? 'none' : '';
  });
}
function ilmSortTable(th) {
  var table = document.getElementById('rank-table');
  var idx = th.cellIndex;
  var asc = th.getAttribute('data-asc') !== 'true';
  table.querySelectorAll('th').forEach(function (h) { h.removeAttribute('data-asc'); });
  th.setAttribute('data-asc', asc ? 'true' : 'false');
  var body = table.tBodies[0];
  var rows = Array.prototype.slice.call(body.rows);
  var num = function (t) { var v = parseFloat(t); return isNaN(v) ? null : v; };
  rows.sort(function (a, b) {
    var x = a.cells[idx].textContent.trim(), y = b.cells[idx].textContent.trim();
    var nx = num(x), ny = num(y);
    var c = (nx !== null && ny !== null) ? nx - ny : x.localeCompare(y);
    return asc ? c : -c;
  });
  rows.forEach(function (r) { body.appendChild(r); });
}