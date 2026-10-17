"""Small Python version of the project's AHP and TOPSIS (standard library).

Used only by the R-independent checks in tools/: to re-compute the committed
results (check_repo.py) and to measure how data-handling choices would move the
rankings (audit_data.py). The app and the analysis use the R code in R/; this
module does not replace or test it.
"""
SAATY_RI7 = 1.32
# Must match PERSONAS in R/ahp.R (tools/check_repo.py checks this).
PERSONAS = {
    "Young Professional": [2, 3, 2, 4, 5, 2, 5],
    "Family": [5, 5, 5, 3, 3, 3, 2],
    "Retiree": [1, 4, 5, 3, 2, 5, 2],
}


def ahp(answers):
    """Difference rule + principal eigenvector (power method). Returns (weights, CR)."""
    n = len(answers)
    M = [[(1 + 2 * (a - b)) if a >= b else 1 / (1 + 2 * (b - a)) for b in answers] for a in answers]
    w = [1 / n] * n
    for _ in range(2000):
        v = [sum(M[i][j] * w[j] for j in range(n)) for i in range(n)]
        s = sum(v)
        w = [x / s for x in v]
    lam = sum(sum(M[i][j] * w[j] for j in range(n)) / w[i] for i in range(n)) / n
    return w, ((lam - n) / (n - 1)) / SAATY_RI7


def topsis(X, w):
    """Closeness for each row of X (criteria already 0-1, higher = better)."""
    p = len(w)
    V = [[w[j] * row[j] for j in range(p)] for row in X]
    best = [max(r[j] for r in V) for j in range(p)]
    worst = [min(r[j] for r in V) for j in range(p)]
    out = []
    for r in V:
        dp = sum((r[j] - best[j]) ** 2 for j in range(p)) ** 0.5
        dm = sum((r[j] - worst[j]) ** 2 for j in range(p)) ** 0.5
        out.append(dm / (dp + dm) if dp + dm else 0.0)
    return out


def ranking(names, X, w):
    """Names ordered best first, and closeness by name."""
    cl = topsis(X, w)
    order = sorted(range(len(names)), key=lambda i: -cl[i])
    return [names[i] for i in order], dict(zip(names, cl))


def spearman_rank(a, b):
    """Spearman correlation of two complete rankings (lists of the same names)."""
    n = len(a)
    pos_b = {s: i for i, s in enumerate(b)}
    d2 = sum((i - pos_b[s]) ** 2 for i, s in enumerate(a))
    return 1 - 6 * d2 / (n * (n * n - 1))
