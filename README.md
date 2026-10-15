# City Livability Optimizer

**Personalized State and UT Livability Recommendation System**
Programming for Data Science (BCSE207L), built in R.

**Team:** Saipriya R (24BDS1100) and Sahana Pasupathy (24BDS1049)

A person answers seven short questions about what matters to them. The system turns those answers into personal weights (AHP), ranks all 36 Indian States and Union Territories for them (TOPSIS), shows the top 3 with a plain-language explanation, and lets them explore the result on an interactive map of India.

---

## 1. Problem

Livability rankings usually give one fixed list. But a young professional, a family and a retiree do not want the same things. This project asks:

> Which State or UT is the best fit for *this* person's priorities, and why?

## 2. Data

`data/State_UT_Livability_Dataset_Final.csv` has one row per State/UT (36 rows: 28 States, 8 UTs).

- **17 raw indicators** are grouped into **7 criteria**:

| Criterion | Built from |
|---|---|
| Education | Education score, Grade 8 proficiency, higher-secondary enrolment |
| Safety | Crime rate, murders per lakh |
| Health | Immunisation, institutional deliveries, health-worker density |
| Housing | Kachha houses, clean cooking fuel, rural piped water |
| Economy | Unemployment, poverty headcount (MPI), price stability |
| Environment | Air quality (PM2.5) |
| Connectivity | Villages with 3G/4G, electric vehicles per lakh people |

- Each criterion is **min-max scaled to 0-1 with higher = better**, so different units can be compared.
- 4 States/UTs are flagged **low data confidence** because several indicators are missing: A&N Islands, Chandigarh, Ladakh, Lakshadweep.

## 3. Method

1. **Questionnaire:** the user rates each criterion from 1 (not important) to 5 (extremely important).
2. **AHP (Analytic Hierarchy Process):**
   - For each pair of criteria, the difference between the two answers becomes a comparison on Saaty's 1-9 scale (0 apart = 1, 1 apart = 3, 2 = 5, 3 = 7, 4 = 9).
   - The weights are the principal eigenvector of the 7x7 comparison matrix.
   - The consistency ratio is CR = CI / RI with RI(7) = 1.32. CR of 0.10 or below is accepted.
3. **TOPSIS:**
   - The weighted scores are v = w x score.
   - The ideal solution takes the best value on each criterion and the worst solution takes the lowest.
   - Closeness = D- / (D+ + D-), from 0 (worst) to 1 (ideal).
4. **Explanation:** "Why this fits you" compares a State's scores on the user's three heaviest criteria with the other States (Strong match is 0.70 or more, Moderate 0.40 to 0.70, Falls short below 0.40), and names where it falls furthest short.
5. **Similar States/UTs:** Euclidean distance on the 7 scores with equal importance. This is kept separate from the personalised ranking.

## 4. Results (demonstration personas)

Importance answers (1-5) for three personas: Young Professional, Family and Retiree. Baseline = the same TOPSIS with equal weights.

| Persona | Top 3 | CR | Spearman vs equal weights | Top-3 overlap with equal weights |
|---|---|---|---|---|
| Young Professional | Delhi, Chandigarh, Goa | 0.023 | 0.812 | 2 of 3 |
| Family | Himachal Pradesh, Chandigarh, Punjab | 0.014 | 0.834 | 2 of 3 |
| Retiree | Lakshadweep, Ladakh, Karnataka | 0.039 | 0.675 | 0 of 3 |
| Equal weights (baseline) | Chandigarh, Himachal Pradesh, Goa | 0 | 1 | 3 of 3 |

- **Personalisation changes the answer.** For the Retiree persona, Delhi falls 19 places and Andhra Pradesh rises 14.
- **Low-confidence States rank high** (Chandigarh, Ladakh, Lakshadweep appear in several top 3s). The app has a switch to leave them out; for the Retiree persona the top 3 then becomes Karnataka, Andhra Pradesh, Kerala.
- Tables are in `outputs/tables/` and figures in `outputs/figures/`.

### Robustness (Step 9, `analysis/10_sensitivity.R`)

How much do the recommendations change if the weights or the method change a little?

- **Random weight changes** (1000 draws per persona; each weight moves by about 0.02-0.06). The number 1 choice stays the same in 82% of draws for Young Professional, 40% for Family, 68% for Retiree and 51% for equal weights. The exact top 3 stays the same in 77%, 47%, 59% and 22% of draws. The average Spearman correlation with the original ranking is 0.92 to 0.97. So **the overall ranking is stable, but the order inside the top 3 is not**: Delhi (99%), Lakshadweep (97%) and Ladakh (93%) almost always stay in the top 3, while third place (for example Punjab 64%, Karnataka 67%) often swaps with the next State.
- **Ignoring one criterion at a time.** Ranking correlation with the original stays above 0.65 in every case. Some criteria matter a lot for some people: ignoring Connectivity changes the Young Professional top 3 to Manipur, Delhi, Tripura, ignoring Education or Safety changes the Family top 3, and ignoring Environment or Health changes the Retiree top 3.
- **Leaving out the 4 low-confidence States/UTs** hardly changes the order of the others (Spearman 0.995 to 1.000), but it does change the top 3 of the Retiree persona (Lakshadweep and Ladakh drop out).
- **Different method.** A simple weighted sum agrees with TOPSIS (Spearman 0.97 to 0.99) and gives the same top 3 as a set in 3 of 4 cases. Vector normalisation also agrees (0.975 to 0.994).
- Figure: `outputs/figures/10_top3_stability.png`. Tables: `outputs/tables/10_*.csv`.

## 5. The Shiny app

Features: flashcard questions, three presets (Young Professional, Family, Retiree), a skip option (equal weights), the top 3 with "Why this fits you", an interactive India map coloured by closeness, a searchable and sortable full ranking, a weights panel with the consistency check, "How it works" and "About and limits" tabs, and a downloadable personal report (HTML).

### How to run it

1. Install R and RStudio.
2. Open `city-livability-optimizer.Rproj` in RStudio.
3. Install the packages (once):
   ```r
   install.packages(c("shiny", "bslib", "ggplot2"))
   ```
4. Open `app/app.R` and click **Run App** (or run `shiny::runApp("app")` in the Console).

### How to reproduce the analysis

Source the scripts in `analysis/` in order from the project root, for example `source("analysis/04_topsis.R")`. Each prints PASS/FAIL checks and writes its tables and figures to `outputs/`.

| Script | What it does |
|---|---|
| `01_audit.R` | Audits the dataset before use |
| `02_eda.R` | Exploratory analysis and baseline ranking |
| `03_ahp.R` | Persona weights and consistency |
| `04_topsis.R` | Personalised TOPSIS rankings |
| `05_recommend.R` | Top 3, explanations and similar States |
| `06_compare.R` | Personalised vs equal-weight ranking |
| `07_map.R` | Interactive map files (`outputs/maps/`) |
| `08_app_check.R` | Checks of the app's logic |
| `09_report_check.R` | Checks of the report and texts |
| `10_sensitivity.R` | Sensitivity and robustness analysis |
| `11_run_tests.R` | Runs the automated tests in `tests/testthat/` |

### Automated tests

Run `source("analysis/11_run_tests.R")` (needs `install.packages("testthat")`). The 25 tests cover:

- AHP: the difference rule, a perfectly consistent matrix, Saaty's textbook 3x3 example and contradictory answers.
- TOPSIS: a small case checked by hand and rejection of bad inputs.
- Similar States/UTs: nearest-distance results.
- The real data and the app's calculation: the three persona top 3s, the dataset's shape, and the low-confidence switch.

Breaking the code on purpose makes tests fail, so they do catch mistakes.

## 6. Project structure

```
app/                 Shiny app (app.R, www/style.css, www/table.js)
R/                   Tested functions: common, ahp, topsis, similarity,
                     explain, compare, map, app_logic, app_text, report,
                     sensitivity
analysis/            Numbered scripts (01-11)
data/                Dataset and map outlines (data/map/)
data_extraction/     build_map_paths.py: how the map outlines were made
tests/testthat/      Automated tests (testthat)
outputs/             Tables, figures, maps, sample report
```

## 7. Limitations

- The ranking is **relative, not absolute**. A closeness of 0.78 means "closest to the best available State/UT", not "78% livable".
- The weights come from simple 1-5 answers. They express priorities, not measured facts.
- **Four indicators have sources that could not be traced** to a published table: Education score, Crime rate, Price stability and Electric vehicles per lakh. Criteria that use them should be treated with some caution.
- **Some values look unusual.** Manipur's price-stability figure is 1.32, far below other States, and has not been verified.
- **Low-confidence States/UTs** can rank high on thin data (see Results).
- Data are at State/UT level; cities and districts can differ a lot from the State average.
- **Map outlines** come from the public repository `udit-001/india-maps-data` (commit 2884453), whose licence is not clearly stated. They are simplified, for illustration only, and are not an authoritative map. The outline file is rebuilt by `data_extraction/build_map_paths.py`.