# ==============================================================================
# Plain-language text shared by the app tabs and the downloadable report.
# Keeping it in ONE place means the app and the report can never disagree.
# ==============================================================================

# Which raw indicators feed each criterion (see INDICATORS in R/common.R).
CRITERION_INDICATORS <- list(
  Education    = "Education score, Grade 8 proficiency, higher-secondary enrolment",
  Safety       = "Crime rate, murders per lakh",
  Health       = "Immunisation, institutional deliveries, health-worker density",
  Housing      = "Kachha houses, clean cooking fuel, rural piped water",
  Economy      = "Unemployment, poverty headcount (MPI), price stability",
  Environment  = "Air quality (PM2.5)",
  Connectivity = "Villages with 3G/4G, electric vehicles per lakh people"
)

# The method, step by step, for the "How it was calculated" tab and the report.
METHOD_STEPS <- c(
  "Seven criteria. 17 indicators are grouped into Education, Safety, Health, Housing, Economy, Environment and Connectivity. Each criterion is scaled to 0-1 so that higher is always better and different units can be compared.",
  "Your answers. You rate each criterion from 1 (not important) to 5 (extremely important).",
  "Pairwise comparisons (AHP). For every pair of criteria, the difference between your two answers becomes a comparison on Saaty's 1-9 scale: equal answers mean equal importance, one point apart means 'moderately more important', and so on up to 'extremely'.",
  "Your weights. The weights are the principal eigenvector of that 7x7 comparison matrix. They sum to 100% and show how much each criterion counts for you.",
  "Consistency check. The consistency ratio (CR) tests whether your comparisons contradict each other. CR of 0.10 or below is accepted. Answers given as 1-5 scores are always consistent, so CR is normally very small.",
  "Ranking (TOPSIS). Each State/UT is placed in a 7-dimensional space using your weights. TOPSIS measures its distance to the ideal State/UT (best score on every criterion) and to the worst. Closeness = D- / (D+ + D-), from 0 (worst possible) to 1 (the ideal).",
  "Explanation. 'Why this fits you' compares a State/UT's score on your three most important criteria with the other States/UTs. Strong match is a score of 0.70 or more, moderate 0.40 to 0.70, and below 0.40 falls short.",
  "Similar States/UTs. These are the States/UTs with the nearest overall profile (Euclidean distance on all seven scores, equal importance). They do not depend on your weights."
)

# Honest limits of the data and method, for the "About and limits" tab and the report.
LIMITATIONS <- c(
  "The ranking is relative, not absolute. A closeness of 0.78 means 'closest to the best available State/UT', not '78% livable'.",
  "Your weights come from simple 1-5 answers. They express your priorities; they are not measured facts.",
  "Four indicators (Education score, Crime rate, Price stability and Electric vehicles per lakh) have sources that could not be traced to a published table, so treat the criteria that use them with some caution.",
  "Some values look unusual. For example, Manipur's price-stability figure is 1.32, which looks too low next to other States.",
  "Four States/UTs have low data confidence because several indicators are missing: A&N Islands, Chandigarh, Ladakh and Lakshadweep. They can rank high on thin data; use the 'leave out' switch to see the ranking without them.",
  "Data are at State/UT level. Cities and districts inside a State can differ a lot from the State average.",
  "The map outlines come from a public India boundary dataset (udit-001/india-maps-data, whose licence is not clearly stated) and are simplified for display. They are for illustration only, not an authoritative map."
)