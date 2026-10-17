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
  "Seven criteria. 17 State/UT-level indicators are grouped into Education, Safety, Health, Housing, Economy, Environment and Connectivity. Each indicator is put on a 0-1 scale where higher is always better (extreme values are capped first), the indicators of a criterion are averaged, and the result is rescaled to 0-1 so that different units can be compared. These are relative scores, not the original measurements.",
  "Your answers. You rate how much each criterion should count, from 1 (not important) to 5 (extremely important). You are weighting the indicators that are available, not describing every part of your life.",
  "Pairwise comparisons (AHP). For every pair of criteria, the difference between your two answers becomes a comparison on Saaty's 1-9 scale: equal answers mean equal importance, one point apart means 'moderately more important', and so on up to 'extremely'.",
  "Your weights. The comparisons are turned into weights that sum to 100%. Gaps between answers are magnified: in each comparison a one-point gap counts as 3 times as important and a four-point gap as 9 times, so a criterion you rate low can end up with a very small weight.",
  "Consistency. AHP also computes a consistency ratio. With 1-5 answers it always passes, so it is not shown as a sign of quality.",
  "Ranking (TOPSIS). Each State/UT is compared with an imaginary place that has the best score of the places ranked on every criterion, and with one that has the worst. The relative match runs from 0 to 1; it depends on which places are ranked and is not a measure of how livable a place is.",
  "Explanation. 'Why this fits you' shows where a State/UT stands among the places ranked (top, middle or bottom third) on your three most important criteria, with the underlying indicator values, and names its biggest gap for you.",
  "Stability and similar places. The stability check re-ranks with each of your answers moved one point up or down, one at a time, and counts how often your top places stay on top. Similar States/UTs have the nearest overall profile across all seven scores and do not use your weights."
)

# Honest limits of the data and method, for the "About and limits" tab and the report.
LIMITATIONS <- c(
  "The ranking is relative, not absolute. A relative match of 0.78 means 'close to the best of the places ranked', not '78% livable', and it changes if places are left out.",
  "Your weights come from simple 1-5 answers. They express your priorities; they are not measured facts.",
  "The data cannot tell you about rent or house prices, salaries or job openings, the cost of living, public transport, distance to hospitals, personal safety, language or what daily life is like in a particular city.",
  "Sources and reporting years are not recorded for any indicator. Four (Education score, Crime rate, Price stability and Electric vehicles per lakh) could not be traced to a published table, so treat the criteria that use them with caution.",
  "Some values look unusual. For example, Manipur's price-stability figure is 1.32, far below other States, and lower inflation is always counted as better.",
  "Four States/UTs have limited data coverage because several indicators are missing: A&N Islands, Chandigarh, Ladakh and Lakshadweep. They can rank high on thin data; use the 'leave out' switch to see the ranking without them.",
  "Data are at State/UT level. Cities and districts inside a State can differ a lot from the State average.",
  "The map outlines come from a public India boundary dataset (udit-001/india-maps-data, whose licence is not clearly stated) and are simplified for display. They are for illustration only, not an authoritative map."
)

# What the stored criterion scores imply about how they were built. These facts
# are checked against the dataset by R/data_quality.R (tests) and
# tools/audit_data.py (see docs/DATA_AUDIT.md).
DATA_NOTES <- c(
  "Scaling. Each indicator is capped at its 5th and 95th percentiles, and percentages above 100 at 100, before scaling. So a State/UT far ahead of the rest gets the same score as one at the 95th percentile (for example, Delhi's PM2.5 counts like the 95th-percentile value).",
  "Missing indicators. A criterion score is the average of whichever of its indicators are available. Chandigarh's Housing score of 1.00, for example, rests on one of its three indicators.",
  "Imputed value. Lakshadweep has neither Connectivity indicator. Its Connectivity score (0.41) is the median of the other 35 States/UTs: it is an estimate, not a measurement.",
  "Percentages above 100. Clean cooking fuel is above 100% in 22 States/UTs and immunisation in 7. The underlying definitions need checking before these figures are read as shares of households or children.",
  "Why it matters. If missing indicators counted as the worst score instead of being skipped, the Retiree profile's top 3 would lose Lakshadweep and Ladakh. The 'leave out low-confidence States/UTs' switch shows a similar effect."
)

# What the stability check means (shared by the app, the report and view models).
STABILITY_NOTES <- c(
  "The stability check moves each of your answers one point up or down, one at a time, and re-ranks with the same method and the same places.",
  "It shows how sensitive your result is to your own answers. It is not a probability and says nothing about how accurate the data are."
)

# What to check before relying on the result (static guidance, no data claims).
BEFORE_YOU_MOVE <- c(
  "Rents and house prices in the specific city or neighbourhood.",
  "Jobs and salaries in your field in that city.",
  "Commute, public transport and travel connections.",
  "The languages used locally at work, in schools and in daily life.",
  "Schools, hospitals and doctors near where you would live.",
  "Climate and seasonal conditions.",
  "Any entry permits or local rules for residents (check official government sources).",
  "Recent local news and, if possible, a visit or conversations with people who live there."
)
