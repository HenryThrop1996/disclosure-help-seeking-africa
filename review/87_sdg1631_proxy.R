# 87_sdg1631_proxy.R - Correction 5, post-hoc addition 4 (b): SDG 16.3.1 proxy.
# Share of young women (15-24) with physical or sexual violence in the past 12 months who sought help from the police or
# legal/justice sources (Correction 4 source coding). Survey-level estimates follow script 83 (logit CIs, PSU clustering,
# continuity correction for zero-event surveys); pooled estimates follow script 80 (sample-size and population weighting).
# Sensitivity: women whose violence was only in the past 12 months (recent_only). Labelled post-hoc and descriptive.
suppressMessages({library(data.table); library(survey)})
ROOT <- Sys.getenv("RR"); OUT2 <- file.path(ROOT, "outputs/tables/P38_review")
s50 <- readLines(file.path(ROOT, "analysis/50_disclosure_predictors.R")); Sys.setenv(P33_SCOPE = "youth", AGE_CONT = "1")
eval(parse(text = s50[2:(grep("^V <- fread", s50)[1] - 1)]))                    # D exactly as in the primary analysis
D[, year := 1900 + mean(v008, na.rm = TRUE) / 12, by = survey]
D[, police_legal := fifelse(is.na(formal), NA_integer_, as.integer(src_police == 1 | src_legal == 1))]   # no source detail -> NA, as for formal
YR <- fread(file.path(OUT2, "P38_B9_population_by_survey.csv"))                  # WorldPop women 15-24 by survey (script 80)
D <- merge(D, YR[, .(survey, pop_f1524, rounds)], by = "survey")
D[, w_pop := w * (pop_f1524 / rounds) / sum(w), by = survey]; D[, w_pop := w_pop / mean(w_pop)]
options(survey.lonely.psu = "adjust")
qlogis_safe <- function(p) qlogis(pmin(pmax(p, 1e-4), 1 - 1e-4))

POP <- list(victim_12m = D[victim_12m == 1], recent_only = D[recent_only == 1])
# survey-level estimates (script 83 rules)
E <- rbindlist(lapply(names(POP), function(pp) {
  d <- POP[[pp]][!is.na(police_legal)]; des <- svydesign(ids = ~clus, weights = ~w, data = d, nest = TRUE)
  r <- as.data.table(svyby(~police_legal, ~survey, des, svyciprop, method = "logit", vartype = "ci", na.rm = TRUE))
  setnames(r, c("survey", "p", "lo", "hi"))
  r <- merge(r, d[, .(country = country[1], region = region[1], year = year[1], n = .N, events = sum(police_legal)), by = survey], by = "survey")
  r[, `:=`(population = pp, outcome = "police_legal", logit = qlogis_safe(p), se = (qlogis_safe(hi) - qlogis_safe(lo)) / (2 * 1.959964))] }))
E[, zero_event_corrected := events == 0 | events == n]
E[zero_event_corrected == TRUE, `:=`(p = (events + 0.5) / (n + 1), logit = qlogis((events + 0.5) / (n + 1)), se = sqrt(1 / (events + 0.5) + 1 / (n - events + 0.5)))]
E[, t := (year - 2015) / 10]
fwrite(E, file.path(OUT2, "P87_sdg1631_survey_estimates.csv"))

# pooled estimates (script 80 rules)
est <- function(d, wt, lab) { d <- d[!is.na(police_legal)]; des <- svydesign(ids = ~clus, weights = as.formula(paste0("~", wt)), data = d, nest = TRUE)
  p <- svyciprop(~police_legal, des, method = "logit")
  data.table(weighting = lab, pct = 100 * as.numeric(p), lo = 100 * confint(p)[1], hi = 100 * confint(p)[2], n = nrow(d), events = sum(d$police_legal)) }
P <- rbindlist(lapply(names(POP), function(pp) { d <- POP[[pp]]; cbind(population = pp, rbindlist(lapply(c("All", "West", "Central", "East", "Southern"), function(r) {
  dr <- if (r == "All") d else d[region == r]
  cbind(region = r, rbind(est(dr, "w", "sample-size"), est(dr, "w_pop", "population"))) }))) }))
fwrite(P, file.path(OUT2, "P87_sdg1631_pooled.csv"))
print(E[, .(surveys = .N, countries = uniqueN(country), events = sum(events), n = sum(n), zero_event = sum(events == 0)), by = population])
print(P[, .(population, region, weighting, est = sprintf("%.2f (%.2f-%.2f)", pct, lo, hi), n, events)])
