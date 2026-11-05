# 83c_trend_by_age.R - post-hoc (2026-11-05): survey-level cascade estimates by age group (15-24, 25-34, 35-49, 15-49), for trend models by age,
# and a frequentist test of whether the trend differs by age group.
# Outputs: outputs/tables/P38_review/P39_survey_estimates_age.csv, P39_age_interaction_test.csv
suppressMessages({library(data.table); library(survey); library(metafor)}); source(Sys.getenv("FIRTH"))
ROOT <- Sys.getenv("RR")
s50 <- readLines(file.path(ROOT, "analysis/50_disclosure_predictors.R")); Sys.setenv(P33_SCOPE = "all", AGE_CONT = "0")
eval(parse(text = s50[2:(grep("^V <- fread", s50)[1] - 1)]))                    # D = all-age sample, as in the registered analysis
D <- D[v012 >= 15 & v012 <= 49]                                                    # 15-49 (drops the 109 MZ 2015 women aged 50-59)
D[, year := 1900 + mean(v008, na.rm = TRUE) / 12, by = survey]
D[, age := fcase(v012 <= 24, "15-24", v012 <= 34, "25-34", default = "35-49")]
options(survey.lonely.psu = "adjust")
est <- function(d, y, lab) {
  d <- d[!is.na(get(y))]; des <- svydesign(ids = ~clus, weights = ~w, data = d, nest = TRUE)
  r <- as.data.table(svyby(as.formula(paste0("~", y)), ~survey, des, svyciprop, method = "logit", vartype = "ci", na.rm = TRUE)); setnames(r, c("survey", "p", "lo", "hi"))
  r <- merge(r, d[, .(country = country[1], region = region[1], year = year[1], n = .N, events = sum(get(y))), by = survey], by = "survey")
  r[, `:=`(outcome = y, age = lab, logit = qlogis(pmin(pmax(p, 1e-4), 1 - 1e-4)), se = (qlogis(pmin(pmax(hi, 1e-4), 1 - 1e-4)) - qlogis(pmin(pmax(lo, 1e-4), 1 - 1e-4))) / (2 * 1.959964))]
  r[, zero_event_corrected := events == 0 | events == n]
  r[zero_event_corrected == TRUE, `:=`(p = (events + 0.5) / (n + 1), logit = qlogis((events + 0.5) / (n + 1)), se = sqrt(1 / (events + 0.5) + 1 / (n - events + 0.5)))]
  r[se < 0.02 | !is.finite(se), se := 0.02][] }
E <- rbindlist(lapply(c("told", "sought_any", "formal"), function(y) rbind(
  rbindlist(lapply(c("15-24", "25-34", "35-49"), function(a) est(D[age == a], y, a))), est(D, y, "15-49"))))
E[, t := (year - 2015) / 10]
fwrite(E, file.path(ROOT, "outputs/tables/P38_review/P39_survey_estimates_age.csv"))
print(E[, .(surveys = .N, countries = uniqueN(country), zero_corrected = sum(zero_event_corrected), mean_pct = round(100 * mean(p), 1)), by = .(outcome, age)])
# interaction test: does the slope differ by age group? (three age groups only; random intercept + slope by country, random effect for survey)
T <- rbindlist(lapply(c("told", "sought_any", "formal"), function(y) {
  x <- E[outcome == y & age != "15-49"]; x[, age := factor(age, levels = c("15-24", "25-34", "35-49"))]
  m <- rma.mv(logit, se^2, mods = ~ t * age, random = list(~ t | country, ~ 1 | survey), struct = "GEN", data = x, method = "REML")
  a <- anova(m, btt = grep(":", rownames(m$b)))                                     # Wald test of t x age
  b <- coef(m); V <- vcov(m)
  sl <- data.table(age = c("15-24", "25-34", "35-49"),
                   est = c(b["t"], b["t"] + b["t:age25-34"], b["t"] + b["t:age35-49"]),
                   se = sqrt(c(V["t", "t"], V["t", "t"] + V["t:age25-34", "t:age25-34"] + 2 * V["t", "t:age25-34"], V["t", "t"] + V["t:age35-49", "t:age35-49"] + 2 * V["t", "t:age35-49"])))
  sl[, `:=`(outcome = y, OR = exp(est), lo = exp(est - 1.96 * se), hi = exp(est + 1.96 * se), p_interaction = a$QMp)][] }))
fwrite(T, file.path(ROOT, "outputs/tables/P38_review/P39_age_interaction_test.csv"))
print(T[, .(outcome, age, OR = round(OR, 2), lo = round(lo, 2), hi = round(hi, 2), p_interaction = signif(p_interaction, 2))])
