# 87c_sdg1631_definitions.R - SDG 16.3.1 proxy under wider definitions of the authorities reported to.
# Same women as 87_sdg1631_proxy.R (violence in the past 12 months, source detail recorded); police or legal as in 87, then adding
# health workers, chiefs or local authorities, and religious or traditional leaders (coding audit, 41d_correction4.py; a missing flag counts as not named).
# Population-weighted percentages (script 80 weights) with survey-design 95% CIs (logit method, PSU clustering).
suppressMessages({library(data.table); library(survey)})
ROOT <- Sys.getenv("RR"); OUT2 <- file.path(ROOT, "outputs/tables/P38_review")
s50 <- readLines(file.path(ROOT, "analysis/50_disclosure_predictors.R")); Sys.setenv(P33_SCOPE = "youth", AGE_CONT = "1")
eval(parse(text = s50[2:(grep("^V <- fread", s50)[1] - 1)]))                    # D exactly as in the primary analysis
D[, police_legal := fifelse(is.na(formal), NA_integer_, as.integer(src_police == 1 | src_legal == 1))]   # as in 87
YR <- fread(file.path(OUT2, "P38_B9_population_by_survey.csv")); D <- merge(D, YR[, .(survey, pop_f1524, rounds)], by = "survey")
D[, w_pop := w * (pop_f1524 / rounds) / sum(w), by = survey]; D[, w_pop := w_pop / mean(w_pop)]
options(survey.lonely.psu = "adjust")
f <- function(x) fifelse(is.na(x), 0L, as.integer(x == 1))
d <- D[victim_12m == 1 & !is.na(police_legal)]
d[, `:=`(h = f(src_health), la = f(src_local_authority), rt = f(src_religious_traditional))]
DEF <- list(police_legal = quote(police_legal), plus_health = quote(police_legal | h), plus_local = quote(police_legal | la),
            plus_health_local = quote(police_legal | h | la), plus_all = quote(police_legal | h | la | rt))
R <- rbindlist(lapply(names(DEF), function(k) {
  d[, y := as.integer(eval(DEF[[k]]))]
  des <- svydesign(ids = ~clus, weights = ~w_pop, data = d, nest = TRUE); p <- svyciprop(~y, des, method = "logit")
  data.table(definition = k, pct = 100 * as.numeric(p), lo = 100 * confint(p)[1], hi = 100 * confint(p)[2], events = sum(d$y), n = nrow(d)) }))
fwrite(R, file.path(OUT2, "P87_sdg1631_definitions.csv")); print(R)
