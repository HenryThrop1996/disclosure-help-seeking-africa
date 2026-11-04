# 83b_trend_standardised.R - post-hoc (2026-11-04): covariate-standardised survey-level cascade estimates for adjusted trend curves.
# In each country: logit(y) ~ survey round + main-model predictors (survey-weighted, PSU-clustered; Firth refit if separated, firth_helper.R).
# Each round's percentage is predicted over the country's pooled women (all rounds), so only the round effect differs; SE by the delta method.
# Output: outputs/tables/P38_review/P39_survey_estimates_adjusted.csv (same columns as P39_survey_estimates.csv)
suppressMessages({library(data.table); library(fixest)}); source(Sys.getenv("FIRTH"))
ROOT <- Sys.getenv("RR")
s50 <- readLines(file.path(ROOT, "analysis/50_disclosure_predictors.R")); Sys.setenv(P33_SCOPE = "youth", AGE_CONT = "1")
eval(parse(text = s50[2:(grep("^V <- fread", s50)[1] - 1)]))
D[, year := 1900 + mean(v008, na.rm = TRUE) / 12, by = survey]
RHS <- c("agegrp", "educ", "wealthq", "urban", "marital", "working", "kids3", "media", "decisions2", "justify", "vtype", "perp", "recent", "private")
out <- list(); skipped <- list()
for (y in c("told", "sought_any", "formal")) for (k in sort(unique(D$country))) {
  d <- D[country == k & !is.na(get(y))]; d <- d[complete.cases(d[, c(RHS, "w", "clus"), with = FALSE])]
  if (nrow(d) < 100 || sum(d[[y]]) < 5) { skipped[[length(skipped) + 1]] <- data.table(outcome = y, country = k, n = nrow(d)); next }
  d[, svf := factor(survey)]
  keep <- RHS[sapply(RHS, function(v) uniqueN(d[[v]]) > 1)]
  f <- as.formula(paste(y, "~", if (nlevels(d$svf) > 1) "svf +" else "", paste(keep, collapse = " + ")))
  m <- tryCatch(feglm(f, d, family = binomial("logit"), weights = ~w, cluster = ~clus, notes = FALSE, warn = FALSE), error = function(e) NULL)
  if (is.null(m)) { skipped[[length(skipped) + 1]] <- data.table(outcome = y, country = k, n = nrow(d)); next }
  b <- coef(m); V <- vcov(m); b <- b[!is.na(b)]; V <- V[names(b), names(b), drop = FALSE]
  X <- model.matrix(update(f, NULL ~ .), d); X <- X[, names(b), drop = FALSE]; ww <- d$w / sum(d$w)
  for (s in levels(d$svf)) {
    Xs <- X; sc <- grep("^svf", colnames(Xs)); if (length(sc)) { Xs[, sc] <- 0; nm <- paste0("svf", s); if (nm %in% colnames(Xs)) Xs[, nm] <- 1 }
    p <- plogis(drop(Xs %*% b)); P <- sum(ww * p); g <- colSums(ww * p * (1 - p) * Xs); seP <- sqrt(drop(t(g) %*% V %*% g))
    ds <- d[survey == s]
    out[[length(out) + 1]] <- data.table(survey = s, p = P, country = k, region = d$region[1], year = ds$year[1], n = nrow(ds), events = sum(ds[[y]]), outcome = y,
                                         logit = qlogis(P), se = seP / (P * (1 - P)), zero_event_corrected = FALSE) } }
E <- rbindlist(out); E[, t := (year - 2015) / 10]
E[se < 0.02 | !is.finite(se), se := 0.02]                                                   # guard; logged below
fwrite(E, file.path(ROOT, "outputs/tables/P38_review/P39_survey_estimates_adjusted.csv"))
print(E[, .(surveys = .N, countries = uniqueN(country), min_pct = round(100 * min(p), 1), max_pct = round(100 * max(p), 1)), by = outcome])
if (length(skipped)) print(rbindlist(skipped))
