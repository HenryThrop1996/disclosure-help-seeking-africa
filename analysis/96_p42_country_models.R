# 96_p42_country_models.R - Pre-registration 42, B: country models with the four perpetrator x type indicators (mutually adjusted), replacing vtype and perp.
# Stage 1 as Pre-reg 33: survey-weighted logit, survey-round fixed effects, PSU-clustered SEs; >= 300 women and >= 30 events; terms with finite SE < 5.
# Pre-specified contrasts per country from the coefficient covariance: NS - PS and NP - PP (log OR ratios). C2: single-type profiles (reference PP only).
suppressMessages({library(data.table); library(fixest)}); source(Sys.getenv("FIRTH"))
ROOT <- Sys.getenv("RR")
s50 <- readLines(file.path(ROOT, "analysis/50_disclosure_predictors.R")); Sys.setenv(P33_SCOPE = "youth", AGE_CONT = "1")
eval(parse(text = s50[2:(grep("^V <- fread", s50)[1] - 1)]))
OUT <- file.path(ROOT, "outputs/tables/P42")
K <- fread(file.path(OUT, "P42_analysis_keys.csv.gz"), colClasses = list(character = "caseid"))
D[, caseid := trimws(as.character(caseid))]; D <- merge(D, K, by = c("survey", "caseid"))
RHS <- "agegrp + educ + wealthq + urban + marital + working + kids3 + media + decisions2 + justify + recent + private + PP + PS + NP + NS"
TERMS <- c("PP", "PS", "NP", "NS")
fit <- function(d, y) { d <- d[!is.na(get(y))]; if (nrow(d) < 300 || sum(d[[y]]) < 30) return(NULL)
  m <- tryCatch(feglm(as.formula(paste(y, "~", RHS, "| survey")), d, family = binomial("logit"), weights = ~w, cluster = ~clus, notes = FALSE, warn = FALSE), error = function(e) NULL)
  if (is.null(m)) return(NULL); b <- coef(m); V <- vcov(m); k <- intersect(TERMS, names(b)); k <- k[is.finite(sqrt(diag(V)[k])) & sqrt(diag(V)[k]) < 5]
  r <- data.table(term = k, est = b[k], se = sqrt(diag(V)[k]))
  for (cc in list(c("NS", "PS"), c("NP", "PP"))) if (all(cc %in% k)) r <- rbind(r, data.table(term = paste0(cc[1], "_minus_", cc[2]), est = b[cc[1]] - b[cc[2]],
                                                                                     se = sqrt(V[cc[1], cc[1]] + V[cc[2], cc[2]] - 2 * V[cc[1], cc[2]])))
  r[, `:=`(n = nobs(m), events = sum(d[[y]]))][] }
fitP <- function(d, y) { d <- d[profile %in% c("Partner physical only", "Partner sexual only", "Non-partner physical only", "Non-partner sexual only") & !is.na(get(y))]
  d[, prof := factor(profile, levels = c("Partner physical only", "Partner sexual only", "Non-partner physical only", "Non-partner sexual only"))]
  if (nrow(d) < 300 || sum(d[[y]]) < 30) return(NULL)
  m <- tryCatch(feglm(as.formula(paste(y, "~ prof + agegrp + educ + wealthq + urban + marital + working + kids3 + media + decisions2 + justify + recent + private | survey")),
                      d, family = binomial("logit"), weights = ~w, cluster = ~clus, notes = FALSE, warn = FALSE), error = function(e) NULL)
  if (is.null(m)) return(NULL); b <- coef(m); s <- se(m); k <- grep("^prof", names(b), value = TRUE); k <- k[is.finite(s[k]) & s[k] < 5]
  if (!length(k)) return(NULL); data.table(term = sub("^prof", "", k), est = b[k], se = s[k], n = nobs(m), events = sum(d[[y]])) }
E <- list()
for (k in sort(unique(D$country))) for (y in c("told", "sought_any", "formal")) {
  r <- fit(D[country == k], y); if (!is.null(r)) E[[length(E) + 1]] <- cbind(analysis = "B_types", model = "main", outcome = y, country = k, region = REG[[k]], r)
  r <- fit(D[country == k & d127_resolved == 1], y); if (!is.null(r)) E[[length(E) + 1]] <- cbind(analysis = "C1_d127", model = "main", outcome = y, country = k, region = REG[[k]], r)
  r <- fitP(D[country == k], y); if (!is.null(r)) E[[length(E) + 1]] <- cbind(analysis = "C2_profiles", model = "main", outcome = y, country = k, region = REG[[k]], r) }
E <- rbindlist(E); fwrite(E, file.path(OUT, "P42_country_estimates.csv"))
print(E[, .(countries = uniqueN(country)), by = .(analysis, outcome, term)][order(analysis, outcome, term)], nrows = 100)
