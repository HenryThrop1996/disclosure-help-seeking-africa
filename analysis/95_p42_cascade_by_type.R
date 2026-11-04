# 95_p42_cascade_by_type.R - Pre-registration 42, A: survey-weighted cascade (told, sought any help, formal help) by perpetrator x type of violence,
# for the four non-exclusive types and the seven mutually exclusive profiles; all regions and each UN region. Logit 95% CIs, PSU clustering (as scripts 59/80).
suppressMessages({library(data.table); library(survey)})
ROOT <- Sys.getenv("RR"); OUTP <- file.path(ROOT, "outputs/tables/P42")
s50 <- readLines(file.path(ROOT, "analysis/50_disclosure_predictors.R")); Sys.setenv(P33_SCOPE = "youth", AGE_CONT = "1")
eval(parse(text = s50[2:(grep("^V <- fread", s50)[1] - 1)]))
OUT <- OUTP
TY <- fread(file.path(ROOT, "data/derived/women_perp_type.csv.gz"), colClasses = list(character = "caseid"))
D[, caseid := trimws(as.character(caseid))]; D <- merge(D, TY, by = c("survey", "caseid"))
D[, profile := fcase(PP == 1 & PS == 0 & NP == 0 & NS == 0, "Partner physical only", PP == 0 & PS == 1 & NP == 0 & NS == 0, "Partner sexual only",
                     PP == 1 & PS == 1 & NP == 0 & NS == 0, "Partner physical and sexual", PP == 0 & PS == 0 & NP == 1 & NS == 0, "Non-partner physical only",
                     PP == 0 & PS == 0 & NP == 0 & NS == 1, "Non-partner sexual only", PP == 0 & PS == 0 & NP == 1 & NS == 1, "Non-partner physical and sexual",
                     default = "Partner and non-partner")]
fwrite(D[, .(survey, caseid, PP, PS, NP, NS, profile, d127_resolved)], file.path(OUTP, "P42_analysis_keys.csv.gz"))
cat(sprintf("Young victims: %s | PP %.1f%% PS %.1f%% NP %.1f%% NS %.1f%% (weighted)\n", format(nrow(D), big.mark = ","),
            100 * D[, weighted.mean(PP, w)], 100 * D[, weighted.mean(PS, w)], 100 * D[, weighted.mean(NP, w)], 100 * D[, weighted.mean(NS, w)]))
options(survey.lonely.psu = "adjust")
est <- function(d, y) { d <- d[!is.na(get(y))]; if (nrow(d) < 50) return(data.table(pct = NA_real_, lo = NA_real_, hi = NA_real_, n = nrow(d)))
  des <- svydesign(ids = ~clus, weights = ~w, data = d, nest = TRUE); p <- svyciprop(as.formula(paste0("~", y)), des, method = "logit")
  data.table(pct = 100 * as.numeric(p), lo = 100 * confint(p)[1], hi = 100 * confint(p)[2], n = nrow(d)) }
GROUPS <- c(list(`Partner physical` = quote(PP == 1), `Partner sexual` = quote(PS == 1), `Non-partner physical` = quote(NP == 1), `Non-partner sexual` = quote(NS == 1)),
            setNames(lapply(c("Partner physical only", "Partner sexual only", "Partner physical and sexual", "Non-partner physical only", "Non-partner sexual only",
                              "Non-partner physical and sexual", "Partner and non-partner"), function(p) bquote(profile == .(p))),
                     c("Partner physical only", "Partner sexual only", "Partner physical and sexual", "Non-partner physical only", "Non-partner sexual only",
                       "Non-partner physical and sexual", "Partner and non-partner")),
            list(`All young victims` = quote(TRUE)))
R <- rbindlist(lapply(names(GROUPS), function(g) rbindlist(lapply(c("All", "West", "Central", "East", "Southern"), function(rg) {
  d <- D[eval(GROUPS[[g]])]; if (rg != "All") d <- d[region == rg]
  rbindlist(lapply(c("told", "sought_any", "formal"), function(y) cbind(group = g, set = if (g %in% c("Partner physical", "Partner sexual", "Non-partner physical", "Non-partner sexual")) "type (non-exclusive)" else if (g == "All young victims") "all" else "profile (mutually exclusive)",
                                                                   region = rg, outcome = y, share_of_victims = 100 * D[eval(GROUPS[[g]]), sum(w)] / D[, sum(w)], est(d, y)))) }))))
fwrite(R, file.path(OUTP, "P42_cascade_by_type.csv"))
pr <- R[region == "All", .(group, outcome, est = fifelse(is.na(pct), "n<50", sprintf("%.1f (%.1f-%.1f)", pct, lo, hi)), n, share = round(share_of_victims, 1))]
print(dcast(pr, group + share ~ outcome, value.var = "est")[order(-share)])
# sensitivity C1: surveys where d127 resolves the perpetrator of forced sex
R1 <- rbindlist(lapply(c("Partner sexual", "Non-partner sexual", "Partner sexual only", "Non-partner sexual only"), function(g)
  rbindlist(lapply(c("told", "sought_any", "formal"), function(y) cbind(group = g, outcome = y, est(D[d127_resolved == 1][eval(GROUPS[[g]])], y))))))
fwrite(R1, file.path(OUTP, "P42_C1_d127_resolved.csv")); cat("\nC1 (d127 asked):\n"); print(R1[, .(group, outcome, est = sprintf("%.1f (%.1f-%.1f)", pct, lo, hi), n)])
