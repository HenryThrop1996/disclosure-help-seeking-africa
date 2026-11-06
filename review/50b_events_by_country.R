# 50b_events_by_country.R - Events per country among young women with violence (DHS, 15-24), to explain which countries could be modelled.
# Country models need at least 300 women and 30 events (50_disclosure_predictors.R, line 42). Women without recorded help sources are excluded, as in the models.
suppressMessages({library(data.table)})
ROOT <- Sys.getenv("RR"); OUT2 <- file.path(ROOT, "outputs/tables/P38_review")
s50 <- readLines(file.path(ROOT, "analysis/50_disclosure_predictors.R")); Sys.setenv(P33_SCOPE = "youth", AGE_CONT = "1")
eval(parse(text = s50[2:(grep("^V <- fread", s50)[1] - 1)]))                    # D exactly as in the primary analysis
E <- rbindlist(lapply(c("formal", "sought_any", "told"), function(o) D[!is.na(get(o)), .(outcome = o, region = region[1], n = .N, events = sum(get(o) == 1)), by = country]))
E[, eligible := n >= 300 & events >= 30]
fwrite(E[order(outcome, -events)], file.path(OUT2, "P50b_events_by_country.csv"))
print(E[, .(countries = .N, eligible = sum(eligible), events = sum(events), n = sum(n), pct = round(100 * sum(events) / sum(n), 1), median_events = median(events)), by = outcome])
print(E[outcome == "formal" & eligible == TRUE, .N, by = region])
