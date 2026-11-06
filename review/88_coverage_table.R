# 88_coverage_table.R - Correction 5, post-hoc addition 4 (d): data coverage by region and survey period.
# Surveys and countries from the Run B survey-level estimates (71 DHS surveys, young women 15-24 with violence).
# Population coverage = women aged 15-24 in 2025 in countries with at least one included survey, as a share of all women aged 15-24
# in the UN subregion (UN WPP 2024, medium variant). The four UN subregions (Western, Middle, Eastern and Southern Africa) are the
# paper's West, Central, East and Southern regions; together they make up WPP's sub-Saharan Africa total.
suppressMessages(library(data.table))
ROOT <- Sys.getenv("RR"); OUT2 <- file.path(ROOT, "outputs/tables/P38_review")
WPP <- fread(cmd = paste("gzcat", shQuote(path.expand("~/Documents/08 Data/other_datasets/WPP2024/WPP2024_PopulationByAge5GroupSex_Medium.csv.gz"))))
SUB <- c(West = 914, Central = 911, East = 910, Southern = 913)                 # WPP LocIDs of the UN subregions
W25 <- WPP[Time == 2025 & AgeGrp %in% c("15-19", "20-24"), .(f1524 = sum(PopFemale) * 1000), by = .(LocID, ISO3_code, LocTypeName, ParentID, Location)]
CTRY <- W25[LocTypeName == "Country/Area" & ParentID %in% SUB][, region := names(SUB)[match(ParentID, SUB)]]

E <- fread(file.path(OUT2, "P39_survey_estimates.csv"))[outcome == "sought_any", .(survey, country, region, year)]
ISO <- fread(file.path(OUT2, "P38_B9_population_by_survey.csv"))[, .(survey, iso3)]
E <- merge(E, ISO, by = "survey", all.x = TRUE)
stopifnot(!anyNA(E$iso3), all(E$iso3 %in% CTRY$ISO3_code))
E[, period := cut(floor(year), c(2003, 2009, 2014, 2019, 2026), labels = c("2004-09", "2010-14", "2015-19", "2020-25"))]

row <- function(lab, e, ctry) {
  covered <- ctry[ISO3_code %in% e$iso3]
  data.table(group = lab, countries = nrow(ctry), surveys = nrow(e), countries_with_data = uniqueN(e$iso3),
             countries_2plus = sum(table(e$iso3) >= 2), pop_coverage_pct = 100 * sum(covered$f1524) / sum(ctry$f1524),
             f1524_millions = sum(ctry$f1524) / 1e6) }
TAB <- rbind(row("Sub-Saharan Africa", E, CTRY),
             rbindlist(lapply(names(SUB), function(r) row(r, E[region == r], CTRY[region == r]))),
             rbindlist(lapply(levels(E$period), function(p) row(p, E[period == p], CTRY))))
fwrite(TAB, file.path(OUT2, "P88_coverage_table.csv"))
fwrite(CTRY[, .(iso3 = ISO3_code, Location, region, f1524, has_data = ISO3_code %in% E$iso3)], file.path(OUT2, "P88_wpp2025_f1524_by_country.csv"))
print(TAB[, .(group, countries, surveys, countries_with_data, countries_2plus, cov = sprintf("%.1f%%", pop_coverage_pct), f1524_millions = round(f1524_millions, 1))])
