# 61g_fig_country_trends.R - Supplementary figure: partially pooled trend in each country with repeated surveys (Pre-reg 39 model; 84b_trend_predictive.py).
# Points and bars: survey-weighted estimate with design-based 95% CI. Line and band: posterior median and 95% CrI of the country's trajectory
# a_c + b_c t, which borrows strength from the regional and overall trend. Panel text: country OR per decade (95% CrI) and Pr(decline).
suppressMessages({library(data.table); library(ggplot2)})
ROOT <- Sys.getenv("RR"); T <- file.path(ROOT, "outputs/tables/P38_review"); FG <- file.path(ROOT, "outputs/figures/disclosure_v3")
FONT <- "Times New Roman"; NAVY <- "#1B2A6B"
LOGIT <- scales::trans_new("logit_pct", function(x) qlogis(x / 100), function(x) 100 * plogis(x)); BRK <- c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 30, 50, 70, 90)   # same y scale as figure 4
md <- function(x, d = 2) gsub(".", "·", formatC(x, format = "f", digits = d), fixed = TRUE)
NAMES <- c(AO = "Angola", BF = "Burkina Faso", BU = "Burundi", CD = "DR Congo", CI = "Côte d'Ivoire", CM = "Cameroon", ET = "Ethiopia", GA = "Gabon", GH = "Ghana", GM = "Gambia",
           KE = "Kenya", ML = "Mali", MW = "Malawi", MZ = "Mozambique", NG = "Nigeria", RW = "Rwanda", SL = "Sierra Leone", SN = "Senegal", TZ = "Tanzania", UG = "Uganda", ZM = "Zambia", ZW = "Zimbabwe", TD = "Chad")
OUT <- c(told = "told anyone", sought_any = "sought any help", formal = "sought formal help")
E <- fread(file.path(T, "P39_survey_estimates.csv")); C <- fread(file.path(T, "P39_country_curves.csv")); S <- fread(file.path(T, "P39_country_slopes.csv"))
for (o in names(OUT)) {
  sl <- S[outcome == o & surveys >= 2]; ks <- sl[order(region, country), country]
  lv <- paste0(NAMES[ks], " (", substr(sl[match(ks, country), region], 1, 1), ")")
  e <- E[outcome == o & country %in% ks]; c <- C[outcome == o & country %in% ks]
  for (d in list(e, c, sl)) d[, panel := factor(paste0(NAMES[country], " (", substr(region, 1, 1), ")"), levels = lv)]
  sl[, txt := sprintf("OR/decade %s (%s–%s)\nPr(decline) %s", md(OR_decade), md(OR_lo), md(OR_hi), md(P_decline))]
  YR <- list(told = c(12, 97), sought_any = c(6, 93), formal = c(0.08, 55))[[o]]
  g <- ggplot() +
    geom_ribbon(data = c, aes(year, ymin = lo, ymax = hi), fill = NAVY, alpha = 0.22) + geom_line(data = c, aes(year, pct), colour = NAVY, linewidth = 0.7) +
    geom_errorbar(data = e, aes(year, ymin = 100 * lo, ymax = 100 * hi), width = 0, colour = "grey35", linewidth = 0.35) +
    geom_point(data = e, aes(year, 100 * p), shape = 21, fill = "white", colour = "black", size = 1.4, stroke = 0.4) +
    geom_text(data = sl, aes(x = 2004, y = YR[2], label = txt), hjust = 0, vjust = 1, family = FONT, size = 2.2, lineheight = 0.9) +
    facet_wrap(~panel, ncol = 5) +
    scale_x_continuous(breaks = seq(2005, 2025, 2), limits = c(2003.5, 2026)) + scale_y_continuous(trans = LOGIT, breaks = BRK, labels = function(b) gsub(".", "·", format(b, drop0trailing = TRUE, trim = TRUE), fixed = TRUE), limits = YR, oob = scales::squish, expand = expansion(mult = c(0.01, 0.01))) +
    labs(x = "Year of survey", y = paste0("Young women with violence who ", OUT[[o]], ", % (logit scale)")) +
    theme_minimal(base_family = FONT, base_size = 9) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), strip.text = element_text(face = "bold", size = 8),
          axis.text = element_text(size = 7), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6.5), axis.line = element_line(colour = "black", linewidth = 0.3), axis.ticks = element_line(colour = "black", linewidth = 0.3))
  nr <- ceiling(length(ks) / 5)
  ragg::agg_png(file.path(FG, sprintf("figS_country_trends_%s.png", o)), width = 7.2, height = 1.55 * nr + 0.5, units = "in", res = 300, background = "white"); print(g); invisible(dev.off())
  cat(o, length(ks), "countries\n") }
