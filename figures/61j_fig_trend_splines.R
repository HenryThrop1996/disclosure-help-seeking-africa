# 61j_fig_trend_splines.R - post-hoc appendix figure: linear versus spline trends in the cascade (young women 15-24, all countries).
# Points: survey-weighted estimates. Dashed: linear model (figure 4). Solid line and band: natural cubic spline, 3 df (posterior median, 95% CrI).
# Thin lines: 2- and 4-df splines and the penalised smooth. Panel text: spline OR per decade by period, LOO difference and likelihood-ratio test.
suppressMessages({library(data.table); library(ggplot2)})
ROOT <- Sys.getenv("RR"); T <- file.path(ROOT, "outputs/tables/P38_review"); FG <- file.path(ROOT, "outputs/figures/disclosure_v3")
FONT <- "Times New Roman"; NAVY <- "#1B2A6B"
md <- function(x, d = 2) gsub(".", "·", formatC(x, format = "f", digits = d), fixed = TRUE)
LOGIT <- scales::trans_new("logit_pct", function(x) qlogis(x / 100), function(x) 100 * plogis(x)); BRK <- c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 30, 50, 70, 90)
OUT <- c(told = "Told anyone", sought_any = "Sought any help", formal = "Sought formal help")
C <- fread(file.path(T, "P39_spline_curves.csv")); S <- fread(file.path(T, "P39_spline_summary.csv")); LR <- fread(file.path(T, "P39_spline_lrt.csv"))
E <- fread(file.path(T, "P39_survey_estimates.csv"))
LO <- fread(file.path(T, "P39_spline_loo.csv"))[model == "ns3", .(outcome, dl = elpd_diff_vs_linear, dse = se_diff)]
lab <- merge(merge(S[model == "ns3"], LO, by = "outcome"), LR, by = "outcome")
lab[, txt := sprintf("Spline OR per decade\n2005–14: %s (%s–%s)\n2015–25: %s (%s–%s)\nΔELPD vs linear: %s (SE %s)\nLikelihood-ratio p=%s",
                     md(OR_2005_14), md(lo_2005_14), md(hi_2005_14), md(OR_2015_25), md(lo_2015_25), md(hi_2015_25),
                     md(dl, 1), md(dse, 1), md(p, 2))]
YL <- data.table(outcome = names(OUT), ymin = c(25, 12, 0.08), ymax = c(97, 93, 60)); lab <- merge(lab, YL, by = "outcome")
for (d in list(C, E, lab, YL)) d[, outcome := factor(OUT[outcome], levels = OUT)]
main <- C[model == "ns3"]; thin <- C[model %in% c("ns2", "ns4", "smooth")]; linc <- C[model == "linear"]
g <- ggplot() + geom_blank(data = YL, aes(2015, ymin)) + geom_blank(data = YL, aes(2015, ymax)) +
  geom_point(data = E, aes(year, 100 * p), colour = "grey60", size = 0.9) +                                        # survey estimates
  geom_ribbon(data = main, aes(year, ymin = lo, ymax = hi), fill = NAVY, alpha = 0.22) +                            # 3-df spline band
  geom_line(data = thin, aes(year, pct, group = model), colour = NAVY, linewidth = 0.35, alpha = 0.6) +             # other splines
  geom_line(data = linc, aes(year, pct), colour = "black", linewidth = 0.6, linetype = "dashed") +                  # linear model
  geom_line(data = main, aes(year, pct), colour = NAVY, linewidth = 0.9) +                                          # 3-df spline
  geom_text(data = lab, aes(x = 2004.3, y = ymax, label = txt), hjust = 0, vjust = 1, family = FONT, size = 2.3, lineheight = 0.9) +
  facet_wrap(~outcome, scales = "free_y", nrow = 1) +
  scale_x_continuous(breaks = seq(2005, 2025, 2), limits = c(2003.5, 2026)) +
  scale_y_continuous(trans = LOGIT, breaks = BRK, labels = function(b) gsub(".", "·", format(b, drop0trailing = TRUE, trim = TRUE), fixed = TRUE), expand = expansion(mult = c(0.01, 0.01))) +
  labs(x = "Year of survey", y = "Young women with violence, % (logit scale)") +
  theme_minimal(base_family = FONT, base_size = 9) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), strip.text = element_text(face = "bold", size = 9),
        axis.text = element_text(size = 7), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6.5),
        axis.line = element_line(colour = "black", linewidth = 0.3), axis.ticks = element_line(colour = "black", linewidth = 0.3))
ragg::agg_png(file.path(FG, "figS_trend_splines.png"), width = 7.2, height = 3.8, units = "in", res = 300, background = "white"); print(g); invisible(dev.off())
cat("figS_trend_splines.png written\n")
