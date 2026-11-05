# 61i_fig_trends_by_age.R - post-hoc appendix figure: trends in the cascade by age group (all countries).
# Curves and bands: posterior median and 95% CrI of the mean curve from the P39 model fitted separately to each age group (84_trend_model_age.py).
suppressMessages({library(data.table); library(ggplot2)})
ROOT <- Sys.getenv("RR"); T <- file.path(ROOT, "outputs/tables/P38_review"); FG <- file.path(ROOT, "outputs/figures/disclosure_v3")
FONT <- "Times New Roman"; COL <- c("15-24" = "#1B2A6B", "25-34" = "#C26A1F", "35-49" = "#4C9A8A")   # navy = the paper's age group
md <- function(x, d = 2) gsub(".", "·", formatC(x, format = "f", digits = d), fixed = TRUE)
LOGIT <- scales::trans_new("logit_pct", function(x) qlogis(x / 100), function(x) 100 * plogis(x)); BRK <- c(0.5, 1, 2, 5, 10, 20, 30, 50, 70)
OUT <- c(told = "Told anyone", sought_any = "Sought any help", formal = "Sought formal help")
C <- fread(file.path(T, "P39_curves_age.csv"))[scope == "All" & age != "15-49"]
S <- fread(file.path(T, "P39_slopes_age.csv"))[scope == "All" & age != "15-49"]
P <- unique(fread(file.path(T, "P39_age_interaction_test.csv"))[, .(outcome, p_interaction)])
lab <- S[, .(txt = paste(sprintf("%s: %s (%s–%s)", age, md(OR_decade), md(OR_lo), md(OR_hi)), collapse = "\n")), by = outcome]
lab <- merge(lab, P, by = "outcome"); lab[, txt := paste0("OR per decade\n", txt, "\nAge × time p=", md(p_interaction, 2))]
YL <- data.table(outcome = names(OUT), ymin = c(25, 18, 0.8), ymax = c(85, 75, 25)); lab <- merge(lab, YL, by = "outcome")
for (d in list(C, lab, YL)) d[, outcome := factor(OUT[outcome], levels = OUT)]
g <- ggplot() + geom_blank(data = YL, aes(2015, ymin)) + geom_blank(data = YL, aes(2015, ymax)) +
  geom_ribbon(data = C, aes(year, ymin = lo, ymax = hi, fill = age), alpha = 0.15) + geom_line(data = C, aes(year, pct, colour = age), linewidth = 0.8) +
  geom_text(data = lab, aes(x = 2004.3, y = ymax, label = txt), hjust = 0, vjust = 1, family = FONT, size = 2.5, lineheight = 0.9) +
  facet_wrap(~outcome, scales = "free_y", nrow = 1) +
  scale_colour_manual(values = COL, name = "Age, years") + scale_fill_manual(values = COL, name = "Age, years") +
  scale_x_continuous(breaks = seq(2005, 2025, 2), limits = c(2003.5, 2026)) +
  scale_y_continuous(trans = LOGIT, breaks = BRK, labels = function(b) gsub(".", "·", format(b, drop0trailing = TRUE, trim = TRUE), fixed = TRUE), expand = expansion(mult = c(0.01, 0.01))) +
  labs(x = "Year of survey", y = "Women with violence, % (logit scale)") +
  theme_minimal(base_family = FONT, base_size = 9) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), strip.text = element_text(face = "bold", size = 9),
        axis.text = element_text(size = 7), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6.5),
        axis.line = element_line(colour = "black", linewidth = 0.3), axis.ticks = element_line(colour = "black", linewidth = 0.3))
ragg::agg_png(file.path(FG, "figS_trends_by_age.png"), width = 7.2, height = 3.6, units = "in", res = 300, background = "white"); print(g); invisible(dev.off())
cat("figS_trends_by_age.png written\n")
