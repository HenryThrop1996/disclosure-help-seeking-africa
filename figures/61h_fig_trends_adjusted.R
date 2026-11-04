# 61h_fig_trends_adjusted.R - post-hoc appendix figure: trend curves before and after standardising for the women's characteristics.
# Both curves from the P39 hierarchical meta-regression fitted to the same surveys: survey-weighted (unadjusted) and covariate-standardised (83b) estimates.
suppressMessages({library(data.table); library(ggplot2)})
ROOT <- Sys.getenv("RR"); T <- file.path(ROOT, "outputs/tables/P38_review"); FG <- file.path(ROOT, "outputs/figures/disclosure_v3")
FONT <- "Times New Roman"; COL <- c(Unadjusted = "#8C8C8C", Adjusted = "#1B2A6B")
md <- function(x, d = 2) gsub(".", "·", formatC(x, format = "f", digits = d), fixed = TRUE)
LOGIT <- scales::trans_new("logit_pct", function(x) qlogis(x / 100), function(x) 100 * plogis(x)); BRK <- c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 30, 50, 70, 90)
OUT <- c(told = "Told anyone", sought_any = "Sought any help", formal = "Sought formal help"); SC <- c(All = "All countries", West = "West Africa", Central = "Central Africa", East = "East Africa")
C <- rbind(cbind(fread(file.path(T, "P39_curves_crude_samesurveys.csv")), fit = "Unadjusted"), cbind(fread(file.path(T, "P39_curves_adjusted.csv")), fit = "Adjusted"), fill = TRUE)
S <- rbind(cbind(fread(file.path(T, "P39_slopes_crude_samesurveys.csv")), fit = "Unadjusted"), cbind(fread(file.path(T, "P39_slopes_adjusted.csv")), fit = "Adjusted"), fill = TRUE)
C <- C[scope %in% names(SC)]; S <- S[scope %in% names(SC)]
lab <- dcast(S, outcome + scope ~ fit, value.var = c("OR_decade", "OR_lo", "OR_hi"))
lab[, txt := sprintf("Unadjusted %s (%s–%s)\nAdjusted %s (%s–%s)", md(OR_decade_Unadjusted), md(OR_lo_Unadjusted), md(OR_hi_Unadjusted), md(OR_decade_Adjusted), md(OR_lo_Adjusted), md(OR_hi_Adjusted))]
YL <- data.table(outcome = names(OUT), ymin = c(25, 15, 0.4), ymax = c(80, 65, 12)); lab <- merge(lab, YL, by = "outcome")
blank <- YL[rep(1:3, each = 4)][, scope := rep(names(SC), 3)]
for (d in list(C, lab, blank)) { d[, outcome := factor(OUT[outcome], levels = OUT)]; d[, scope := factor(SC[scope], levels = SC)] }
C[, fit := factor(fit, levels = names(COL))]
g <- ggplot() + geom_blank(data = blank, aes(2015, ymin)) + geom_blank(data = blank, aes(2015, ymax)) +
  geom_ribbon(data = C, aes(year, ymin = lo, ymax = hi, fill = fit), alpha = 0.18) + geom_line(data = C, aes(year, pct, colour = fit, linetype = fit), linewidth = 0.7) +
  geom_text(data = lab, aes(x = 2004.3, y = ymax, label = txt), hjust = 0, vjust = 1, family = FONT, size = 2.3, lineheight = 0.9) +
  facet_grid(outcome ~ scope, scales = "free_y", switch = "y") +
  scale_colour_manual(values = COL, name = NULL) + scale_fill_manual(values = COL, name = NULL) + scale_linetype_manual(values = c(Unadjusted = "dashed", Adjusted = "solid"), name = NULL) +
  scale_x_continuous(breaks = seq(2005, 2025, 2), limits = c(2003.5, 2026)) +
  scale_y_continuous(trans = LOGIT, breaks = BRK, labels = function(b) gsub(".", "·", format(b, drop0trailing = TRUE, trim = TRUE), fixed = TRUE), expand = expansion(mult = c(0.01, 0.01))) +
  labs(x = "Year of survey", y = "Young women with violence, % (logit scale)") +
  theme_minimal(base_family = FONT, base_size = 9) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), strip.placement = "outside", strip.text = element_text(face = "bold", size = 9),
        axis.text = element_text(size = 7), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6.5), panel.spacing = unit(0.5, "lines"),
        axis.line = element_line(colour = "black", linewidth = 0.3), axis.ticks = element_line(colour = "black", linewidth = 0.3))
ragg::agg_png(file.path(FG, "figS_trends_adjusted.png"), width = 7.2, height = 7.4, units = "in", res = 300, background = "white"); print(g); invisible(dev.off())
cat("figS_trends_adjusted.png written\n")
