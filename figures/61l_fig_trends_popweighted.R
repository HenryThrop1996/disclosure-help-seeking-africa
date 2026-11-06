# 61l_fig_trends_popweighted.R - Correction 5, post-hoc addition 5: headline trend figure (layout and colours after Strain et al. 2026, figure 3).
# Columns: all countries, then each region (one colour per region). Rows: told anyone, sought any help, sought formal help.
# Line and dots: population-weighted average of the countries' fitted trajectories (posterior median; 84f); grey band: 95% CrI.
# Linear percentage axis from 0; each row has its own range. Southern Africa (no repeated surveys) is dashed: its curve comes from pooling only.
suppressMessages({library(data.table); library(ggplot2)})
ROOT <- Sys.getenv("RR"); T <- file.path(ROOT, "outputs/tables/P38_review"); FG <- file.path(ROOT, "outputs/figures/disclosure_v3")
FONT <- "Times New Roman"
md <- function(x, d = 1) sub("^-", "−", gsub(".", "·", formatC(x, format = "f", digits = d), fixed = TRUE))   # mid-dot decimals, true minus sign
OUT <- c(told = "Told anyone (%)", sought_any = "Sought any help (%)", formal = "Sought formal help (%)")
SC <- c(All = "Sub-Saharan Africa\n(33 countries)", West = "West Africa", Central = "Central Africa", East = "East Africa", Southern = "Southern Africa")
COL <- c(All = "#1F5FA8", West = "#C0392B", Central = "#2E8B3E", East = "#1A93A6", Southern = "#7B5BA6")
C <- fread(file.path(T, "P39pw_curves.csv")); S <- fread(file.path(T, "P39pw_summary.csv"))
C[, sc := scope]; S[, sc := scope]
S[, txt := ifelse(scope == "Southern", sprintf("2005–25: %s pp\n(no repeated surveys;\ncurve from pooling only)", md(change_pp)),
                  sprintf("2005–25: %s pp\n(%s to %s)\nPr(decline) %s", md(change_pp), md(change_lo), md(change_hi),
                          ifelse(P_decline > 0.999, ">0·999", ifelse(P_decline < 0.001, "<0·001", md(P_decline, 3)))))]
YL <- data.table(outcome = rep(names(OUT), each = 2), y = c(0, 85, 0, 64, 0, 15)); YT <- YL[, .(ytop = max(y)), by = outcome]
S <- merge(S, YT, by = "outcome")
for (d in list(C, S, YL)) d[, outcome := factor(OUT[outcome], levels = OUT)]
for (d in list(C, S)) d[, scope := factor(SC[scope], levels = SC)]
DOTS <- C[year %% 2 == 1]                                                    # dots every other year (2005, 2007, ..., 2025)
g <- ggplot() +
  geom_blank(data = YL, aes(2015, y)) +
  geom_ribbon(data = C, aes(year, ymin = lo, ymax = hi, group = scope), fill = "grey75", alpha = 0.55) +
  geom_line(data = C, aes(year, pct, colour = sc, linetype = sc == "Southern"), linewidth = 0.7) +
  geom_point(data = DOTS, aes(year, pct, colour = sc), size = 0.9) +
  geom_text(data = S, aes(2004.4, ytop, label = txt), hjust = 0, vjust = 1, family = FONT, size = 2.25, lineheight = 0.9) +
  facet_grid(outcome ~ scope, scales = "free_y", switch = "y") +
  scale_colour_manual(values = COL, guide = "none") + scale_linetype_manual(values = c(`FALSE` = "solid", `TRUE` = "22"), guide = "none") +
  scale_x_continuous(breaks = seq(2005, 2025, 5), limits = c(2004, 2025.5)) +
  scale_y_continuous(breaks = scales::breaks_pretty(4), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Year", y = NULL) +
  theme_minimal(base_family = FONT, base_size = 9) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.3),
        strip.text.x = element_text(size = 8.5, lineheight = 0.9), strip.text.y.left = element_text(size = 8.5, angle = 90), strip.placement = "outside",
        axis.text = element_text(size = 7), axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_line(colour = "black", linewidth = 0.3), axis.ticks = element_line(colour = "black", linewidth = 0.3), panel.spacing = unit(0.7, "lines"))
ragg::agg_png(file.path(FG, "fig_trends_popweighted.png"), width = 7.4, height = 6.0, units = "in", res = 300, background = "white"); print(g); invisible(dev.off())
cat("fig_trends_popweighted.png written\n")
