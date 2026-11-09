# 61n_fig_trends_countries.R - regional trends with individual country trajectories, layout after Strain et al. 2026, figure 1B (2026-11-09).
# Columns: all 33 countries, then each region. Rows: told anyone, sought any help, sought formal help.
# Grey: each country's fitted trajectory over its survey years (posterior median, P39 model; a dot where a country has one survey).
# Highlighted: the population-weighted regional curve (84f; posterior median, 95% CrI band), with its 2005 and 2025 values printed at the ends.
# Southern Africa (no repeated surveys) is dashed. Linear percentage axis from 0; each row has its own range. Presentation of existing model output only.
suppressMessages({library(data.table); library(ggplot2)})
ROOT <- Sys.getenv("RR"); T <- file.path(ROOT, "outputs/tables/P38_review"); FG <- file.path(ROOT, "outputs/figures/disclosure_v3"); FONT <- "Times New Roman"
md <- function(x, d = 0) gsub(".", "·", formatC(x, format = "f", digits = d), fixed = TRUE)
OUT <- c(told = "Told anyone (%)", sought_any = "Sought any help (%)", formal = "Sought formal help (%)")
SC <- c(All = "Sub-Saharan Africa\n(33 countries)", West = "West Africa", Central = "Central Africa", East = "East Africa", Southern = "Southern Africa")
COL <- c(All = "#1F5FA8", West = "#C0392B", Central = "#2E8B3E", East = "#1A93A6", Southern = "#7B5BA6")
PW <- fread(file.path(T, "P39pw_curves.csv")); CC <- fread(file.path(T, "P39_country_curves.csv"))
CC <- rbind(copy(CC)[, scope := "All"], copy(CC)[, scope := region]); CC[, n := uniqueN(round(year, 1)), by = .(outcome, scope, country)]
PW[, col := scope]; CC[, col := scope]
for (d in list(PW, CC)) { d[, outcome := factor(OUT[outcome], levels = OUT)]; d[, scope := factor(SC[scope], levels = SC)] }
END <- PW[year %in% c(2005, 2025)]; END[, `:=`(lab = fifelse(grepl("formal", outcome), md(pct, 1), md(pct, 0)), hj = fifelse(year == 2005, 1.35, -0.35))]
YL <- data.table(outcome = factor(OUT, levels = OUT), y = c(85, 60, 12))
g <- ggplot() + geom_blank(data = YL, aes(2015, y)) +
  geom_line(data = CC[n > 1], aes(year, pct, group = country), colour = "grey65", linewidth = 0.3) +
  geom_point(data = CC[n == 1], aes(year, pct), colour = "grey55", size = 0.6) +
  geom_ribbon(data = PW, aes(year, ymin = lo, ymax = hi, fill = col), alpha = 0.18) +
  geom_line(data = PW, aes(year, pct, colour = col, linetype = col == "Southern"), linewidth = 0.9) +
  geom_point(data = END, aes(year, pct, colour = col), size = 1.3) +
  geom_text(data = END, aes(year, pct, label = lab, colour = col, hjust = hj), family = FONT, fontface = "bold", size = 2.4) +
  facet_grid(outcome ~ scope, scales = "free_y", switch = "y") +
  scale_colour_manual(values = COL, guide = "none") + scale_fill_manual(values = COL, guide = "none") +
  scale_linetype_manual(values = c(`FALSE` = "solid", `TRUE` = "22"), guide = "none") +
  scale_x_continuous(breaks = seq(2005, 2025, 5), limits = c(2000.5, 2029.5)) + scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.03))) +
  labs(x = "Year", y = NULL) + theme_minimal(base_family = FONT, base_size = 9) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
        strip.text.x = element_text(size = 8.5, lineheight = 0.9), strip.text.y.left = element_text(size = 8.5, angle = 90), strip.placement = "outside",
        axis.text = element_text(size = 7), axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_line(colour = "black", linewidth = 0.3), axis.ticks = element_line(colour = "black", linewidth = 0.3), panel.spacing = unit(0.7, "lines"))
ragg::agg_png(file.path(FG, "fig_trends_countries.png"), width = 7.4, height = 6.2, units = "in", res = 300, background = "white"); print(g); invisible(dev.off())
cat("fig_trends_countries.png written\n")
