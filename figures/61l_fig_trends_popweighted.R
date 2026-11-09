# 61l_fig_trends_popweighted.R - headline trend figure in the layout of Strain et al. 2026, figure 3 (revised 2026-11-09 after co-author review).
# Columns: all 33 countries, then each region. Row 1: stacked distribution of young survivors across four mutually exclusive groups
#   (told no one; told someone but sought no help; sought informal help only; sought formal help), from the posterior medians of the
#   population-weighted curves of told anyone, sought any help and sought formal help (84f; the curves are nested in every year and region), in 2005 and 2025, values printed on the bars.
# Rows 2-3: sought any help and sought formal help - line: population-weighted average of the countries' fitted trajectories (posterior median),
#   grey band: 95% CrI; hollow points and vertical lines: observed survey-weighted percentage in each survey with its design-based 95% CI.
# Bands, darkest to lightest: 50% CrI, 95% CrI (84f), 95% prediction interval for a new country (84g, P39_pred_band_full.csv, 2004-2025 for every region: true percentage in a survey of a new country; all countries: also
#   a new region), nested bands after Andrich & Eltigani's heat and sexual violence draft.
# Axes: sought any help 0-100% (as the top row), formal help 0-30%, so no band or interval is cut off.
# Southern Africa (no repeated surveys) is dashed: its trend comes from pooling only.
suppressMessages({library(data.table); library(ggplot2); library(patchwork)})
ROOT <- Sys.getenv("RR"); T <- file.path(ROOT, "outputs/tables/P38_review"); FG <- file.path(ROOT, "outputs/figures/disclosure_v3"); FONT <- "Times New Roman"
SC <- c(All = "Sub-Saharan Africa\n(33 countries)", West = "West Africa", Central = "Central Africa", East = "East Africa", Southern = "Southern Africa\n(no repeated surveys)")
C <- fread(file.path(T, "P39pw_curves.csv")); C[, scope := factor(SC[scope], levels = SC)]
PB <- fread(file.path(T, "P39_pred_band_full.csv")); PB[, scope := factor(SC[scope], levels = SC)]
OB <- fread(file.path(T, "P39_survey_estimates.csv"))[, .(outcome, region, year, pct = 100 * p, lo = 100 * lo, hi = 100 * hi)]
OB <- rbind(copy(OB)[, scope := "All"], copy(OB)[, scope := region]); OB[, scope := factor(SC[scope], levels = SC)]
W <- dcast(C, scope + year ~ outcome, value.var = "pct")
G <- c("Told no one", "Told someone, sought no help", "Sought informal help only", "Sought formal help")
ST <- rbind(W[, .(scope, year, group = G[4], pct = formal)], W[, .(scope, year, group = G[3], pct = sought_any - formal)],
            W[, .(scope, year, group = G[2], pct = told - sought_any)], W[, .(scope, year, group = G[1], pct = 100 - told)])
ST[, group := factor(group, levels = G)]
FILL <- setNames(c("#EE8A3B", "#F7C873", "#A9C6E8", "#2F6DB5"), G)          # orange = least disclosure, blue = formal help (after Strain et al.)
th <- theme_minimal(base_family = FONT, base_size = 8) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.25),
        axis.line = element_line(colour = "black", linewidth = 0.3), axis.ticks = element_line(colour = "black", linewidth = 0.3),
        axis.text = element_text(size = 6.5), strip.text = element_text(size = 7.5, lineheight = 0.9), panel.spacing = unit(0.6, "lines"),
        legend.position = "top", legend.justification = "left", legend.title = element_text(size = 7.5, face = "bold"), legend.text = element_text(size = 7),
        legend.key.size = unit(0.3, "cm"), legend.margin = margin(0, 0, 0, 0))
xs <- scale_x_continuous(breaks = c(2005, 2010, 2015, 2020, 2025), limits = c(2004, 2025.6))
GL <- setNames(c("Told no one", "Told, no help", "Informal only", "Formal help"), G)
YRS <- c(2005, 2015, 2025); YCOL <- setNames(c("#C9D6E3", "#7090B0", "#2F5D8A"), YRS)                  # compromise agreed 2026-11-09: three years
BR <- ST[year %in% c(2005, 2025)][, `:=`(yr = factor(year, levels = c(2025, 2005)), grp = factor(GL[as.character(group)], levels = rev(GL)),
                                       lab = fifelse(group == G[4], gsub(".", "\u00b7", sprintf("%.1f", pct), fixed = TRUE), sprintf("%.0f", pct)))]
GQ <- fread(file.path(T, "P39pw_groups_5y.csv"))[year %in% YRS]; GQ[, group := c(`Told no one` = G[1], `Told someone, no help` = G[2], `Informal help only` = G[3], `Formal help` = G[4])[group]]   # 84h: 95% CrIs every five years
GQ[, `:=`(scope = factor(SC[scope], levels = SC), grp = factor(GL[group], levels = rev(GL)), yr = factor(year, levels = rev(YRS)),
          lab = fifelse(group == G[4], gsub(".", "\u00b7", sprintf("%.1f", pct), fixed = TRUE), sprintf("%.0f", pct)))]
stopifnot(!anyNA(GQ$grp))
DG <- position_dodge(width = 0.86)
p1 <- ggplot(GQ, aes(pct, grp, fill = yr)) + geom_col(position = DG, width = 0.84, orientation = "y") +
  geom_errorbar(aes(xmin = pmax(lo, 0), xmax = hi), position = DG, width = 0.45, linewidth = 0.25, colour = "grey25", orientation = "y") +
  geom_text(aes(x = hi + 1, label = lab), position = DG, hjust = 0, family = FONT, size = 2.1) +   # values for every bar
  facet_grid(. ~ scope) + scale_fill_manual(values = YCOL, breaks = as.character(YRS), name = "Modelled estimate, by year (95% CrI):") +
  scale_x_continuous(limits = c(0, 80), breaks = seq(0, 75, 25), expand = c(0, 0)) +
  labs(x = "Young survivors (%)", y = NULL) + th +
  theme(axis.text.y = element_text(size = 7), panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.25),
        axis.title.x = element_text(size = 7.5))
TOP <- Sys.getenv("TOPROW", "bars")                                              # "dot": dot plot alternative for the top row (written to *_dot.png)
if (TOP == "dot") {
  DT <- dcast(BR, scope + grp ~ year, value.var = c("pct", "lab")); setnames(DT, c("pct_2005", "pct_2025", "lab_2005", "lab_2025"), c("a", "b", "la", "lb"))
  DT[, `:=`(left = pmin(a, b), right = pmax(a, b), la_h = fifelse(a <= b, 1, 0), lb_h = fifelse(a <= b, 0, 1))]
  DL <- ST[year %in% YRS][, `:=`(yr = factor(year, levels = YRS), grp = factor(GL[as.character(group)], levels = rev(GL)))]
  p1 <- ggplot(DT) + geom_segment(aes(x = a, xend = b, y = grp, yend = grp), colour = "grey70", linewidth = 1.1) +
    geom_point(data = DL, aes(pct, grp, fill = yr, size = yr), shape = 21, colour = "white", stroke = 0.3) +
    geom_text(aes(x = a, y = grp, label = la), nudge_y = 0.3, family = FONT, size = 2.1, colour = "grey40") +              # 2005 value above its dot
    geom_text(aes(x = b, y = grp, label = lb), nudge_y = -0.3, family = FONT, size = 2.1, colour = "#2F5D8A", fontface = "bold") +   # 2025 value below
    facet_grid(. ~ scope) + scale_fill_manual(values = setNames(c("#C9D6E3", "#9FB3C8", "#7090B0", "#4A7199", "#2F5D8A"), YRS), name = "Young survivors, by year:") +
    scale_size_manual(values = setNames(c(2.6, 1.7, 1.7, 1.7, 2.6), YRS), name = "Young survivors, by year:") +
    scale_x_continuous(limits = c(-6, 76), breaks = seq(0, 75, 25), expand = c(0, 0)) +
    labs(x = "Young survivors (%)", y = NULL) + th +
    theme(axis.text.y = element_text(size = 7), panel.grid.major.y = element_line(colour = "grey94", linewidth = 0.25), panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.25),
          axis.title.x = element_text(size = 7.5))
}
line_row <- function(o, ylab, ymax, top = FALSE) {
  d <- C[outcome == o]; d[, southern := grepl("^Southern", scope)]; ob <- OB[outcome == o]; ob[, hi := pmin(hi, ymax)]
  pb <- PB[outcome == o]; pb[, pred_hi := pmin(pred_hi, ymax)]
  ggplot() + geom_ribbon(data = pb, aes(year, ymin = pred_lo, ymax = pred_hi, fill = "95% PI, new country")) +
    geom_ribbon(data = d, aes(year, ymin = lo, ymax = hi, fill = "95% CrI")) +
    geom_ribbon(data = d, aes(year, ymin = lo50, ymax = hi50, fill = "50% CrI")) +
    geom_linerange(data = ob, aes(year, ymin = lo, ymax = hi), colour = "grey55", alpha = 0.45, linewidth = 0.25) +
    geom_point(data = ob, aes(year, pct, shape = "Observed survey (95% CI)"), colour = "grey35", size = 0.75, stroke = 0.3) +
    geom_line(data = d, aes(year, pct, linetype = southern, colour = "Estimate (population-weighted)"), linewidth = 0.7) +
    facet_grid(. ~ scope) + xs + scale_y_continuous(limits = c(0, ymax), breaks = if (ymax == 100) seq(0, 100, 25) else seq(0, ymax, 10), expand = c(0, 0)) +
    scale_colour_manual(values = c("Estimate (population-weighted)" = "#0B4F71"), name = NULL) +
    scale_fill_manual(values = c("95% PI, new country" = "#DDF0F7", "95% CrI" = "#93C8DF", "50% CrI" = "#3E8FB5"), name = NULL,
                      breaks = c("50% CrI", "95% CrI", "95% PI, new country")) +
    scale_shape_manual(values = c("Observed survey (95% CI)" = 1), name = NULL) +
    scale_linetype_manual(values = c(`FALSE` = "solid", `TRUE` = "22"), guide = "none") +
    labs(x = if (o == "formal") "Year" else NULL, y = ylab) + th +
    theme(strip.text = element_blank(), legend.position = if (top) "top" else "none") + guides(fill = guide_legend(order = 2, nrow = 1), colour = guide_legend(order = 1), shape = guide_legend(order = 3)) +
    (if (o == "formal") theme(axis.text.x = element_text(angle = 45, hjust = 1)) else theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()))
}
P <- p1 / line_row("sought_any", "Sought any help (%)", 100, top = TRUE) / line_row("formal", "Sought formal help (%)", 30) + plot_layout(heights = c(1.1, 1, 0.8))
ragg::agg_png(file.path(FG, if (TOP == "dot") "fig_trends_popweighted_dot.png" else "fig_trends_popweighted.png"), width = 7.4, height = 7.0, units = "in", res = 300, background = "white"); print(P); invisible(dev.off())
cat(if (TOP == "dot") "fig_trends_popweighted_dot.png" else "fig_trends_popweighted.png", "written\n")
