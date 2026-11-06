# 61k_fig_trends_regional.R - Correction 5, post-hoc addition 4 (e): trends as regional small multiples (layout after Strain et al. 2026).
# Columns: all of sub-Saharan Africa, then each region. Rows: the three cascade steps. Grey lines: each country's partially pooled
# trajectory over its observed years (posterior median; P39 model, 84b). Navy line and band: overall or regional curve (posterior
# median, 95% CrI). Panel text: OR per decade (95% CrI) and Pr(decline). Same model outputs as figures 4 and S-country; presentation only.
suppressMessages({library(data.table); library(ggplot2)})
ROOT <- Sys.getenv("RR"); T <- file.path(ROOT, "outputs/tables/P38_review"); FG <- file.path(ROOT, "outputs/figures/disclosure_v3")
FONT <- "Times New Roman"; NAVY <- "#1B2A6B"
LOGIT <- scales::trans_new("logit_pct", function(x) qlogis(x / 100), function(x) 100 * plogis(x)); BRK <- c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 30, 50, 70, 90)
md <- function(x, d = 2) gsub(".", "·", formatC(x, format = "f", digits = d), fixed = TRUE)
OUT <- c(told = "Told anyone", sought_any = "Sought any help", formal = "Sought formal help")
SC <- c(All = "Sub-Saharan Africa", West = "West", Central = "Central", East = "East", Southern = "Southern")
C <- fread(file.path(T, "P39_curves.csv")); CC <- fread(file.path(T, "P39_country_curves.csv")); S <- fread(file.path(T, "P39_slopes.csv"))
CC <- rbind(CC, copy(CC)[, region := "All"])                                  # every country also appears in the overall column
S[, txt := ifelse(scope == "Southern", "Trend not estimable\n(no repeated surveys)",
                  sprintf("OR/decade %s (%s–%s)\nPr(decline) %s", md(OR_decade), md(OR_lo), md(OR_hi), md(P_decline)))]
YL <- data.table(outcome = rep(names(OUT), each = 2), y = c(12, 97, 6, 93, 0.08, 55))
setnames(CC, "region", "scope")
for (d in list(C, CC, S)) d[, `:=`(outcome = factor(OUT[outcome], levels = OUT), scope = factor(SC[scope], levels = SC))]
YL[, outcome := factor(OUT[outcome], levels = OUT)]
YT <- YL[, .(ytop = max(y)), by = outcome]; S <- merge(S, YT, by = "outcome")
g <- ggplot() +
  geom_blank(data = YL, aes(2015, y)) +
  geom_line(data = CC, aes(year, pct, group = country), colour = "grey60", linewidth = 0.35) +
  geom_ribbon(data = C, aes(year, ymin = lo, ymax = hi), fill = NAVY, alpha = 0.18) +
  geom_line(data = C, aes(year, pct), colour = NAVY, linewidth = 0.9) +
  geom_text(data = S, aes(2004.5, ytop, label = txt), hjust = 0, vjust = 1, family = FONT, size = 2.2, lineheight = 0.9) +
  facet_grid(outcome ~ scope, scales = "free_y", switch = "y") +
  scale_x_continuous(breaks = seq(2005, 2025, 5), limits = c(2004, 2026)) +
  scale_y_continuous(trans = LOGIT, breaks = BRK, labels = function(b) gsub(".", "·", format(b, drop0trailing = TRUE, trim = TRUE), fixed = TRUE)) +
  labs(x = "Year of survey", y = "Young women with violence, % (logit scale)") +
  theme_minimal(base_family = FONT, base_size = 9) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), strip.text = element_text(face = "bold", size = 8.5),
        strip.placement = "outside", axis.text = element_text(size = 7), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.line = element_line(colour = "black", linewidth = 0.3), axis.ticks = element_line(colour = "black", linewidth = 0.3), panel.spacing = unit(0.6, "lines"))
ragg::agg_png(file.path(FG, "fig_trends_regional.png"), width = 7.4, height = 6.2, units = "in", res = 300, background = "white"); print(g); invisible(dev.off())
cat("fig_trends_regional.png written\n")
