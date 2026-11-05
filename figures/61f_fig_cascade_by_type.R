# 61f_fig_cascade_by_type.R - Pre-registration 42: disclosure cascade by perpetrator and type of violence (told anyone, sought any help,
# sought formal help), three forests sharing one label column, in the style of 61d.
# Input: outputs/tables/P42/P42_cascade_by_type.csv (95_p42_cascade_by_type.R; survey-weighted %, logit 95% CI, PSU clustering).
suppressMessages({library(data.table); library(ggplot2)})
ROOT <- Sys.getenv("RR"); FG <- file.path(ROOT, "outputs/figures/disclosure_v3")
FONT <- "Times New Roman"; NAVY <- "#1B2A6B"; MAROON <- "#9E2A3A"; SQ <- "#B5B5B5"
md <- function(x, d = 1) gsub(".", "·", formatC(x, format = "f", digits = d), fixed = TRUE)
est <- function(p, l, h) sprintf("%s (%s–%s)", md(p), md(l), md(h))
CI <- fread(file.path(ROOT, "outputs/tables/P42/P42_cascade_by_type.csv"))[region == "All"]
OUT <- c("told", "sought_any", "formal"); TITLE <- c("A  Told anyone", "B  Sought any help", "C  Sought formal help")
AX <- list(c(20, 70, 10), c(10, 60, 10), c(0, 8, 2))
LAY <- list(list("By type (types can co-occur)", c("Partner physical", "Partner sexual", "Non-partner physical", "Non-partner sexual")),
            list("One type only", c("Partner physical only", "Partner sexual only", "Non-partner physical only", "Non-partner sexual only")),
            list("Combinations", c("Partner physical and sexual", "Non-partner physical and sexual", "Partner and non-partner")))
rows <- list(); y <- 0
add <- function(...) { y <<- y - 1; rows[[length(rows) + 1]] <<- data.table(y = y, ...) }
for (b in LAY) { add(type = "head", lab = b[[1]], g = NA_character_); for (g in b[[2]]) add(type = "row", lab = g, g = g); y <- y - 0.5 }
add(type = "all", lab = "All young survivors", g = "All young victims")
F <- rbindlist(rows)
F[type == "row", lab := paste0(lab, " (", md(CI[group == g & outcome == "sought_any", share_of_victims], 0), "%)"), by = g]
X0 <- c(24, 51.5, 79); XW <- 11.5; XT <- X0 + XW + 0.6
pos <- function(v, j) X0[j] + XW * (pmin(pmax(v, AX[[j]][1]), AX[[j]][2]) - AX[[j]][1]) / (AX[[j]][2] - AX[[j]][1])
V <- rbindlist(lapply(1:3, function(j) { o <- OUT[j]; v <- copy(F)
  v[type != "head", c("p", "l", "h", "n") := CI[outcome == o][match(g, group), .(pct, lo, hi, n)]]
  v[, `:=`(j = j, xo = pos(p, j), xl = pos(l, j), xh = pos(h, j), cut_l = l < AX[[j]][1], etxt = est(p, l, h))]; v }))
V[type == "head", etxt := NA_character_]; stopifnot(!anyNA(V[type != "head", p]))
ALL <- V[type == "all"]; R_ <- V[type == "row"]
dia <- V[type == "all", .(x = c(xl, xo, xh, xo), y = c(y, y + 0.42, y, y - 0.42)), by = j]
ybot <- min(F$y) - 0.9; top <- 1.3
TK <- rbindlist(lapply(1:3, function(j) data.table(j = j, v = seq(AX[[j]][1], AX[[j]][2], AX[[j]][3]))))[, x := pos(v, j), by = j]
g <- ggplot() +
  annotate("segment", x = 0, xend = 100, y = top - 0.75, yend = top - 0.75, linetype = "longdash", colour = "grey70", linewidth = 0.35) +
  geom_segment(data = ALL, aes(x = xo, xend = xo, y = ybot, yend = top - 0.75), colour = MAROON, linetype = "dashed", linewidth = 0.45) +
  geom_point(data = R_, aes(x = xo, y = y, size = n), shape = 15, colour = SQ) +
  geom_segment(data = R_, aes(x = xl, xend = xh, y = y, yend = y), colour = "black", linewidth = 0.42) +
  geom_point(data = R_, aes(x = xo, y = y), shape = 18, size = 1.3, colour = "black") +
  geom_polygon(data = dia, aes(x = x, y = y, group = j), fill = "white", colour = NAVY, linewidth = 0.5) +
  geom_text(data = F, aes(x = 0, y = y, label = lab, fontface = ifelse(type %in% c("head", "all"), "bold", "plain")), hjust = 0, family = FONT, size = 2.6) +
  geom_text(data = V[type != "head"], aes(x = XT[j], y = y, label = etxt), hjust = 0, family = FONT, size = 2.45) +
  annotate("text", x = 0, y = top, label = "Violence experienced (% of survivors)", hjust = 0, vjust = 0.2, family = FONT, size = 2.9) +
  annotate("text", x = X0 + (XW + 9) / 2, y = top + 1.25, label = TITLE, family = FONT, fontface = "bold", size = 3) +
  annotate("text", x = XT, y = top, label = "% (95% CI)", hjust = 0, vjust = 0.2, family = FONT, size = 2.9) +
  annotate("segment", x = X0, xend = X0 + XW, y = ybot, yend = ybot, linewidth = 0.45) +
  geom_segment(data = TK, aes(x = x, xend = x, y = ybot, yend = ybot - 0.3), linewidth = 0.4) +
  geom_text(data = TK, aes(x = x, y = ybot - 0.75, label = v), family = FONT, size = 2.4) +
  annotate("text", x = X0 + XW / 2, y = ybot - 1.6, label = "% of women (95% CI)", family = FONT, size = 2.8) +
  scale_size_area(max_size = 3.6, guide = "none") +
  coord_cartesian(xlim = c(0, 100), ylim = c(ybot - 2.0, top + 1.9), expand = FALSE, clip = "off") +
  theme_void(base_family = FONT) + theme(plot.margin = margin(4, 6, 4, 4))
ragg::agg_png(file.path(FG, "fig_cascade_by_type.png"), width = 9.2, height = 0.24 * (top + 1.9 - ybot + 2.0) + 0.3, units = "in", res = 300, background = "white")
print(g); invisible(dev.off()); cat("written fig_cascade_by_type.png\n")
