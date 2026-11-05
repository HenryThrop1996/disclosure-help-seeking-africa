# 84e_trend_splines.R - post-hoc sensitivity (2026-11-05): non-linear trends with splines.
# The P39 survey-level meta-regression (young women 15-24), with the overall linear time term replaced by a spline of time.
#   y_cs ~ N(f(t_cs) + a_c + b_c t_cs, se_cs^2 + sigma_e^2); country and region random intercepts and linear slopes as in P39.
#   f(t): natural cubic spline, 3 df (primary; 2 and 4 as sensitivity) with interior knots at quantiles of survey dates, or a penalised smooth s(t).
# Outputs (outputs/tables/P38_review/): P39_spline_curves.csv, P39_spline_summary.csv, P39_spline_lrt.csv
suppressMessages({library(data.table); library(brms); library(splines); library(metafor); library(posterior); library(loo)})
ROOT <- Sys.getenv("RR"); T <- file.path(ROOT, "outputs/tables/P38_review"); MOD <- file.path(ROOT, "outputs/models_splines")
options(brms.backend = "cmdstanr", mc.cores = 4)
E <- fread(file.path(T, "P39_survey_estimates.csv"))                               # one row per survey and outcome (logit %, SE, time)
grid <- data.table(t = seq((2004 - 2015) / 10, (2025.6 - 2015) / 10, length.out = 60))   # prediction grid, 2004-2025.6
basis <- function(x, df, ref) {                                                    # natural spline basis with knots fixed from the data
  kn <- if (df > 1) quantile(ref, probs = seq(0, 1, length.out = df + 1)[-c(1, df + 1)]) else NULL   # interior knots at quantiles of survey dates
  b <- ns(x, knots = kn, Boundary.knots = range(ref)); colnames(b) <- paste0("ns", seq_len(ncol(b))); b }
run <- function(o) {
  d <- E[outcome == o]; a0 <- qlogis(if (o == "formal") 0.03 else 0.3)
  pri <- c(set_prior(sprintf("normal(%f, 1.5)", a0), class = "Intercept"), prior(normal(0, 1), class = "b"),
           prior(normal(0, 1), class = "sd", coef = "Intercept", group = "region"), prior(normal(0, 0.5), class = "sd", coef = "t", group = "region"),
           prior(normal(0, 1), class = "sd", coef = "Intercept", group = "country"), prior(normal(0, 0.5), class = "sd", coef = "t", group = "country"),
           prior(normal(0, 0.5), class = "sigma"))
  RE <- "(1 + t || region) + (1 + t || country)"
  fitm <- function(rhs, data, name, extra = NULL) brm(as.formula(paste("logit | se(se, sigma = TRUE) ~", rhs, "+", RE)), data = data,
                                                      prior = c(pri, extra), chains = 4, iter = 4000, warmup = 2000, seed = 20261023,
                                                      control = list(adapt_delta = 0.995, max_treedepth = 12), refresh = 0, file = file.path(MOD, paste0(o, "_", name)))
  fits <- list(); curves <- list()
  fits$linear <- fitm("t", d, "linear")
  ng <- copy(grid)[, se := 0]
  for (df in 2:4) {                                                                # natural splines with 2, 3, 4 degrees of freedom
    dd <- cbind(d, basis(d$t, df, d$t)); gg <- cbind(ng, basis(ng$t, df, d$t))     # same knots for data and grid
    nm <- paste0("ns", df); fits[[nm]] <- fitm(paste(paste0("ns", 1:df), collapse = " + "), dd, nm)
    curves[[nm]] <- posterior_epred(fits[[nm]], newdata = gg, re_formula = NA) }
  fits$smooth <- fitm("s(t, k = 5)", d, "smooth", prior(normal(0, 1), class = "sds"))    # penalised thin-plate smooth: data choose the wiggliness
  curves$smooth <- posterior_epred(fits$smooth, newdata = ng, re_formula = NA)
  curves$linear <- posterior_epred(fits$linear, newdata = ng, re_formula = NA)
  # summaries: curve bands, period slopes, LOO
  cur <- rbindlist(lapply(names(curves), function(k) { m <- curves[[k]]
    data.table(outcome = o, model = k, year = 2015 + 10 * grid$t, pct = 100 * plogis(apply(m, 2, median)),
               lo = 100 * plogis(apply(m, 2, quantile, .025)), hi = 100 * plogis(apply(m, 2, quantile, .975))) }))
  at <- function(m, yr) { j <- which.min(abs(2015 + 10 * grid$t - yr)); m[, j] }    # draws of the mean logit at a given year
  per <- function(m, y0, y1) { s <- (at(m, y1) - at(m, y0)) / ((y1 - y0) / 10); exp(quantile(s, c(.5, .025, .975))) }   # average OR per decade over a period
  L <- lapply(fits, function(f) loo(f, moment_match = FALSE)); cmp <- loo_compare(L)
  summ <- rbindlist(lapply(names(curves), function(k) { m <- curves[[k]]; p1 <- per(m, 2005, 2014); p2 <- per(m, 2015, 2025); pt <- per(m, 2005, 2025)
    ch <- 100 * (plogis(at(m, 2025)) - plogis(at(m, 2005)))
    data.table(outcome = o, model = k, OR_2005_14 = p1[1], lo_2005_14 = p1[2], hi_2005_14 = p1[3], OR_2015_25 = p2[1], lo_2015_25 = p2[2], hi_2015_25 = p2[3],
               OR_2005_25 = pt[1], change_pp = median(ch), change_lo = quantile(ch, .025), change_hi = quantile(ch, .975),
               elpd_diff_vs_best = cmp[k, "elpd_diff"], se_diff = cmp[k, "se_diff"], elpd_loo = L[[k]]$estimates["elpd_loo", "Estimate"],
               rhat_max = max(rhat(fits[[k]]), na.rm = TRUE), divergences = sum(nuts_params(fits[[k]])[nuts_params(fits[[k]])$Parameter == "divergent__", "Value"])) }))
  # frequentist likelihood-ratio test: 3-df natural spline vs linear (ML, random intercepts and slopes by country)
  dd <- cbind(d, basis(d$t, 3, d$t))
  m1 <- rma.mv(logit, se^2, mods = ~ t, random = list(~ t | country, ~ 1 | survey), struct = "GEN", data = dd, method = "ML")
  m3 <- rma.mv(logit, se^2, mods = ~ ns1 + ns2 + ns3, random = list(~ t | country, ~ 1 | survey), struct = "GEN", data = dd, method = "ML")
  a <- anova(m3, m1)
  list(cur = cur, summ = summ, lrt = data.table(outcome = o, LRT = a$LRT, df = a$parms.f - a$parms.r, p = a$pval)) }
R <- lapply(c("told", "sought_any", "formal"), run)
fwrite(rbindlist(lapply(R, `[[`, "cur")), file.path(T, "P39_spline_curves.csv"))
S <- rbindlist(lapply(R, `[[`, "summ")); fwrite(S, file.path(T, "P39_spline_summary.csv"))
LR <- rbindlist(lapply(R, `[[`, "lrt")); fwrite(LR, file.path(T, "P39_spline_lrt.csv"))
print(S[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)]); print(LR)
