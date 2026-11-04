# firth_helper.R - Correction 5: masks fixest::feglm. Flagged (separated) models are refitted with Firth logistic regression.
# Firth (Jeffreys-penalised) weighted logistic regression: Newton with step cap and halving on the penalised log-likelihood.
.firth <- function(f, d, cv, tol = 1e-7, maxit = 500) {
  X <- model.matrix(f, d); y <- d[[all.vars(f)[1]]]; w <- d$.w
  qx <- qr(X); if (qx$rank < ncol(X)) X <- X[, qx$pivot[seq_len(qx$rank)], drop = FALSE]
  pl <- function(b) { eta <- pmin(pmax(drop(X %*% b), -30), 30); p <- plogis(eta); I <- crossprod(X * sqrt(w * p * (1 - p))); ld <- determinant(I)$modulus; sum(w * (y * eta - log1p(exp(eta)))) + 0.5 * ld }
  b <- rep(0, ncol(X)); b[colnames(X) == "(Intercept)"] <- qlogis(weighted.mean(y, w)); ok <- FALSE; it <- 0
  for (it in seq_len(maxit)) {
    eta <- pmin(pmax(drop(X %*% b), -30), 30); p <- plogis(eta); W <- w * p * (1 - p); Ii <- solve(crossprod(X * sqrt(W)))
    h <- W * rowSums((X %*% Ii) * X); U <- drop(crossprod(X, w * (y - p) + h * (0.5 - p))); st <- drop(Ii %*% U)
    if (max(abs(st)) > 2) st <- st * 2 / max(abs(st))
    l0 <- pl(b); a <- 1; while (a > 1e-4 && !isTRUE(pl(b + a * st) >= l0 - 1e-10)) a <- a / 2
    b <- b + a * st; if (max(abs(a * st)) < tol || abs(pl(b) - l0) < 1e-9) { ok <- TRUE; break } }
  if (!ok) stop("Firth did not converge")
  names(b) <- colnames(X); eta <- drop(X %*% b); p <- plogis(eta); W <- w * p * (1 - p); Vm <- solve(crossprod(X * sqrt(W))); dimnames(Vm) <- list(colnames(X), colnames(X))
  sc <- X * (w * (y - p)); g <- rowsum(sc, d[[cv]], reorder = FALSE); G <- nrow(g); Vr <- Vm %*% crossprod(g) %*% Vm * (G / (G - 1))
  list(b = b, Vm = Vm, sm = sqrt(diag(Vm)), sr = sqrt(diag(Vr)), iter = it) }
.flog <- function(...) cat(paste(..., sep = "\t"), "\n", file = file.path(Sys.getenv("RR"), "outputs/firth_flagged.tsv"), append = TRUE, sep = "")
feglm <- function(fml, data, family, weights, cluster, notes = FALSE, warn = FALSE, ...) {
  m <- fixest::feglm(fml, data, family = family, weights = weights, cluster = cluster, notes = notes, warn = warn, ...)
  b <- coef(m); s <- fixest::se(m); conv <- isTRUE(m$convStatus)
  if (conv && all(abs(b[names(b) != "(Intercept)"]) <= 5, na.rm = TRUE) && all(is.finite(s)) && all(s < 5)) return(m)
  txt <- paste(deparse(fml, width.cutoff = 500L), collapse = " "); parts <- strsplit(txt, "\\|")[[1]]; rhsfe <- if (length(parts) > 1) trimws(parts[2]) else NULL
  wv <- all.vars(weights); cv <- all.vars(cluster); base <- as.formula(trimws(parts[1]))
  d <- as.data.frame(data)[, unique(c(all.vars(fml), wv, cv)), drop = FALSE]; d <- d[complete.cases(d), , drop = FALSE]
  d$.w <- d[[wv]] / mean(d[[wv]]); hasfe <- !is.null(rhsfe) && length(unique(d[[rhsfe]])) > 1
  f2 <- if (hasfe) update(base, as.formula(paste(". ~ . + factor(", rhsfe, ")"))) else base
  r <- tryCatch(.firth(f2, d, cv), error = function(e) e)
  if (inherits(r, "error")) { .flog("FAILED", txt, conditionMessage(r)); stop(r) }
  keep <- names(r$b); sn <- pmax(r$sm, r$sr, na.rm = TRUE); V <- diag(sn / r$sm, length(sn)) %*% r$Vm %*% diag(sn / r$sm, length(sn)); dimnames(V) <- dimnames(r$Vm)
  out <- if (!is.null(rhsfe)) setdiff(keep, c("(Intercept)", grep("^factor\\(", keep, value = TRUE))) else keep
  .flog("REFIT", all.vars(base)[1], nrow(d), if (length(unique(data[["country"]])) == 1) data[["country"]][1] else "", paste(names(b)[abs(b) > 5], collapse = ","), r$iter, paste(round(r$b[out], 2), collapse = ","))
  coefr <- r$b
  structure(list(coefficients = coefr[out], sev = sn[out], V = V[out, out, drop = FALSE], n = nrow(d)), class = "firthfit") }
coef.firthfit <- function(object, ...) object$coefficients
vcov.firthfit <- function(object, ...) object$V
nobs.firthfit <- function(object, ...) object$n
confint.firthfit <- function(object, ...) cbind(object$coefficients - 1.96 * object$sev, object$coefficients + 1.96 * object$sev)
se <- function(x, ...) if (inherits(x, "firthfit")) x$sev else fixest::se(x, ...)
