mars.control <- function(Mmax = 10, d = 3, trace = FALSE) {
  out <- list(Mmax = as.integer(Mmax), d = d, trace = trace)
  class(out) <- "mars.control"
  out
}

make_basis <- function(x, funcs) {
  x <- as.matrix(x)
  if (is.null(funcs)) {
    return(rep(1, nrow(x)))
  }
  funcs <- as.matrix(funcs)
  b <- rep(1, nrow(x))
  for (i in seq_len(nrow(funcs))) {
    s <- funcs[i, "s"]
    v <- funcs[i, "v"]
    t <- funcs[i, "t"]
    b <- b * h(s, x[, v], t)
  }
  unname(b)
}

basis_matrix <- function(x, Bfuncs) {
  out <- lapply(Bfuncs, function(f) make_basis(x, f))
  out <- as.data.frame(out, optional = TRUE)
  names(out) <- paste0("B", seq_along(Bfuncs) - 1)
  out
}

LOF <- function(formula, data, control) {
  fit <- lm(formula, data = data)
  rss <- sum(stats::residuals(fit)^2)
  M <- ncol(data) - 1L
  C_M <- M + control$d * (M - 1L) / 2
  rss / nrow(data) / (1 - C_M / nrow(data))^2
}

fwd_stepwise <- function(y, x, control = mars.control()) {
  x <- as.matrix(x)
  y <- y

  Bfuncs <- list(NULL)
  B <- data.frame(B0 = rep(1, nrow(x)))

  for (step in seq_len(control$Mmax / 2)) {
    best <- list(score = Inf)

    for (m in seq_along(Bfuncs)) {
      curf <- Bfuncs[[m]]
      used_vars <- if (is.null(curf)) integer() else curf[, "v"]
      cand_vars <- setdiff(seq_len(ncol(x)), used_vars)
      baseb <- B[[m]]

      for (v in cand_vars) {
        xs <- sort(unique(x[baseb > 0, v]))
        if (length(xs) <= 2) next

        for (t in xs[2:(length(xs) - 1)]) {
          leftf <- rbind(curf, c(s = -1, v = v, t = t))
          rightf <- rbind(curf, c(s = 1, v = v, t = t))
          b1 <- make_basis(x, leftf)
          b2 <- make_basis(x, rightf)
          dat <- data.frame(y = y, B, new1 = b1, new2 = b2)
          rss <- sum(stats::residuals(stats::lm(y ~ . - 1, data = dat))^2)

          if (rss < best$score - 1e-12) {
            best <- list(score = rss, leftf = leftf, rightf = rightf)
          }
        }
      }
    }

    Bfuncs[[length(Bfuncs) + 1L]] <- best$leftf
    Bfuncs[[length(Bfuncs) + 1L]] <- best$rightf
    B[[paste0("B", ncol(B))]] <- make_basis(x, best$leftf)
    B[[paste0("B", ncol(B))]] <- make_basis(x, best$rightf)
  }

  list(y = y, B = B, Bfuncs = Bfuncs)
}

bwd_stepwise <- function(fwd, control) {
  Mmax <- ncol(fwd$B)
  Jstar <- 2:Mmax
  Kstar <- Jstar
  dat <- data.frame(y = fwd$y, fwd$B[, c(1, Jstar), drop = FALSE])
  lofstar <- LOF(y ~ . - 1, dat, control)

  for (M in Mmax:2) {
    b <- Inf
    L <- Kstar
    if (control$trace) cat("L:", L, "\n")

    for (m in L) {
      K <- setdiff(L, m)
      dat <- data.frame(y = fwd$y, fwd$B[, c(1, K), drop = FALSE])
      lof <- LOF(y ~ . - 1, dat, control)
      if (control$trace) cat("M:K:lof", M, ":", K, ":", lof, "\n")
      if (lof < b) {
        b <- lof
        Kstar <- K
      }
      if (lof < lofstar) {
        lofstar <- lof
        Jstar <- K
      }
    }
    if (control$trace) cat("M:Jstar:lofstar", M, ":", Jstar, ":", lofstar, "\n")
  }

  Jstar <- c(1, Jstar)
  list(y = fwd$y, B = fwd$B[, Jstar, drop = FALSE], Bfuncs = fwd$Bfuncs[Jstar])
}

mars <- function(formula, data, control = mars.control()) {
  mf <- model.frame(formula, data)
  y <- model.response(mf)
  x <- model.matrix(delete.response(terms(mf)), mf)
  if ("(Intercept)" %in% colnames(x)) {
    x <- x[, colnames(x) != "(Intercept)", drop = FALSE]
  }

  fwd <- fwd_stepwise(y, x, control)
  bwd <- bwd_stepwise(fwd, control)
  fit <- lm(y ~ . - 1, data = data.frame(y = y, bwd$B))

  out <- list(
    call = match.call(),
    formula = formula,
    y = y,
    B = bwd$B,
    Bfuncs = bwd$Bfuncs,
    x_names = colnames(x),
    coefficients = fit$coefficients,
    residuals = fit$residuals,
    effects = fit$effects,
    rank = fit$rank,
    fitted.values = fit$fitted.values,
    assign = fit$assign,
    qr = fit$qr,
    df.residual = fit$df.residual,
    xlevels = fit$xlevels,
    call = fit$call,
    terms = fit$terms,
    model = fit$model
  )
  class(out) <- c("mars", "lm")
  out
}

predict.mars <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    return(as.numeric(object$fitted.values))
  }
  x <- as.matrix(newdata[, object$x_names, drop = FALSE])
  Bnew <- basis_matrix(x, object$Bfuncs)
  as.numeric(as.matrix(Bnew) %*% object$coefficients)
}

print.mars <- function(x, ...) {
  cat("\nCall:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coefficients)
  invisible(x)
}

summary.mars <- function(object, ...) {
  out <- list(
    call = object$call,
    coefficients = coef(summary.lm(object)),
    sigma = summary.lm(object)$sigma,
    r.squared = summary.lm(object)$r.squared,
    adj.r.squared = summary.lm(object)$adj.r.squared,
    df = c(object$rank, object$df.residual)
  )
  class(out) <- "summary.mars"
  out
}

print.summary.mars <- function(x, ...) {
  cat("\nCall:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coefficients)
  cat("\nResidual standard error:", x$sigma, "\n")
  cat("Multiple R-squared:", x$r.squared,
      ", Adjusted R-squared:", x$adj.r.squared, "\n")
  invisible(x)
}

anova.mars <- function(object, ...) {
  NextMethod("anova")
}

plot.mars <- function(x, ...) {
  op <- par(mfrow = c(1, 2))
  on.exit(par(op))
  plot(x$fitted.values, x$residuals,
       xlab = "Fitted values", ylab = "Residuals",
       main = "Residuals vs Fitted", ...)
  abline(h = 0, lty = 2, col = "grey50")
  qqnorm(x$residuals, main = "Normal Q-Q", ...)
  qqline(x$residuals, col = "grey50")
  invisible(x)
}
