suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(ggplot2)
  library(plotly)
  library(minpack.lm)
  library(boot)
  library(bbmle)
  library(MASS)
  library(bslib)
  library(gridExtra)
})

APP_COLORS <- c(
  primary = "#3498db",
  danger = "#e74c3c",
  dark = "#2c3e50",
  warning = "#e67e22"
)

theme_set(theme_minimal(base_size = 13))
options(shiny.maxRequestSize = 20 * 1024^2)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    y
  } else {
    x
  }
}

safe_run <- function(expr, fallback = NULL, context = "") {
  tryCatch(
    expr,
    error = function(e) {
      if (nzchar(context)) {
        message(sprintf("[%s] %s", context, e$message))
      } else {
        message(e$message)
      }
      fallback
    }
  )
}

parse_named_numeric <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) {
    return(numeric(0))
  }

  tokens <- trimws(unlist(strsplit(text, ",")))
  out <- numeric(0)

  for (tok in tokens) {
    if (!nzchar(tok)) {
      next
    }
    kv <- trimws(unlist(strsplit(tok, "=", fixed = TRUE)))
    if (length(kv) != 2) {
      next
    }
    nm <- kv[1]
    val <- suppressWarnings(as.numeric(kv[2]))
    if (nzchar(nm) && is.finite(val)) {
      out[nm] <- val
    }
  }

  out
}

make_builtin_data <- function(kind = "lineaire", n = 50, seed = 123, noise_sd = 1) {
  set.seed(seed)

  if (identical(kind, "puromycin")) {
    df <- data.frame(
      x = as.numeric(Puromycin$conc),
      y = as.numeric(Puromycin$rate),
      z = as.numeric(Puromycin$state == "treated"),
      state = as.character(Puromycin$state),
      stringsAsFactors = FALSE
    )
    df$dataset <- "Puromycin"
    return(df)
  }

  x <- seq(0.2, 10, length.out = n)
  z <- rnorm(n, mean = 0, sd = 1)

  if (identical(kind, "exponentielle")) {
    y <- 2.5 * exp(0.35 * x) + rnorm(n, sd = noise_sd)
    dataset <- "Exponentielle"
  } else if (identical(kind, "polynomiale")) {
    y <- 1.2 - 0.8 * x + 0.35 * x^2 - 0.02 * x^3 + rnorm(n, sd = noise_sd)
    dataset <- "Polynomiale"
  } else {
    y <- 2.0 + 1.8 * x + 0.7 * z + rnorm(n, sd = noise_sd)
    dataset <- "Lineaire"
  }

  data.frame(
    x = x,
    y = as.numeric(y),
    z = z,
    dataset = dataset,
    stringsAsFactors = FALSE
  )
}

clean_xy_data <- function(df, x_col, y_col) {
  if (is.null(df) || !is.data.frame(df)) {
    stop("Donnees invalides.")
  }
  if (!(x_col %in% names(df)) || !(y_col %in% names(df))) {
    stop("Variables x/y introuvables dans les donnees.")
  }

  df[[x_col]] <- suppressWarnings(as.numeric(df[[x_col]]))
  df[[y_col]] <- suppressWarnings(as.numeric(df[[y_col]]))

  keep <- is.finite(df[[x_col]]) & is.finite(df[[y_col]])
  out <- df[keep, , drop = FALSE]

  if (nrow(out) < 3) {
    stop("Au moins 3 observations valides sont requises.")
  }

  out
}

extract_coef_table <- function(fit_obj) {
  co <- safe_run(summary(fit_obj)$coefficients, fallback = NULL, context = "coef table")
  if (is.null(co)) {
    co <- matrix(
      coef(fit_obj),
      ncol = 1,
      dimnames = list(names(coef(fit_obj)), "Estimate")
    )
  }
  if (is.null(dim(co))) {
    co <- matrix(co, ncol = 1, dimnames = list(names(co), "Estimate"))
  }

  out <- data.frame(
    parametre = rownames(co),
    as.data.frame(co, check.names = FALSE),
    row.names = NULL,
    check.names = FALSE
  )
  out
}

metrics_to_df <- function(metrics) {
  if (is.null(metrics)) {
    return(data.frame())
  }

  data.frame(
    metrique = c("R2", "R2a", "RSS", "PRESS", "R2_pred", "n", "p"),
    valeur = c(
      metrics$R2,
      metrics$R2a,
      metrics$RSS,
      metrics$PRESS,
      metrics$R2_pred,
      metrics$n,
      metrics$p
    ),
    row.names = NULL,
    check.names = FALSE
  )
}

compute_metrics <- function(model, df, y_col) {
  y <- as.numeric(df[[y_col]])
  yhat <- safe_run(as.numeric(predict(model, newdata = df)), fallback = rep(NA_real_, nrow(df)), context = "predict metrics")

  ok <- is.finite(y) & is.finite(yhat)
  y <- y[ok]
  yhat <- yhat[ok]

  n <- length(y)
  p <- length(coef(model))

  if (n == 0) {
    return(list(
      n = 0,
      p = p,
      R2 = NA_real_,
      R2a = NA_real_,
      RSS = NA_real_,
      PRESS = NA_real_,
      R2_pred = NA_real_
    ))
  }

  rss <- sum((y - yhat)^2, na.rm = TRUE)
  tss <- sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
  r2 <- if (isTRUE(tss > 0)) 1 - rss / tss else NA_real_
  r2a <- if (is.finite(r2) && (n > (p + 1))) 1 - (1 - r2) * (n - 1) / (n - p - 1) else NA_real_

  press <- NA_real_
  if (inherits(model, "lm")) {
    h <- safe_run(hatvalues(model), fallback = NULL, context = "hatvalues")
    e <- residuals(model)
    if (!is.null(h) && length(h) == length(e)) {
      denom <- pmax(1 - h, 1e-8)
      press <- sum((e / denom)^2, na.rm = TRUE)
    }
  } else {
    press <- rss
  }

  r2_pred <- if (is.finite(press) && isTRUE(tss > 0)) 1 - press / tss else NA_real_

  list(
    n = n,
    p = p,
    R2 = r2,
    R2a = r2a,
    RSS = rss,
    PRESS = press,
    R2_pred = r2_pred
  )
}

make_model_object <- function(type, family, fit_obj, formula, data, pred_fun, sigma_hat,
                              metrics, coef_table, vcov, corr, meta = list()) {
  list(
    type = type,
    family = family,
    fit_obj = fit_obj,
    formula = formula,
    data = data,
    pred_fun = pred_fun,
    sigma_hat = sigma_hat,
    metrics = metrics,
    coef_table = coef_table,
    vcov = vcov,
    corr = corr,
    meta = meta
  )
}

fit_lp <- function(df, form, y_col, x_var = NULL, algo = "lm") {
  fit <- lm(formula = form, data = df)

  vc <- safe_run(vcov(fit), fallback = NULL, context = "vcov LP")
  corr <- safe_run(cov2cor(vc), fallback = NULL, context = "corr LP")
  metrics <- compute_metrics(fit, df, y_col)
  coef_table <- extract_coef_table(fit)
  sigma_hat <- safe_run(sigma(fit), fallback = sqrt(mean(residuals(fit)^2, na.rm = TRUE)), context = "sigma LP")

  pred_fun <- function(newdata, params = NULL) {
    mm <- model.matrix(delete.response(terms(fit)), data = newdata)
    beta <- coef(fit)

    if (!is.null(params) && length(params) > 0) {
      common <- intersect(names(beta), names(params))
      beta[common] <- params[common]
    }

    beta_full <- rep(0, ncol(mm))
    names(beta_full) <- colnames(mm)

    common_full <- intersect(names(beta_full), names(beta))
    beta_full[common_full] <- beta[common_full]

    as.numeric(mm %*% beta_full)
  }

  make_model_object(
    type = "LP",
    family = "linear",
    fit_obj = fit,
    formula = formula(fit),
    data = df,
    pred_fun = pred_fun,
    sigma_hat = sigma_hat,
    metrics = metrics,
    coef_table = coef_table,
    vcov = vc,
    corr = corr,
    meta = list(x_var = x_var, y_var = y_col, algo = algo)
  )
}

predict_nls_with_params <- function(model, newdata, params) {
  form <- formula(model)
  rhs <- form[[3]]
  form_env <- environment(form)

  out <- numeric(nrow(newdata))

  for (i in seq_len(nrow(newdata))) {
    eval_env <- list2env(as.list(newdata[i, , drop = FALSE]), parent = form_env)
    for (nm in names(params)) {
      assign(nm, params[[nm]], envir = eval_env)
    }
    out[i] <- safe_run(eval(rhs, envir = eval_env), fallback = NA_real_, context = "pred nls")
  }

  as.numeric(out)
}

numerical_gradient <- function(fun, theta, eps = 1e-6) {
  g <- numeric(length(theta))

  for (i in seq_along(theta)) {
    step <- rep(0, length(theta))
    h <- eps * (abs(theta[i]) + 1)
    step[i] <- h

    f_plus <- fun(theta + step)
    f_minus <- fun(theta - step)

    if (!is.finite(f_plus) || !is.finite(f_minus)) {
      g[i] <- NA_real_
    } else {
      g[i] <- (f_plus - f_minus) / (2 * h)
    }
  }

  g
}

calc_ic_nls <- function(model, newdata, level = 0.95) {
  theta <- coef(model)
  pnames <- names(theta)
  fit <- predict_nls_with_params(model, newdata, theta)

  vc <- safe_run(vcov(model), fallback = NULL, context = "vcov nls ic")
  z <- qnorm(1 - (1 - level) / 2)
  se_mean <- rep(NA_real_, length(fit))

  if (!is.null(vc) && nrow(vc) == length(theta)) {
    for (i in seq_len(nrow(newdata))) {
      row_data <- newdata[i, , drop = FALSE]

      grad <- safe_run(
        numerical_gradient(
          fun = function(par_vec) {
            par_named <- setNames(par_vec, pnames)
            predict_nls_with_params(model, row_data, par_named)[1]
          },
          theta = unname(theta)
        ),
        fallback = rep(NA_real_, length(theta)),
        context = "grad nls ic"
      )

      if (all(is.finite(grad))) {
        g <- matrix(grad, ncol = 1)
        var_mu <- safe_run(as.numeric(t(g) %*% vc %*% g), fallback = NA_real_, context = "var nls ic")
        if (is.finite(var_mu)) {
          se_mean[i] <- sqrt(max(var_mu, 0))
        }
      }
    }
  }

  data.frame(
    fit = fit,
    se_mean = se_mean,
    lwr = fit - z * se_mean,
    upr = fit + z * se_mean,
    row.names = NULL
  )
}

calc_ip_nls <- function(model, newdata, level = 0.95) {
  ic <- calc_ic_nls(model, newdata, level = level)

  sigma2 <- safe_run(
    {
      rss <- sum(residuals(model)^2, na.rm = TRUE)
      dfres <- max(df.residual(model), 1)
      rss / dfres
    },
    fallback = NA_real_,
    context = "sigma2 nls ip"
  )

  se_pred <- if (is.finite(sigma2)) {
    sqrt(pmax(ic$se_mean^2 + sigma2, 0))
  } else {
    rep(NA_real_, nrow(ic))
  }

  z <- qnorm(1 - (1 - level) / 2)

  data.frame(
    fit = ic$fit,
    se_pred = se_pred,
    lwr = ic$fit - z * se_pred,
    upr = ic$fit + z * se_pred,
    row.names = NULL
  )
}

fit_nlp <- function(df, formula_nls, start, algo = "nlsLM", y_col, x_var = NULL) {
  start <- as.list(start)

  fit <- if (identical(algo, "nls")) {
    nls(
      formula = formula_nls,
      data = df,
      start = start,
      control = nls.control(maxiter = 500, warnOnly = TRUE, minFactor = 1e-8)
    )
  } else {
    minpack.lm::nlsLM(
      formula = formula_nls,
      data = df,
      start = start,
      control = minpack.lm::nls.lm.control(maxiter = 500)
    )
  }

  vc <- safe_run(vcov(fit), fallback = NULL, context = "vcov NLP")
  corr <- safe_run(cov2cor(vc), fallback = NULL, context = "corr NLP")
  metrics <- compute_metrics(fit, df, y_col)
  coef_table <- extract_coef_table(fit)

  sigma_hat <- safe_run(
    {
      rss <- sum(residuals(fit)^2, na.rm = TRUE)
      dfres <- max(df.residual(fit), 1)
      sqrt(rss / dfres)
    },
    fallback = sqrt(mean(residuals(fit)^2, na.rm = TRUE)),
    context = "sigma NLP"
  )

  pred_fun <- function(newdata, params = NULL) {
    params_use <- coef(fit)
    if (!is.null(params) && length(params) > 0) {
      common <- intersect(names(params_use), names(params))
      params_use[common] <- params[common]
    }
    predict_nls_with_params(fit, newdata, params_use)
  }

  make_model_object(
    type = "NLP",
    family = "nonlinear",
    fit_obj = fit,
    formula = formula(fit),
    data = df,
    pred_fun = pred_fun,
    sigma_hat = sigma_hat,
    metrics = metrics,
    coef_table = coef_table,
    vcov = vc,
    corr = corr,
    meta = list(x_var = x_var, y_var = y_col, algo = algo)
  )
}

analytic_ci_from_vcov <- function(model, level = 0.95) {
  cf <- coef(model)
  vc <- safe_run(vcov(model), fallback = NULL, context = "vcov analytic ci")
  z <- qnorm(1 - (1 - level) / 2)

  if (is.null(vc)) {
    return(data.frame(
      parametre = names(cf),
      lwr = NA_real_,
      upr = NA_real_,
      row.names = NULL
    ))
  }

  se <- sqrt(pmax(diag(vc), 0))
  data.frame(
    parametre = names(cf),
    lwr = as.numeric(cf - z * se),
    upr = as.numeric(cf + z * se),
    row.names = NULL
  )
}

bootstrap_ci_percentile <- function(samples, level = 0.95) {
  if (is.null(samples) || nrow(samples) == 0) {
    return(data.frame())
  }

  alpha <- (1 - level) / 2
  qs <- apply(
    samples,
    2,
    function(v) {
      quantile(v, probs = c(alpha, 1 - alpha), na.rm = TRUE, names = FALSE)
    }
  )

  if (is.vector(qs)) {
    qs <- matrix(qs, nrow = 2)
    colnames(qs) <- colnames(samples)
  }

  data.frame(
    parametre = colnames(samples),
    lwr = as.numeric(qs[1, ]),
    upr = as.numeric(qs[2, ]),
    row.names = NULL
  )
}

bootstrap_lp <- function(model_obj, R = 1000, type = "nonparam", level = 0.95) {
  fit <- model_obj$fit_obj
  df <- model_obj$data
  y_col <- model_obj$meta$y_var

  y <- as.numeric(df[[y_col]])
  fhat <- as.numeric(fitted(fit))
  e <- as.numeric(residuals(fit))
  e_centered <- e - mean(e, na.rm = TRUE)

  coef_names <- names(coef(fit))
  samples <- matrix(NA_real_, nrow = R, ncol = length(coef_names))
  colnames(samples) <- coef_names

  for (i in seq_len(R)) {
    y_star <- if (identical(type, "param")) {
      fhat + rnorm(length(fhat), mean = 0, sd = model_obj$sigma_hat %||% sd(e, na.rm = TRUE))
    } else {
      fhat + sample(e_centered, size = length(e_centered), replace = TRUE)
    }

    df_star <- df
    df_star[[y_col]] <- y_star

    refit <- safe_run(lm(formula(fit), data = df_star), fallback = NULL, context = "bootstrap LP refit")
    if (!is.null(refit)) {
      cf <- coef(refit)
      samples[i, names(cf)] <- cf
    }
  }

  ok <- complete.cases(samples)
  samples_ok <- samples[ok, , drop = FALSE]

  ci_boot <- bootstrap_ci_percentile(samples_ok, level = level)
  ci_analytic <- safe_run(
    {
      ci <- confint(fit, level = level)
      data.frame(
        parametre = rownames(ci),
        lwr = ci[, 1],
        upr = ci[, 2],
        row.names = NULL
      )
    },
    fallback = analytic_ci_from_vcov(fit, level = level),
    context = "analytic ci LP"
  )

  list(
    samples = samples_ok,
    ci_boot = ci_boot,
    ci_analytic = ci_analytic,
    success_rate = mean(ok),
    type = type,
    R = R
  )
}

bootstrap_nlp <- function(model_obj, R = 1000, type = "nonparam", level = 0.95) {
  fit <- model_obj$fit_obj
  df <- model_obj$data
  y_col <- model_obj$meta$y_var
  algo <- model_obj$meta$algo %||% "nlsLM"

  y <- as.numeric(df[[y_col]])
  fhat <- as.numeric(predict(fit, newdata = df))
  e <- as.numeric(y - fhat)
  e_centered <- e - mean(e, na.rm = TRUE)

  coef_names <- names(coef(fit))
  samples <- matrix(NA_real_, nrow = R, ncol = length(coef_names))
  colnames(samples) <- coef_names

  start_base <- as.list(coef(fit))

  for (i in seq_len(R)) {
    y_star <- if (identical(type, "param")) {
      fhat + rnorm(length(fhat), mean = 0, sd = model_obj$sigma_hat %||% sd(e, na.rm = TRUE))
    } else {
      fhat + sample(e_centered, size = length(e_centered), replace = TRUE)
    }

    df_star <- df
    df_star[[y_col]] <- y_star

    refit <- if (identical(algo, "nls")) {
      safe_run(
        nls(
          formula = formula(fit),
          data = df_star,
          start = start_base,
          control = nls.control(maxiter = 500, warnOnly = TRUE, minFactor = 1e-8)
        ),
        fallback = NULL,
        context = "bootstrap NLP refit nls"
      )
    } else {
      safe_run(
        minpack.lm::nlsLM(
          formula = formula(fit),
          data = df_star,
          start = start_base,
          control = minpack.lm::nls.lm.control(maxiter = 500)
        ),
        fallback = NULL,
        context = "bootstrap NLP refit nlsLM"
      )
    }

    if (!is.null(refit)) {
      cf <- coef(refit)
      samples[i, names(cf)] <- cf
    }
  }

  ok <- complete.cases(samples)
  samples_ok <- samples[ok, , drop = FALSE]

  ci_boot <- bootstrap_ci_percentile(samples_ok, level = level)
  ci_analytic <- analytic_ci_from_vcov(fit, level = level)

  list(
    samples = samples_ok,
    ci_boot = ci_boot,
    ci_analytic = ci_analytic,
    success_rate = mean(ok),
    type = type,
    R = R
  )
}

mle_fit_gaussian <- function(df, pred_fun, start, method = "BFGS", lower = NULL, upper = NULL, y_col = "y") {
  if (is.null(names(start)) || any(!nzchar(names(start)))) {
    stop("Les parametres initiaux doivent etre nommes.")
  }

  if (!("sigma" %in% names(start))) {
    start <- c(start, sigma = sd(df[[y_col]], na.rm = TRUE))
  }
  start["sigma"] <- max(start["sigma"], 1e-6)

  nll_fn <- function(par_vec) {
    if (is.null(names(par_vec))) {
      names(par_vec) <- names(start)
    }

    par <- par_vec[names(start)]
    sigma <- par[["sigma"]]

    if (!is.finite(sigma) || sigma <= 0) {
      return(1e12)
    }

    theta <- par[setdiff(names(par), "sigma")]
    mu <- safe_run(as.numeric(pred_fun(df, theta)), fallback = rep(NA_real_, nrow(df)), context = "pred MLE")
    y <- as.numeric(df[[y_col]])

    if (length(mu) != length(y) || any(!is.finite(mu)) || any(!is.finite(y))) {
      return(1e12)
    }

    -sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))
  }

  opt_args <- list(
    par = start,
    fn = nll_fn,
    method = method,
    hessian = TRUE,
    control = list(maxit = 5000)
  )

  if (identical(method, "L-BFGS-B")) {
    if (is.null(lower)) {
      lower <- rep(-Inf, length(start))
      names(lower) <- names(start)
    }
    if (is.null(upper)) {
      upper <- rep(Inf, length(start))
      names(upper) <- names(start)
    }

    lower <- lower[names(start)]
    upper <- upper[names(start)]
    if (!is.finite(lower["sigma"]) || lower["sigma"] <= 0) {
      lower["sigma"] <- 1e-8
    }

    opt_args$lower <- lower
    opt_args$upper <- upper
  }

  opt <- do.call(optim, opt_args)
  par_hat <- opt$par
  names(par_hat) <- names(start)

  loglik <- -opt$value
  k <- length(par_hat)
  n <- nrow(df)
  aic <- 2 * k - 2 * loglik
  bic <- log(max(n, 1)) * k - 2 * loglik

  hess <- opt$hessian
  vc <- safe_run(
    solve(hess),
    fallback = safe_run(MASS::ginv(hess), fallback = NULL, context = "ginv Hessian"),
    context = "solve Hessian"
  )

  list(
    optim = opt,
    par = par_hat,
    logLik = loglik,
    AIC = aic,
    BIC = bic,
    vcov = vc,
    nll_fn = nll_fn,
    method = method,
    y_col = y_col,
    df = df,
    pred_fun = pred_fun,
    convergence = opt$convergence,
    message = opt$message
  )
}

profile_loglik_1d <- function(fit, param_name, grid) {
  if (!(param_name %in% names(fit$par))) {
    stop("Parametre introuvable pour le profil de log-vraisemblance.")
  }

  base_par <- fit$par
  nll <- fit$nll_fn
  others <- setdiff(names(base_par), param_name)

  loglik_vals <- vapply(
    grid,
    FUN.VALUE = numeric(1),
    FUN = function(v) {
      if (length(others) == 0) {
        p <- base_par
        p[param_name] <- v
        return(-nll(p))
      }

      obj <- function(p_other) {
        p <- base_par
        names(p_other) <- others
        p[others] <- p_other
        p[param_name] <- v
        nll(p)
      }

      opt <- safe_run(
        optim(par = base_par[others], fn = obj, method = "BFGS"),
        fallback = NULL,
        context = "profil optim"
      )

      if (is.null(opt)) {
        return(NA_real_)
      }

      p <- base_par
      p[others] <- opt$par
      p[param_name] <- v
      -nll(p)
    }
  )

  data.frame(
    param_value = grid,
    logLik = as.numeric(loglik_vals),
    row.names = NULL
  )
}
