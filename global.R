# ============================================================
# Global.R — Packages, fonctions utilitaires, données intégrées
# Identification de Modèles Paramétriques — Application Shiny
# ============================================================

# --- Packages requis ---
required_packages <- c(
  "shiny", "shinydashboard", "DT", "ggplot2", "plotly",
  "minpack.lm", "boot", "bbmle", "MASS", "bslib", "gridExtra"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cran.r-project.org")
  }
  library(pkg, character.only = TRUE)
}

# --- Jeux de données intégrés ---
data_puromycin <- Puromycin[Puromycin$state == "treated", c("conc", "rate")]
names(data_puromycin) <- c("x", "y")

set.seed(42)
n_synth <- 50
x_synth <- seq(0.5, 10, length.out = n_synth)
data_expo <- data.frame(
  x = x_synth,
  y = 3.5 * exp(-0.4 * x_synth) + rnorm(n_synth, 0, 0.15)
)

data_poly <- data.frame(
  x = x_synth,
  y = 2 + 1.5 * x_synth - 0.3 * x_synth^2 + 0.01 * x_synth^3 + rnorm(n_synth, 0, 1.5)
)

data_lineaire <- data.frame(
  x = x_synth,
  y = 2.5 + 3.2 * x_synth + rnorm(n_synth, 0, 2)
)

datasets_integres <- list(
  "Puromycin (Michaelis-Menten)" = data_puromycin,
  "Exponentielle décroissante" = data_expo,
  "Polynomiale cubique" = data_poly,
  "Linéaire simple" = data_lineaire
)

# --- Modèles NLP prédéfinis ---
modeles_nlp <- list(
  "Exponentiel: a*exp(b*x)" = list(
    formula = y ~ a * exp(b * x),
    start = list(a = 1, b = -0.5),
    params = c("a", "b")
  ),
  "Michaelis-Menten: Vm*x/(K+x)" = list(
    formula = y ~ Vm * x / (K + x),
    start = list(Vm = 200, K = 0.05),
    params = c("Vm", "K")
  ),
  "Logistique: L/(1+exp(-k*(x-x0)))" = list(
    formula = y ~ L / (1 + exp(-k * (x - x0))),
    start = list(L = 1, k = 1, x0 = 5),
    params = c("L", "k", "x0")
  ),
  "Puissance: a*x^b" = list(
    formula = y ~ a * x^b,
    start = list(a = 1, b = 1),
    params = c("a", "b")
  )
)

# --- Fonctions utilitaires ---

# Calcul du PRESS (Predicted Residual Error Sum of Squares)
calc_press <- function(model) {
  if (inherits(model, "lm")) {
    h <- hatvalues(model)
    r <- residuals(model)
    press <- sum((r / (1 - h))^2)
    return(press)
  }
  return(NA)
}

# R² de prédiction
calc_r2_pred <- function(model, data) {
  press <- calc_press(model)
  if (is.na(press)) return(NA)
  ss_tot <- sum((data$y - mean(data$y))^2)
  r2_pred <- 1 - press / ss_tot
  return(r2_pred)
}

# Intervalle de confiance pour modèle linéaire
calc_ic_lm <- function(model, newdata, level = 0.95) {
  predict(model, newdata = newdata, interval = "confidence", level = level)
}

# Intervalle de prédiction pour modèle linéaire
calc_ip_lm <- function(model, newdata, level = 0.95) {
  predict(model, newdata = newdata, interval = "prediction", level = level)
}

# Intervalle de confiance pour NLS via delta method
calc_ic_nls <- function(model, newdata, level = 0.95) {
  pred <- predict(model, newdata = newdata)
  n <- length(residuals(model))
  p <- length(coef(model))
  se_fit <- summary(model)$sigma
  alpha <- 1 - level
  t_val <- qt(1 - alpha / 2, df = n - p)
  
  # Approximation via gradient
  grad_list <- tryCatch({
    numericDeriv(
      expr = quote(predict(model, newdata = newdata)),
      theta = names(coef(model)),
      rho = environment()
    )
  }, error = function(e) NULL)
  
  if (!is.null(grad_list)) {
    J <- attr(grad_list, "gradient")
    vcov_mat <- vcov(model)
    se_pred <- sqrt(diag(J %*% vcov_mat %*% t(J)))
    lwr <- pred - t_val * se_pred
    upr <- pred + t_val * se_pred
  } else {
    lwr <- pred - t_val * se_fit
    upr <- pred + t_val * se_fit
  }
  
  data.frame(fit = pred, lwr = lwr, upr = upr)
}

# Intervalle de prédiction pour NLS
calc_ip_nls <- function(model, newdata, level = 0.95) {
  ic <- calc_ic_nls(model, newdata, level)
  se_fit <- summary(model)$sigma
  n <- length(residuals(model))
  p <- length(coef(model))
  alpha <- 1 - level
  t_val <- qt(1 - alpha / 2, df = n - p)
  
  se_new <- sqrt((ic$fit - ic$lwr)^2 / t_val^2 + se_fit^2)
  ic$lwr <- ic$fit - t_val * se_new
  ic$upr <- ic$fit + t_val * se_new
  ic
}

# Thème ggplot uniforme
theme_app <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}
