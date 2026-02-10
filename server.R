# ============================================================
# Server.R — Logique serveur
# Identification de Modèles Paramétriques
# ============================================================

server <- function(input, output, session) {
  
  # ===================== DONNÉES =====================
  rv <- reactiveValues(data = NULL, lp_model = NULL, nlp_model = NULL, boot_results = NULL)
  
  observeEvent(input$load_data, {
    tryCatch({
      if (input$data_source == "integre") {
        rv$data <- datasets_integres[[input$dataset_choice]]
      } else if (input$data_source == "csv") {
        req(input$csv_file)
        df <- read.csv(input$csv_file$datapath, header = input$csv_header, sep = input$csv_sep)
        names(df)[1:2] <- c("x", "y")
        rv$data <- df
      } else {
        x_vals <- as.numeric(trimws(unlist(strsplit(input$manual_x, ","))))
        y_vals <- as.numeric(trimws(unlist(strsplit(input$manual_y, ","))))
        rv$data <- data.frame(x = x_vals, y = y_vals)
      }
      showNotification("Données chargées avec succès !", type = "message")
    }, error = function(e) {
      showNotification(paste("Erreur :", e$message), type = "error")
    })
  })
  
  output$data_table <- DT::renderDataTable({
    req(rv$data)
    DT::datatable(rv$data, options = list(pageLength = 15, scrollX = TRUE))
  })
  
  output$data_plot <- renderPlotly({
    req(rv$data)
    p <- ggplot(rv$data, aes(x = x, y = y)) +
      geom_point(color = "#3c8dbc", size = 3, alpha = 0.8) +
      labs(title = "Nuage de points", x = "x", y = "y") + theme_app()
    ggplotly(p)
  })
  
  output$data_summary <- renderPrint({
    req(rv$data)
    cat("=== Statistiques descriptives ===\n\n")
    print(summary(rv$data))
    cat("\nNombre d'observations :", nrow(rv$data))
    cat("\nCorrélation :", cor(rv$data$x, rv$data$y), "\n")
  })
  
  # ===================== MODÈLE LINÉAIRE (LP) =====================
  observeEvent(input$fit_lp, {
    req(rv$data)
    tryCatch({
      formula <- switch(input$lp_type,
        "simple" = y ~ x,
        "poly2"  = y ~ x + I(x^2),
        "poly3"  = y ~ x + I(x^2) + I(x^3),
        "multi"  = y ~ .,
        y ~ x
      )
      rv$lp_model <- lm(formula, data = rv$data)
      showNotification("Modèle LP ajusté !", type = "message")
    }, error = function(e) {
      showNotification(paste("Erreur LP :", e$message), type = "error")
    })
  })
  
  output$lp_formula <- renderText({
    req(rv$lp_model)
    coefs <- round(coef(rv$lp_model), 4)
    paste("ŷ =", paste(names(coefs), "=", coefs, collapse = ", "))
  })
  
  output$lp_fit_plot <- renderPlotly({
    req(rv$lp_model, rv$data)
    d <- rv$data
    x_seq <- data.frame(x = seq(min(d$x), max(d$x), length.out = 200))
    ic <- as.data.frame(calc_ic_lm(rv$lp_model, x_seq, input$lp_conf_level))
    ip <- as.data.frame(calc_ip_lm(rv$lp_model, x_seq, input$lp_conf_level))
    x_seq$fit <- ic$fit; x_seq$ic_lwr <- ic$lwr; x_seq$ic_upr <- ic$upr
    x_seq$ip_lwr <- ip$lwr; x_seq$ip_upr <- ip$upr
    
    p <- ggplot() + geom_point(data = d, aes(x, y), color = "#2c3e50", size = 3, alpha = 0.7)
    if (input$lp_show_ip) {
      p <- p + geom_ribbon(data = x_seq, aes(x = x, ymin = ip_lwr, ymax = ip_upr),
                           fill = "#e74c3c", alpha = 0.15)
    }
    if (input$lp_show_ic) {
      p <- p + geom_ribbon(data = x_seq, aes(x = x, ymin = ic_lwr, ymax = ic_upr),
                           fill = "#3498db", alpha = 0.3)
    }
    p <- p + geom_line(data = x_seq, aes(x, fit), color = "#e74c3c", linewidth = 1.2) +
      labs(title = "Ajustement LP avec IC et IP", x = "x", y = "y") + theme_app()
    ggplotly(p)
  })
  
  output$lp_summary <- renderPrint({ req(rv$lp_model); summary(rv$lp_model) })
  
  output$lp_resid_hist <- renderPlot({
    req(rv$lp_model)
    r <- data.frame(res = residuals(rv$lp_model))
    ggplot(r, aes(x = res)) + geom_histogram(bins = 15, fill = "#3498db", color = "white", alpha = 0.8) +
      labs(title = "Histogramme des résidus", x = "Résidus", y = "Fréquence") + theme_app()
  })
  
  output$lp_resid_qq <- renderPlot({
    req(rv$lp_model)
    r <- data.frame(res = residuals(rv$lp_model))
    ggplot(r, aes(sample = res)) + stat_qq(color = "#3498db", size = 2) + stat_qq_line(color = "#e74c3c") +
      labs(title = "QQ-Plot des résidus") + theme_app()
  })
  
  output$lp_resid_fitted <- renderPlot({
    req(rv$lp_model)
    r <- data.frame(fitted = fitted(rv$lp_model), res = residuals(rv$lp_model))
    ggplot(r, aes(x = fitted, y = res)) + geom_point(color = "#3498db", size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#e74c3c") +
      labs(title = "Résidus vs Valeurs ajustées", x = "Valeurs ajustées", y = "Résidus") + theme_app()
  })
  
  output$lp_coef_table <- DT::renderDataTable({
    req(rv$lp_model)
    s <- summary(rv$lp_model)
    df <- as.data.frame(s$coefficients)
    df$Paramètre <- rownames(df)
    df <- df[, c(5, 1:4)]
    names(df) <- c("Paramètre", "Estimation", "Erreur Std", "t-value", "p-value")
    DT::datatable(df, rownames = FALSE, options = list(dom = 't')) %>%
      DT::formatSignif(columns = 2:5, digits = 4)
  })
  
  # ===================== MODÈLE NON-LINÉAIRE (NLP) =====================
  observeEvent(input$fit_nlp, {
    req(rv$data)
    tryCatch({
      if (input$nlp_custom) {
        form <- as.formula(input$nlp_custom_formula)
        start_str <- trimws(unlist(strsplit(input$nlp_custom_start, ",")))
        start_list <- lapply(start_str, function(s) {
          kv <- trimws(unlist(strsplit(s, "=")))
          as.numeric(kv[2])
        })
        names(start_list) <- sapply(start_str, function(s) trimws(unlist(strsplit(s, "=")))[1])
      } else {
        mod_info <- modeles_nlp[[input$nlp_model]]
        form <- mod_info$formula
        start_list <- mod_info$start
      }
      
      if (input$nlp_algo == "lm") {
        rv$nlp_model <- nlsLM(form, data = rv$data, start = start_list,
                               control = nls.lm.control(maxiter = input$nlp_maxiter))
      } else {
        rv$nlp_model <- nls(form, data = rv$data, start = start_list,
                            control = nls.control(maxiter = input$nlp_maxiter))
      }
      showNotification("Modèle NLP ajusté !", type = "message")
    }, error = function(e) {
      showNotification(paste("Erreur NLP :", e$message), type = "error")
    })
  })
  
  output$nlp_formula_display <- renderText({
    req(rv$nlp_model)
    coefs <- round(coef(rv$nlp_model), 4)
    paste(names(coefs), "=", coefs, collapse = " | ")
  })
  
  output$nlp_fit_plot <- renderPlotly({
    req(rv$nlp_model, rv$data)
    d <- rv$data
    x_seq <- data.frame(x = seq(min(d$x), max(d$x), length.out = 200))
    pred <- predict(rv$nlp_model, newdata = x_seq)
    ic <- calc_ic_nls(rv$nlp_model, x_seq, input$nlp_conf_level)
    ip <- calc_ip_nls(rv$nlp_model, x_seq, input$nlp_conf_level)
    x_seq$fit <- pred; x_seq$ic_lwr <- ic$lwr; x_seq$ic_upr <- ic$upr
    x_seq$ip_lwr <- ip$lwr; x_seq$ip_upr <- ip$upr
    
    p <- ggplot() +
      geom_ribbon(data = x_seq, aes(x, ymin = ip_lwr, ymax = ip_upr), fill = "#e74c3c", alpha = 0.15) +
      geom_ribbon(data = x_seq, aes(x, ymin = ic_lwr, ymax = ic_upr), fill = "#3498db", alpha = 0.3) +
      geom_point(data = d, aes(x, y), color = "#2c3e50", size = 3, alpha = 0.7) +
      geom_line(data = x_seq, aes(x, fit), color = "#e74c3c", linewidth = 1.2) +
      labs(title = "Ajustement NLP", x = "x", y = "y") + theme_app()
    ggplotly(p)
  })
  
  output$nlp_summary <- renderPrint({ req(rv$nlp_model); summary(rv$nlp_model) })
  
  output$nlp_resid_hist <- renderPlot({
    req(rv$nlp_model)
    r <- data.frame(res = residuals(rv$nlp_model))
    ggplot(r, aes(x = res)) + geom_histogram(bins = 15, fill = "#e67e22", color = "white", alpha = 0.8) +
      labs(title = "Résidus NLP", x = "Résidus", y = "Fréquence") + theme_app()
  })
  
  output$nlp_resid_qq <- renderPlot({
    req(rv$nlp_model)
    r <- data.frame(res = residuals(rv$nlp_model))
    ggplot(r, aes(sample = res)) + stat_qq(color = "#e67e22", size = 2) + stat_qq_line(color = "#e74c3c") +
      labs(title = "QQ-Plot NLP") + theme_app()
  })
  
  output$nlp_resid_fitted <- renderPlot({
    req(rv$nlp_model)
    r <- data.frame(fitted = fitted(rv$nlp_model), res = residuals(rv$nlp_model))
    ggplot(r, aes(x = fitted, y = res)) + geom_point(color = "#e67e22", size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#e74c3c") +
      labs(title = "Résidus vs Ajustées (NLP)", x = "Valeurs ajustées", y = "Résidus") + theme_app()
  })
  
  output$nlp_vcov <- renderPrint({
    req(rv$nlp_model)
    cat("=== Matrice de Variance-Covariance ===\n\n")
    print(vcov(rv$nlp_model))
    cat("\n=== Matrice de Corrélation ===\n\n")
    vc <- vcov(rv$nlp_model)
    print(cov2cor(vc))
  })
  
  # ===================== LINÉARISATION =====================
  observeEvent(input$fit_lin, {
    req(rv$data)
    # Handled via reactive outputs
  })
  
  lin_data <- reactive({
    req(rv$data, input$fit_lin)
    d <- rv$data
    tryCatch({
      switch(input$lin_transform,
        "log_y"   = data.frame(x = d$x, y = log(d$y), x_orig = d$x, y_orig = d$y),
        "log_log" = data.frame(x = log(d$x), y = log(d$y), x_orig = d$x, y_orig = d$y),
        "inv_y"   = data.frame(x = d$x, y = 1/d$y, x_orig = d$x, y_orig = d$y),
        "inv_inv" = data.frame(x = 1/d$x, y = 1/d$y, x_orig = d$x, y_orig = d$y),
        "sqrt_y"  = data.frame(x = d$x, y = sqrt(d$y), x_orig = d$x, y_orig = d$y)
      )
    }, error = function(e) NULL)
  })
  
  output$lin_transformed_plot <- renderPlot({
    req(lin_data())
    ld <- lin_data()
    m <- lm(y ~ x, data = ld)
    p <- ggplot(ld, aes(x, y)) + geom_point(color = "#2c3e50", size = 3) +
      geom_smooth(method = "lm", se = TRUE, color = "#3498db", fill = "#3498db", alpha = 0.2) +
      labs(title = paste("Espace transformé :", input$lin_transform)) + theme_app()
    p
  })
  
  output$lin_original_plot <- renderPlot({
    req(rv$data, input$fit_lin)
    d <- rv$data
    mod_info <- modeles_nlp[[input$lin_nlp_model]]
    nlp_fit <- tryCatch({
      nlsLM(mod_info$formula, data = d, start = mod_info$start)
    }, error = function(e) NULL)
    
    p <- ggplot(d, aes(x, y)) + geom_point(color = "#2c3e50", size = 3)
    if (!is.null(nlp_fit)) {
      x_seq <- data.frame(x = seq(min(d$x), max(d$x), length.out = 200))
      x_seq$pred <- predict(nlp_fit, newdata = x_seq)
      p <- p + geom_line(data = x_seq, aes(x, pred), color = "#e74c3c", linewidth = 1.2)
    }
    p + labs(title = "Espace original (NLP)") + theme_app()
  })
  
  output$lin_lm_summary <- renderPrint({
    req(lin_data())
    m <- lm(y ~ x, data = lin_data())
    summary(m)
  })
  
  output$lin_nlp_summary <- renderPrint({
    req(rv$data, input$fit_lin)
    mod_info <- modeles_nlp[[input$lin_nlp_model]]
    tryCatch({
      m <- nlsLM(mod_info$formula, data = rv$data, start = mod_info$start)
      summary(m)
    }, error = function(e) cat("Erreur NLP :", e$message))
  })
  
  output$lin_comparison_table <- DT::renderDataTable({
    req(lin_data(), rv$data, input$fit_lin)
    lm_mod <- lm(y ~ x, data = lin_data())
    mod_info <- modeles_nlp[[input$lin_nlp_model]]
    nlp_mod <- tryCatch(nlsLM(mod_info$formula, data = rv$data, start = mod_info$start), error = function(e) NULL)
    
    r2_lm <- summary(lm_mod)$r.squared
    rss_lm <- sum(residuals(lm_mod)^2)
    comp <- data.frame(
      Indicateur = c("R²", "RSS", "Nb paramètres"),
      "LP linéarisé" = c(round(r2_lm, 4), round(rss_lm, 4), length(coef(lm_mod))),
      check.names = FALSE
    )
    if (!is.null(nlp_mod)) {
      ss_tot <- sum((rv$data$y - mean(rv$data$y))^2)
      rss_nlp <- sum(residuals(nlp_mod)^2)
      r2_nlp <- 1 - rss_nlp / ss_tot
      comp$"NLP direct" <- c(round(r2_nlp, 4), round(rss_nlp, 4), length(coef(nlp_mod)))
    }
    DT::datatable(comp, rownames = FALSE, options = list(dom = 't'))
  })
  
  # ===================== BOOTSTRAP =====================
  observeEvent(input$run_boot, {
    req(rv$data)
    tryCatch({
      d <- rv$data
      n_boot <- input$boot_n
      
      if (input$boot_model_type == "lp") {
        base_model <- lm(y ~ x, data = d)
        boot_fn <- function(data, indices) {
          if (input$boot_type == "residual") {
            new_data <- data
            new_data$y <- fitted(base_model) + residuals(base_model)[indices]
          } else {
            new_data <- data[indices, ]
          }
          m <- lm(y ~ x, data = new_data)
          coef(m)
        }
      } else {
        mod_info <- modeles_nlp[[input$boot_nlp_model]]
        base_model <- nlsLM(mod_info$formula, data = d, start = mod_info$start)
        boot_fn <- function(data, indices) {
          if (input$boot_type == "residual") {
            new_data <- data
            new_data$y <- fitted(base_model) + residuals(base_model)[indices]
          } else {
            new_data <- data[indices, ]
          }
          tryCatch({
            m <- nlsLM(mod_info$formula, data = new_data, start = as.list(coef(base_model)))
            coef(m)
          }, error = function(e) rep(NA, length(coef(base_model))))
        }
      }
      
      withProgress(message = "Bootstrap en cours...", value = 0, {
        rv$boot_results <- boot(data = d, statistic = boot_fn, R = n_boot)
        incProgress(1)
      })
      
      showNotification(paste("Bootstrap terminé :", n_boot, "itérations"), type = "message")
    }, error = function(e) {
      showNotification(paste("Erreur Bootstrap :", e$message), type = "error")
    })
  })
  
  output$boot_progress <- renderPrint({
    if (is.null(rv$boot_results)) {
      cat("En attente du lancement...")
    } else {
      cat("Bootstrap terminé.\nItérations :", rv$boot_results$R, "\n")
      cat("Paramètres estimés :", ncol(rv$boot_results$t))
    }
  })
  
  output$boot_hist_plot <- renderPlot({
    req(rv$boot_results)
    bt <- rv$boot_results
    n_params <- ncol(bt$t)
    par(mfrow = c(ceiling(n_params / 2), min(n_params, 2)))
    for (i in 1:n_params) {
      vals <- bt$t[, i]
      vals <- vals[!is.na(vals)]
      hist(vals, breaks = 30, col = "#3498db80", border = "white",
           main = paste("Paramètre", i), xlab = "Valeur", ylab = "Fréquence")
      abline(v = bt$t0[i], col = "#e74c3c", lwd = 2, lty = 2)
    }
  })
  
  output$boot_ci_table <- DT::renderDataTable({
    req(rv$boot_results)
    bt <- rv$boot_results
    n_params <- ncol(bt$t)
    ci_df <- data.frame(Paramètre = paste("Param", 1:n_params),
                        Estimation = round(bt$t0, 4),
                        "Erreur Std Boot" = round(apply(bt$t, 2, sd, na.rm = TRUE), 4),
                        check.names = FALSE)
    for (i in 1:n_params) {
      ci <- tryCatch(boot.ci(bt, index = i, conf = input$boot_conf, type = "perc"),
                     error = function(e) NULL)
      if (!is.null(ci)) {
        ci_df$IC_inf[i] <- round(ci$percent[4], 4)
        ci_df$IC_sup[i] <- round(ci$percent[5], 4)
      } else {
        ci_df$IC_inf[i] <- NA; ci_df$IC_sup[i] <- NA
      }
    }
    DT::datatable(ci_df, rownames = FALSE, options = list(dom = 't'))
  })
  
  output$boot_compare_table <- DT::renderDataTable({
    req(rv$boot_results)
    bt <- rv$boot_results
    n_params <- ncol(bt$t)
    
    # IC analytique
    if (input$boot_model_type == "lp") {
      req(rv$lp_model)
      ci_anal <- confint(rv$lp_model, level = input$boot_conf)
    } else {
      req(rv$nlp_model)
      ci_anal <- tryCatch(confint(rv$nlp_model, level = input$boot_conf), error = function(e) NULL)
    }
    
    comp <- data.frame(Paramètre = paste("Param", 1:n_params))
    for (i in 1:n_params) {
      ci_boot <- tryCatch(boot.ci(bt, index = i, conf = input$boot_conf, type = "perc"),
                          error = function(e) NULL)
      comp$Boot_IC_inf[i] <- if (!is.null(ci_boot)) round(ci_boot$percent[4], 4) else NA
      comp$Boot_IC_sup[i] <- if (!is.null(ci_boot)) round(ci_boot$percent[5], 4) else NA
      if (!is.null(ci_anal) && i <= nrow(ci_anal)) {
        comp$Anal_IC_inf[i] <- round(ci_anal[i, 1], 4)
        comp$Anal_IC_sup[i] <- round(ci_anal[i, 2], 4)
      } else {
        comp$Anal_IC_inf[i] <- NA; comp$Anal_IC_sup[i] <- NA
      }
    }
    DT::datatable(comp, rownames = FALSE, options = list(dom = 't'))
  })
  
  # ===================== QUALITÉ DU MODÈLE =====================
  current_model <- reactive({
    if (!is.null(rv$lp_model)) rv$lp_model else rv$nlp_model
  })
  
  observeEvent(input$calc_quality, { })  # trigger reactifs
  
  output$r2_box <- renderInfoBox({
    req(current_model(), input$calc_quality)
    m <- current_model()
    r2 <- if (inherits(m, "lm")) summary(m)$r.squared else {
      ss_res <- sum(residuals(m)^2); ss_tot <- sum((rv$data$y - mean(rv$data$y))^2); 1 - ss_res/ss_tot
    }
    infoBox("R²", round(r2, 4), icon = icon("chart-bar"), color = "blue")
  })
  
  output$r2a_box <- renderInfoBox({
    req(current_model(), input$calc_quality)
    m <- current_model()
    if (inherits(m, "lm")) {
      r2a <- summary(m)$adj.r.squared
    } else {
      n <- length(residuals(m)); p <- length(coef(m))
      ss_res <- sum(residuals(m)^2); ss_tot <- sum((rv$data$y - mean(rv$data$y))^2)
      r2 <- 1 - ss_res/ss_tot
      r2a <- 1 - (1 - r2) * (n - 1) / (n - p - 1)
    }
    infoBox("R² ajusté", round(r2a, 4), icon = icon("chart-line"), color = "green")
  })
  
  output$press_box <- renderInfoBox({
    req(current_model(), input$calc_quality)
    press <- calc_press(current_model())
    infoBox("PRESS", if (is.na(press)) "N/A (NLP)" else round(press, 2),
            icon = icon("compress"), color = "yellow")
  })
  
  output$r2pred_box <- renderInfoBox({
    req(current_model(), rv$data, input$calc_quality)
    r2p <- calc_r2_pred(current_model(), rv$data)
    infoBox("R² prédiction", if (is.na(r2p)) "N/A (NLP)" else round(r2p, 4),
            icon = icon("bullseye"), color = "red")
  })
  
  output$qual_resid_plot <- renderPlot({
    req(current_model(), input$calc_quality)
    m <- current_model()
    r <- data.frame(fitted = fitted(m), res = residuals(m), std_res = residuals(m) / sd(residuals(m)))
    p1 <- ggplot(r, aes(sample = std_res)) + stat_qq(color = "#3498db") + stat_qq_line(color = "#e74c3c") +
      labs(title = "QQ-Plot résidus standardisés") + theme_app()
    p2 <- ggplot(r, aes(x = fitted, y = std_res)) + geom_point(color = "#3498db") +
      geom_hline(yintercept = 0, lty = 2, color = "#e74c3c") +
      labs(title = "Résidus std vs Ajustées", x = "Ajustées", y = "Résidus std") + theme_app()
    gridExtra::grid.arrange(p1, p2, ncol = 2)
  })
  
  output$qual_shapiro <- renderPrint({
    req(current_model(), input$calc_quality)
    cat("=== Test de normalité (Shapiro-Wilk) ===\n\n")
    res <- residuals(current_model())
    if (length(res) >= 3 && length(res) <= 5000) {
      print(shapiro.test(res))
    } else {
      cat("Taille d'échantillon hors limites pour Shapiro-Wilk\n")
    }
  })
  
  output$qual_signif_table <- DT::renderDataTable({
    req(current_model(), input$calc_quality)
    m <- current_model()
    s <- summary(m)
    df <- as.data.frame(s$coefficients)
    df$Paramètre <- rownames(df)
    nc <- ncol(df)
    df <- df[, c(nc, 1:(nc-1))]
    names(df)[ncol(df)] <- "p-value"
    df$Significatif <- ifelse(df$`p-value` < 0.05, "✓ Oui", "✗ Non")
    DT::datatable(df, rownames = FALSE, options = list(dom = 't')) %>%
      DT::formatSignif(columns = 2:(ncol(df)-1), digits = 4)
  })
  
  output$qual_anova <- renderPrint({
    req(current_model(), input$calc_quality)
    m <- current_model()
    if (inherits(m, "lm")) {
      cat("=== ANOVA ===\n\n")
      print(anova(m))
    } else {
      cat("ANOVA non disponible pour modèle NLP.\n")
      cat("RSS =", sum(residuals(m)^2), "\n")
      cat("Sigma =", summary(m)$sigma, "\n")
    }
  })
  
  # ===================== MLE =====================
  observeEvent(input$fit_mle, {
    req(rv$data)
    tryCatch({
      d <- rv$data
      
      if (input$mle_model_type == "lp") {
        neg_loglik <- function(params) {
          b0 <- params[1]; b1 <- params[2]; sigma <- exp(params[3])
          mu <- b0 + b1 * d$x
          -sum(dnorm(d$y, mean = mu, sd = sigma, log = TRUE))
        }
        lm_init <- lm(y ~ x, data = d)
        init <- c(coef(lm_init), log(summary(lm_init)$sigma))
        mle_fit <- optim(init, neg_loglik, method = input$mle_method, hessian = TRUE)
        
        rv$mle_result <- list(
          par = mle_fit$par, hessian = mle_fit$hessian,
          loglik = -mle_fit$value, convergence = mle_fit$convergence,
          n = nrow(d), k = 3,
          param_names = c("Intercept", "Pente", "log(sigma)"),
          type = "lp"
        )
      } else {
        mod_info <- modeles_nlp[[input$mle_nlp_model]]
        base_fit <- nlsLM(mod_info$formula, data = d, start = mod_info$start)
        
        neg_loglik_nlp <- function(params) {
          n_p <- length(mod_info$params)
          theta <- params[1:n_p]; sigma <- exp(params[n_p + 1])
          names(theta) <- mod_info$params
          env <- as.list(theta); env$x <- d$x
          mu <- tryCatch(eval(mod_info$formula[[3]], envir = env), error = function(e) rep(NA, nrow(d)))
          if (any(is.na(mu))) return(1e10)
          -sum(dnorm(d$y, mean = mu, sd = sigma, log = TRUE))
        }
        init_nlp <- c(coef(base_fit), log(summary(base_fit)$sigma))
        mle_fit <- optim(init_nlp, neg_loglik_nlp, method = input$mle_method, hessian = TRUE)
        
        rv$mle_result <- list(
          par = mle_fit$par, hessian = mle_fit$hessian,
          loglik = -mle_fit$value, convergence = mle_fit$convergence,
          n = nrow(d), k = length(mod_info$params) + 1,
          param_names = c(mod_info$params, "log(sigma)"),
          type = "nlp"
        )
      }
      showNotification("Estimation MLE terminée !", type = "message")
    }, error = function(e) {
      showNotification(paste("Erreur MLE :", e$message), type = "error")
    })
  })
  
  output$mle_summary <- renderPrint({
    req(rv$mle_result)
    r <- rv$mle_result
    cat("=== Résultats MLE ===\n\n")
    cat("Convergence :", ifelse(r$convergence == 0, "OK", "ÉCHEC"), "\n")
    cat("Log-vraisemblance :", round(r$loglik, 4), "\n\n")
    se <- tryCatch(sqrt(diag(solve(r$hessian))), error = function(e) rep(NA, length(r$par)))
    df <- data.frame(Paramètre = r$param_names, Estimation = round(r$par, 4),
                     "Erreur Std" = round(se, 4), check.names = FALSE)
    print(df, row.names = FALSE)
  })
  
  output$mle_compare_table <- DT::renderDataTable({
    req(rv$mle_result)
    r <- rv$mle_result
    
    if (r$type == "lp" && !is.null(rv$lp_model)) {
      mco <- coef(rv$lp_model)
      mle <- r$par[1:length(mco)]
      comp <- data.frame(Paramètre = names(mco), MCO = round(mco, 4), MLE = round(mle, 4))
    } else if (r$type == "nlp" && !is.null(rv$nlp_model)) {
      mco <- coef(rv$nlp_model)
      mle <- r$par[1:length(mco)]
      comp <- data.frame(Paramètre = names(mco), MCO = round(mco, 4), MLE = round(mle, 4))
    } else {
      comp <- data.frame(Paramètre = r$param_names, MLE = round(r$par, 4))
    }
    DT::datatable(comp, rownames = FALSE, options = list(dom = 't'))
  })
  
  output$mle_loglik_plot <- renderPlot({
    req(rv$mle_result, rv$data)
    r <- rv$mle_result
    d <- rv$data
    
    # Profile de log-vraisemblance pour le 1er paramètre
    p1_range <- seq(r$par[1] * 0.5, r$par[1] * 1.5, length.out = 100)
    if (r$par[1] == 0) p1_range <- seq(-2, 2, length.out = 100)
    
    ll_vals <- sapply(p1_range, function(p1_val) {
      params <- r$par; params[1] <- p1_val
      if (r$type == "lp") {
        sigma <- exp(params[3]); mu <- params[1] + params[2] * d$x
        sum(dnorm(d$y, mean = mu, sd = sigma, log = TRUE))
      } else { r$loglik }
    })
    
    df_ll <- data.frame(param = p1_range, loglik = ll_vals)
    ggplot(df_ll, aes(x = param, y = loglik)) + geom_line(color = "#3498db", linewidth = 1.2) +
      geom_vline(xintercept = r$par[1], lty = 2, color = "#e74c3c") +
      labs(title = paste("Profil log-vraisemblance —", r$param_names[1]),
           x = r$param_names[1], y = "Log-vraisemblance") + theme_app()
  })
  
  output$mle_aic_box <- renderInfoBox({
    req(rv$mle_result)
    r <- rv$mle_result
    aic <- -2 * r$loglik + 2 * r$k
    infoBox("AIC", round(aic, 2), icon = icon("balance-scale"), color = "blue")
  })
  
  output$mle_bic_box <- renderInfoBox({
    req(rv$mle_result)
    r <- rv$mle_result
    bic <- -2 * r$loglik + log(r$n) * r$k
    infoBox("BIC", round(bic, 2), icon = icon("balance-scale-right"), color = "green")
  })
  
  output$mle_loglik_box <- renderInfoBox({
    req(rv$mle_result)
    infoBox("Log-Vraisemblance", round(rv$mle_result$loglik, 2), icon = icon("chart-area"), color = "red")
  })
  
} # fin server
