server <- function(input, output, session) {
  rv <- reactiveValues(
    data = make_builtin_data("lineaire", n = 60, seed = 123, noise_sd = 1),
    data_source = "Jeu integre",
    lp_model = NULL,
    nlp_model = NULL,
    active_model = NULL,
    lin_model = NULL,
    boot = NULL,
    mle = NULL
  )

  reset_model_state <- function() {
    rv$lp_model <- NULL
    rv$nlp_model <- NULL
    rv$active_model <- NULL
    rv$lin_model <- NULL
    rv$boot <- NULL
    rv$mle <- NULL
  }

  numeric_cols <- reactive({
    req(rv$data)
    names(rv$data)[vapply(rv$data, is.numeric, logical(1))]
  })

  observe({
    cols <- numeric_cols()
    if (length(cols) == 0) {
      return()
    }

    y_default <- if ("y" %in% cols) "y" else cols[1]
    x_candidates <- setdiff(cols, y_default)
    x_default <- if ("x" %in% x_candidates) "x" else (x_candidates[1] %||% cols[1])

    updateSelectInput(session, "lp_y", choices = cols, selected = y_default)
    updateSelectInput(session, "lp_x", choices = cols, selected = x_default)
    updateSelectizeInput(session, "lp_multi_x", choices = setdiff(cols, y_default), selected = intersect("z", cols), server = TRUE)

    updateSelectInput(session, "nlp_y", choices = cols, selected = y_default)
    updateSelectInput(session, "nlp_x", choices = cols, selected = x_default)

    updateSelectInput(session, "lin_y", choices = cols, selected = y_default)
    updateSelectInput(session, "lin_x", choices = cols, selected = x_default)

    updateSelectInput(session, "view_y", choices = cols, selected = y_default)
    updateSelectInput(session, "view_x", choices = cols, selected = x_default)
  })

  output$var_selectors <- renderUI({
    cols <- numeric_cols()
    if (length(cols) == 0) {
      return(tags$em("Aucune colonne numerique detectee."))
    }

    y_default <- if ("y" %in% cols) "y" else cols[1]
    x_candidates <- setdiff(cols, y_default)
    x_default <- if ("x" %in% x_candidates) "x" else (x_candidates[1] %||% cols[1])

    tagList(
      selectInput("view_y", "Variable Y (visualisation)", choices = cols, selected = y_default),
      selectInput("view_x", "Variable X (visualisation)", choices = cols, selected = x_default)
    )
  })

  observeEvent(
    list(input$data_source, input$builtin_dataset, input$builtin_n, input$builtin_seed, input$builtin_noise),
    {
      if (!identical(input$data_source, "Jeu integre")) {
        return()
      }

      df <- safe_run(
        make_builtin_data(
          kind = input$builtin_dataset,
          n = input$builtin_n,
          seed = input$builtin_seed,
          noise_sd = input$builtin_noise
        ),
        fallback = NULL,
        context = "jeu integre"
      )

      if (is.null(df)) {
        showNotification("Echec du chargement du jeu integre.", type = "error")
        return()
      }

      rv$data <- df
      rv$data_source <- "Jeu integre"
      reset_model_state()
    },
    ignoreInit = FALSE
  )

  observeEvent(
    list(input$data_source, input$csv_file, input$csv_header, input$csv_sep, input$csv_dec),
    {
      if (!identical(input$data_source, "CSV")) {
        return()
      }

      req(input$csv_file)

      df <- safe_run(
        read.csv(
          file = input$csv_file$datapath,
          header = isTRUE(input$csv_header),
          sep = input$csv_sep,
          dec = input$csv_dec,
          stringsAsFactors = FALSE
        ),
        fallback = NULL,
        context = "lecture CSV"
      )

      if (is.null(df) || ncol(df) < 2) {
        showNotification("CSV invalide: au moins deux colonnes requises.", type = "error")
        return()
      }

      rv$data <- as.data.frame(df)
      rv$data_source <- "CSV"
      reset_model_state()
      showNotification("CSV charge avec succes.", type = "message")
    },
    ignoreInit = TRUE
  )

  observeEvent(input$apply_manual, {
    if (!identical(input$data_source, "Saisie manuelle")) {
      return()
    }

    txt <- trimws(input$manual_text %||% "")
    if (!nzchar(txt)) {
      showNotification("La saisie manuelle est vide.", type = "error")
      return()
    }

    df <- safe_run(
      read.csv(text = txt, header = TRUE, stringsAsFactors = FALSE),
      fallback = NULL,
      context = "saisie manuelle header"
    )

    if (is.null(df)) {
      df <- safe_run(
        {
          tmp <- read.csv(text = txt, header = FALSE, stringsAsFactors = FALSE)
          names(tmp) <- paste0("V", seq_len(ncol(tmp)))
          tmp
        },
        fallback = NULL,
        context = "saisie manuelle sans header"
      )
    }

    if (is.null(df) || ncol(df) < 2) {
      showNotification("Saisie manuelle invalide.", type = "error")
      return()
    }

    rv$data <- as.data.frame(df)
    rv$data_source <- "Saisie manuelle"
    reset_model_state()
    showNotification("Saisie manuelle chargee.", type = "message")
  })

  output$data_source_note <- renderText({
    sprintf("Source active: %s | Lignes: %d | Colonnes: %d", rv$data_source, nrow(rv$data), ncol(rv$data))
  })

  output$data_table <- renderDT({
    req(rv$data)
    datatable(
      rv$data,
      options = list(pageLength = 8, scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$data_plot <- renderPlotly({
    req(rv$data, input$view_x, input$view_y)

    df <- safe_run(clean_xy_data(rv$data, input$view_x, input$view_y), fallback = NULL, context = "plot donnees")
    req(df)

    p <- ggplot(df, aes_string(x = input$view_x, y = input$view_y)) +
      geom_point(color = APP_COLORS["primary"], alpha = 0.85, size = 2.1) +
      geom_smooth(method = "loess", se = FALSE, color = APP_COLORS["warning"], linewidth = 0.9) +
      labs(x = input$view_x, y = input$view_y)

    ggplotly(p)
  })

  output$desc_stats <- renderDT({
    req(rv$data)

    num_cols <- rv$data[vapply(rv$data, is.numeric, logical(1))]
    if (ncol(num_cols) == 0) {
      return(datatable(data.frame(message = "Aucune colonne numerique."), options = list(dom = "t"), rownames = FALSE))
    }

    stats_df <- do.call(
      rbind,
      lapply(names(num_cols), function(nm) {
        x <- as.numeric(num_cols[[nm]])
        data.frame(
          variable = nm,
          n = sum(is.finite(x)),
          moyenne = mean(x, na.rm = TRUE),
          ecart_type = sd(x, na.rm = TRUE),
          min = min(x, na.rm = TRUE),
          mediane = median(x, na.rm = TRUE),
          max = max(x, na.rm = TRUE),
          row.names = NULL,
          check.names = FALSE
        )
      })
    )

    datatable(stats_df, options = list(pageLength = 10), rownames = FALSE)
  })

  build_lp_formula <- function(lp_type, y_var, x_var, multi_vars = NULL) {
    if (identical(lp_type, "simple")) {
      as.formula(sprintf("%s ~ %s", y_var, x_var))
    } else if (identical(lp_type, "poly2")) {
      as.formula(sprintf("%s ~ poly(%s, 2, raw = TRUE)", y_var, x_var))
    } else if (identical(lp_type, "poly3")) {
      as.formula(sprintf("%s ~ poly(%s, 3, raw = TRUE)", y_var, x_var))
    } else {
      preds <- unique(c(x_var, multi_vars))
      preds <- preds[preds != y_var]
      if (length(preds) == 0) {
        stop("Le modele multiple requiert au moins un predicteur.")
      }
      as.formula(sprintf("%s ~ %s", y_var, paste(preds, collapse = " + ")))
    }
  }

  observeEvent(input$run_lp, {
    req(rv$data, input$lp_y, input$lp_x)

    form <- safe_run(
      build_lp_formula(input$lp_type, input$lp_y, input$lp_x, input$lp_multi_x),
      fallback = NULL,
      context = "formule LP"
    )
    req(form)

    lp_obj <- safe_run(
      fit_lp(rv$data, form, y_col = input$lp_y, x_var = input$lp_x),
      fallback = NULL,
      context = "fit LP"
    )

    if (is.null(lp_obj)) {
      showNotification("Echec estimation LP. Verifiez les variables et les donnees.", type = "error")
      return()
    }

    rv$lp_model <- lp_obj
    rv$active_model <- lp_obj
    rv$boot <- NULL
    rv$mle <- NULL

    showNotification("Modele LP estime.", type = "message")
  })

  output$lp_coef_table <- renderDT({
    req(rv$lp_model)
    datatable(rv$lp_model$coef_table, options = list(pageLength = 10), rownames = FALSE)
  })

  output$lp_fit_stats <- renderDT({
    req(rv$lp_model)
    stats_df <- metrics_to_df(rv$lp_model$metrics)
    stats_df$formule <- paste(deparse(rv$lp_model$formula), collapse = "")
    datatable(stats_df, options = list(dom = "t"), rownames = FALSE)
  })

  output$lp_fit_plot <- renderPlotly({
    req(rv$lp_model)

    lp <- rv$lp_model
    fit <- lp$fit_obj
    df <- lp$data
    x_var <- lp$meta$x_var %||% input$lp_x
    y_var <- lp$meta$y_var %||% input$lp_y

    df <- safe_run(clean_xy_data(df, x_var, y_var), fallback = NULL, context = "LP plot clean")
    req(df)

    x_seq <- seq(min(df[[x_var]], na.rm = TRUE), max(df[[x_var]], na.rm = TRUE), length.out = 180)
    newdata <- data.frame(tmp = x_seq)
    names(newdata)[1] <- x_var

    pred_vars <- setdiff(all.vars(formula(fit)), y_var)
    for (v in setdiff(pred_vars, x_var)) {
      if (!(v %in% names(df))) {
        next
      }
      if (is.numeric(df[[v]])) {
        newdata[[v]] <- median(df[[v]], na.rm = TRUE)
      } else {
        newdata[[v]] <- df[[v]][1]
      }
    }

    ci <- safe_run(as.data.frame(predict(fit, newdata = newdata, interval = "confidence")), fallback = NULL, context = "LP CI")
    pi <- safe_run(as.data.frame(predict(fit, newdata = newdata, interval = "prediction")), fallback = NULL, context = "LP PI")

    fit_line <- safe_run(as.numeric(predict(fit, newdata = newdata)), fallback = rep(NA_real_, nrow(newdata)), context = "LP fit line")

    plot_df <- data.frame(
      x = newdata[[x_var]],
      fit = fit_line,
      lwr_ci = if (!is.null(ci)) ci$lwr else NA_real_,
      upr_ci = if (!is.null(ci)) ci$upr else NA_real_,
      lwr_pi = if (!is.null(pi)) pi$lwr else NA_real_,
      upr_pi = if (!is.null(pi)) pi$upr else NA_real_
    )

    p <- ggplot() +
      geom_point(data = df, aes_string(x = x_var, y = y_var), color = APP_COLORS["dark"], alpha = 0.8) +
      geom_ribbon(data = plot_df, aes(x = x, ymin = lwr_pi, ymax = upr_pi), fill = APP_COLORS["danger"], alpha = 0.12) +
      geom_ribbon(data = plot_df, aes(x = x, ymin = lwr_ci, ymax = upr_ci), fill = APP_COLORS["primary"], alpha = 0.20) +
      geom_line(data = plot_df, aes(x = x, y = fit), color = APP_COLORS["warning"], linewidth = 1.1) +
      labs(x = x_var, y = y_var)

    ggplotly(p)
  })

  output$lp_diag_plot <- renderPlot({
    req(rv$lp_model)
    fit <- rv$lp_model$fit_obj

    diag_df <- data.frame(
      resid = as.numeric(residuals(fit)),
      fitted = as.numeric(fitted(fit)),
      std_resid = as.numeric(scale(residuals(fit))),
      row.names = NULL
    )

    p1 <- ggplot(diag_df, aes(x = resid)) +
      geom_histogram(bins = 25, fill = APP_COLORS["primary"], color = "white") +
      labs(title = "Histogramme residus")

    p2 <- ggplot(diag_df, aes(sample = std_resid)) +
      stat_qq(color = APP_COLORS["dark"], alpha = 0.75) +
      stat_qq_line(color = APP_COLORS["danger"]) +
      labs(title = "QQ-plot")

    p3 <- ggplot(diag_df, aes(x = fitted, y = resid)) +
      geom_point(color = APP_COLORS["warning"], alpha = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", color = APP_COLORS["danger"]) +
      labs(title = "Residus vs ajustees", x = "Ajustees", y = "Residus")

    gridExtra::grid.arrange(p1, p2, p3, ncol = 3)
  })

  nlp_default_spec <- function(model_type, x_var, y_var, df) {
    x <- as.numeric(df[[x_var]])
    y <- as.numeric(df[[y_var]])

    x_ok <- x[is.finite(x)]
    y_ok <- y[is.finite(y)]

    x_med <- if (length(x_ok) > 0) median(x_ok, na.rm = TRUE) else 1
    y_max <- if (length(y_ok) > 0) max(y_ok, na.rm = TRUE) else 1

    if (identical(model_type, "exponentiel")) {
      list(
        formula_text = sprintf("%s ~ a * exp(b * %s)", y_var, x_var),
        start = c(a = max(y_max, 1e-3), b = 0.1)
      )
    } else if (identical(model_type, "michaelis")) {
      list(
        formula_text = sprintf("%s ~ (Vm * %s) / (K + %s)", y_var, x_var, x_var),
        start = c(Vm = max(y_max, 1e-3), K = max(x_med, 1e-3))
      )
    } else if (identical(model_type, "logistique")) {
      list(
        formula_text = sprintf("%s ~ Asym / (1 + exp((xmid - %s) / scal))", y_var, x_var),
        start = c(Asym = max(y_max, 1e-3), xmid = x_med, scal = 1)
      )
    } else if (identical(model_type, "puissance")) {
      list(
        formula_text = sprintf("%s ~ a * %s^b", y_var, x_var),
        start = c(a = max(y_max, 1e-3), b = 1)
      )
    } else {
      list(formula_text = input$nlp_formula, start = parse_named_numeric(input$nlp_start))
    }
  }

  observeEvent(list(input$nlp_model_type, input$nlp_x, input$nlp_y, rv$data), {
    req(rv$data, input$nlp_x, input$nlp_y)

    if (identical(input$nlp_model_type, "custom")) {
      return()
    }

    spec <- safe_run(
      nlp_default_spec(input$nlp_model_type, input$nlp_x, input$nlp_y, rv$data),
      fallback = NULL,
      context = "spec NLP"
    )
    req(spec)

    updateTextInput(session, "nlp_formula", value = spec$formula_text)
    start_text <- paste(sprintf("%s=%s", names(spec$start), format(spec$start, digits = 4)), collapse = ",")
    updateTextInput(session, "nlp_start", value = start_text)
  }, ignoreInit = FALSE)

  observeEvent(input$run_nlp, {
    req(rv$data, input$nlp_y, input$nlp_x)

    data_clean <- safe_run(clean_xy_data(rv$data, input$nlp_x, input$nlp_y), fallback = NULL, context = "NLP clean")
    req(data_clean)

    spec <- safe_run(
      nlp_default_spec(input$nlp_model_type, input$nlp_x, input$nlp_y, data_clean),
      fallback = NULL,
      context = "NLP spec run"
    )
    req(spec)

    formula_text <- if (identical(input$nlp_model_type, "custom")) input$nlp_formula else spec$formula_text
    formula_obj <- safe_run(as.formula(formula_text), fallback = NULL, context = "NLP formula")
    if (is.null(formula_obj)) {
      showNotification("Formule NLP invalide.", type = "error")
      return()
    }

    start_user <- parse_named_numeric(input$nlp_start)
    start_vals <- if (identical(input$nlp_model_type, "custom")) {
      start_user
    } else {
      defaults <- spec$start
      if (length(start_user) > 0) {
        defaults[names(start_user)] <- start_user
      }
      defaults
    }

    if (length(start_vals) == 0) {
      showNotification("Valeurs initiales NLP manquantes.", type = "error")
      return()
    }

    nlp_obj <- safe_run(
      fit_nlp(
        df = data_clean,
        formula_nls = formula_obj,
        start = as.list(start_vals),
        algo = input$nlp_algo,
        y_col = input$nlp_y,
        x_var = input$nlp_x
      ),
      fallback = NULL,
      context = "fit NLP"
    )

    if (is.null(nlp_obj)) {
      showNotification("Echec estimation NLP (convergence ou specification).", type = "error")
      return()
    }

    rv$nlp_model <- nlp_obj
    rv$active_model <- nlp_obj
    rv$boot <- NULL
    rv$mle <- NULL

    showNotification("Modele NLP estime.", type = "message")
  })

  output$nlp_status <- renderText({
    if (is.null(rv$nlp_model)) {
      return("Aucun modele NLP estime.")
    }

    sprintf(
      "Formule: %s | Algo: %s",
      paste(deparse(rv$nlp_model$formula), collapse = ""),
      rv$nlp_model$meta$algo %||% "nlsLM"
    )
  })

  output$nlp_coef_table <- renderDT({
    req(rv$nlp_model)
    datatable(rv$nlp_model$coef_table, options = list(pageLength = 10), rownames = FALSE)
  })

  output$nlp_vcov_table <- renderDT({
    req(rv$nlp_model)
    vc <- rv$nlp_model$vcov
    if (is.null(vc)) {
      return(datatable(data.frame(message = "vcov indisponible."), options = list(dom = "t"), rownames = FALSE))
    }

    vc_df <- data.frame(parametre = rownames(vc), as.data.frame(vc, check.names = FALSE), row.names = NULL, check.names = FALSE)
    datatable(vc_df, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  output$nlp_corr_table <- renderDT({
    req(rv$nlp_model)
    corr <- rv$nlp_model$corr
    if (is.null(corr)) {
      return(datatable(data.frame(message = "Matrice de correlation indisponible."), options = list(dom = "t"), rownames = FALSE))
    }

    corr_df <- data.frame(parametre = rownames(corr), as.data.frame(corr, check.names = FALSE), row.names = NULL, check.names = FALSE)
    datatable(corr_df, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  output$nlp_fit_plot <- renderPlotly({
    req(rv$nlp_model)

    nlp <- rv$nlp_model
    fit <- nlp$fit_obj
    df <- nlp$data
    x_var <- nlp$meta$x_var %||% input$nlp_x
    y_var <- nlp$meta$y_var %||% input$nlp_y

    df <- safe_run(clean_xy_data(df, x_var, y_var), fallback = NULL, context = "NLP plot clean")
    req(df)

    x_seq <- seq(min(df[[x_var]], na.rm = TRUE), max(df[[x_var]], na.rm = TRUE), length.out = 200)
    newdata <- data.frame(tmp = x_seq)
    names(newdata)[1] <- x_var

    pred_vars <- setdiff(all.vars(formula(fit)), y_var)
    for (v in setdiff(pred_vars, x_var)) {
      if (!(v %in% names(df))) {
        next
      }
      newdata[[v]] <- if (is.numeric(df[[v]])) median(df[[v]], na.rm = TRUE) else df[[v]][1]
    }

    fit_line <- safe_run(as.numeric(predict(fit, newdata = newdata)), fallback = rep(NA_real_, nrow(newdata)), context = "NLP fit line")
    ic <- safe_run(calc_ic_nls(fit, newdata, level = 0.95), fallback = NULL, context = "NLP IC")
    ip <- safe_run(calc_ip_nls(fit, newdata, level = 0.95), fallback = NULL, context = "NLP IP")

    plot_df <- data.frame(
      x = newdata[[x_var]],
      fit = fit_line,
      lwr_ci = if (!is.null(ic)) ic$lwr else NA_real_,
      upr_ci = if (!is.null(ic)) ic$upr else NA_real_,
      lwr_pi = if (!is.null(ip)) ip$lwr else NA_real_,
      upr_pi = if (!is.null(ip)) ip$upr else NA_real_
    )

    p <- ggplot() +
      geom_point(data = df, aes_string(x = x_var, y = y_var), color = APP_COLORS["dark"], alpha = 0.82) +
      geom_ribbon(data = plot_df, aes(x = x, ymin = lwr_pi, ymax = upr_pi), fill = APP_COLORS["danger"], alpha = 0.12) +
      geom_ribbon(data = plot_df, aes(x = x, ymin = lwr_ci, ymax = upr_ci), fill = APP_COLORS["primary"], alpha = 0.20) +
      geom_line(data = plot_df, aes(x = x, y = fit), color = APP_COLORS["warning"], linewidth = 1.1) +
      labs(x = x_var, y = y_var)

    ggplotly(p)
  })

  run_linearisation <- function(df_raw, x_var, y_var, transform_type) {
    df <- clean_xy_data(df_raw, x_var, y_var)
    d <- data.frame(x = as.numeric(df[[x_var]]), y = as.numeric(df[[y_var]]))

    if (identical(transform_type, "log_y")) {
      if (any(d$y <= 0)) stop("La transformation log(y) exige y > 0.")
      lm_fit <- lm(log(y) ~ x, data = d)
      lin_predict <- function(xnew) exp(predict(lm_fit, newdata = data.frame(x = xnew)))
      nlp_formula <- y ~ a * exp(b * x)
      start <- list(a = exp(coef(lm_fit)[1]), b = coef(lm_fit)[2])
    } else if (identical(transform_type, "loglog")) {
      if (any(d$y <= 0) || any(d$x <= 0)) stop("La transformation log-log exige x > 0 et y > 0.")
      lm_fit <- lm(log(y) ~ log(x), data = d)
      lin_predict <- function(xnew) exp(predict(lm_fit, newdata = data.frame(x = xnew)))
      start <- list(a = exp(coef(lm_fit)[1]), b = coef(lm_fit)[2])
      nlp_formula <- y ~ a * x^b
    } else if (identical(transform_type, "inv_y_x")) {
      if (any(d$y == 0)) stop("La transformation 1/y exige y != 0.")
      lm_fit <- lm(I(1 / y) ~ x, data = d)
      lin_predict <- function(xnew) {
        inv_pred <- predict(lm_fit, newdata = data.frame(x = xnew))
        1 / inv_pred
      }
      start <- list(a = coef(lm_fit)[1], b = coef(lm_fit)[2])
      nlp_formula <- y ~ 1 / (a + b * x)
    } else if (identical(transform_type, "inv_y_inv_x")) {
      if (any(d$y == 0) || any(d$x == 0)) stop("La transformation 1/y vs 1/x exige x != 0 et y != 0.")
      lm_fit <- lm(I(1 / y) ~ I(1 / x), data = d)
      lin_predict <- function(xnew) {
        inv_pred <- predict(lm_fit, newdata = data.frame(x = xnew))
        1 / inv_pred
      }
      c0 <- coef(lm_fit)[1]
      c1 <- coef(lm_fit)[2]
      vm <- if (is.finite(c0) && c0 != 0) 1 / c0 else max(d$y, na.rm = TRUE)
      k <- if (is.finite(c0) && c0 != 0) c1 / c0 else median(d$x, na.rm = TRUE)
      start <- list(Vm = vm, K = max(k, 1e-6))
      nlp_formula <- y ~ (Vm * x) / (K + x)
    } else {
      if (any(d$y < 0)) stop("La transformation sqrt(y) exige y >= 0.")
      lm_fit <- lm(sqrt(y) ~ x, data = d)
      lin_predict <- function(xnew) {
        val <- predict(lm_fit, newdata = data.frame(x = xnew))
        pmax(val, 0)^2
      }
      start <- list(a = coef(lm_fit)[1], b = coef(lm_fit)[2])
      nlp_formula <- y ~ (a + b * x)^2
    }

    nlp_fit <- safe_run(
      minpack.lm::nlsLM(
        formula = nlp_formula,
        data = d,
        start = start,
        control = minpack.lm::nls.lm.control(maxiter = 500)
      ),
      fallback = NULL,
      context = "NLP direct linearisation"
    )

    y_lin <- lin_predict(d$x)
    y_nlp <- if (!is.null(nlp_fit)) as.numeric(predict(nlp_fit, newdata = d)) else rep(NA_real_, nrow(d))

    tss <- sum((d$y - mean(d$y, na.rm = TRUE))^2, na.rm = TRUE)
    rss_lin <- sum((d$y - y_lin)^2, na.rm = TRUE)
    rss_nlp <- if (all(is.finite(y_nlp))) sum((d$y - y_nlp)^2, na.rm = TRUE) else NA_real_

    r2_lin <- if (tss > 0) 1 - rss_lin / tss else NA_real_
    r2_nlp <- if (tss > 0 && is.finite(rss_nlp)) 1 - rss_nlp / tss else NA_real_

    x_grid <- seq(min(d$x, na.rm = TRUE), max(d$x, na.rm = TRUE), length.out = 200)
    y_grid_lin <- lin_predict(x_grid)
    y_grid_nlp <- if (!is.null(nlp_fit)) as.numeric(predict(nlp_fit, newdata = data.frame(x = x_grid))) else rep(NA_real_, length(x_grid))

    list(
      lm_fit = lm_fit,
      nlp_fit = nlp_fit,
      comparison = data.frame(
        modele = c("Linearisation", "NLP direct"),
        R2 = c(r2_lin, r2_nlp),
        RSS = c(rss_lin, rss_nlp),
        row.names = NULL
      ),
      points = d,
      curves = data.frame(x = x_grid, y_lin = y_grid_lin, y_nlp = y_grid_nlp)
    )
  }

  observeEvent(input$run_linearize, {
    req(rv$data, input$lin_x, input$lin_y)

    lin_obj <- safe_run(
      run_linearisation(rv$data, input$lin_x, input$lin_y, input$lin_transform),
      fallback = NULL,
      context = "linearisation"
    )

    if (is.null(lin_obj)) {
      showNotification("Echec linearisation. Verifiez contraintes de transformation.", type = "error")
      return()
    }

    rv$lin_model <- lin_obj
    showNotification("Linearisation calculee.", type = "message")
  })

  output$lin_status <- renderText({
    if (is.null(rv$lin_model)) {
      "Aucune comparaison linearisation/NLP calculee."
    } else if (is.null(rv$lin_model$nlp_fit)) {
      "Linearisation OK. NLP direct non convergent."
    } else {
      "Linearisation et NLP direct calcules."
    }
  })

  output$lin_comp_table <- renderDT({
    req(rv$lin_model)
    datatable(rv$lin_model$comparison, options = list(dom = "t"), rownames = FALSE)
  })

  output$lin_plot <- renderPlotly({
    req(rv$lin_model)
    pnts <- rv$lin_model$points
    curv <- rv$lin_model$curves

    p <- ggplot() +
      geom_point(data = pnts, aes(x = x, y = y), color = APP_COLORS["dark"], alpha = 0.75) +
      geom_line(data = curv, aes(x = x, y = y_lin, color = "Linearisation"), linewidth = 1.1) +
      geom_line(data = curv, aes(x = x, y = y_nlp, color = "NLP direct"), linewidth = 1.1) +
      scale_color_manual(values = c("Linearisation" = APP_COLORS["primary"], "NLP direct" = APP_COLORS["danger"])) +
      labs(x = "x", y = "y", color = "Modele")

    ggplotly(p)
  })

  observeEvent(input$run_boot, {
    req(rv$active_model)

    model_obj <- rv$active_model
    R <- as.integer(input$boot_R)
    btype <- input$boot_type

    boot_res <- if (identical(model_obj$family, "linear")) {
      safe_run(bootstrap_lp(model_obj, R = R, type = btype, level = 0.95), fallback = NULL, context = "bootstrap LP")
    } else {
      safe_run(bootstrap_nlp(model_obj, R = R, type = btype, level = 0.95), fallback = NULL, context = "bootstrap NLP")
    }

    if (is.null(boot_res)) {
      showNotification("Echec bootstrap.", type = "error")
      return()
    }

    rv$boot <- boot_res
    showNotification("Bootstrap termine.", type = "message")
  })

  output$boot_status <- renderText({
    if (is.null(rv$boot)) {
      return("Aucun bootstrap execute.")
    }

    sprintf(
      "Type: %s | Iterations: %d | Taux de succes: %.1f%%",
      rv$boot$type,
      rv$boot$R,
      100 * rv$boot$success_rate
    )
  })

  output$boot_ci_table <- renderDT({
    req(rv$boot)

    an <- rv$boot$ci_analytic
    bt <- rv$boot$ci_boot

    if (nrow(an) == 0 && nrow(bt) == 0) {
      return(datatable(data.frame(message = "Aucun IC disponible."), options = list(dom = "t"), rownames = FALSE))
    }

    names(an)[names(an) %in% c("lwr", "upr")] <- c("lwr_analytique", "upr_analytique")
    names(bt)[names(bt) %in% c("lwr", "upr")] <- c("lwr_bootstrap", "upr_bootstrap")

    out <- merge(an, bt, by = "parametre", all = TRUE)
    datatable(out, options = list(pageLength = 10), rownames = FALSE)
  })

  output$boot_hist_plot <- renderPlot({
    req(rv$boot)

    samples <- rv$boot$samples
    if (is.null(samples) || nrow(samples) == 0) {
      plot.new()
      title("Echantillons bootstrap indisponibles")
      return(invisible(NULL))
    }

    long <- stack(as.data.frame(samples))
    names(long) <- c("valeur", "parametre")

    ggplot(long, aes(x = valeur)) +
      geom_histogram(fill = APP_COLORS["primary"], color = "white", bins = 30) +
      facet_wrap(~parametre, scales = "free") +
      labs(x = "Valeur", y = "Frequence")
  })

  metric_value <- function(key) {
    if (is.null(rv$active_model) || is.null(rv$active_model$metrics[[key]]) || !is.finite(rv$active_model$metrics[[key]])) {
      return("NA")
    }
    sprintf("%.4f", rv$active_model$metrics[[key]])
  }

  output$quality_r2 <- renderInfoBox({
    infoBox("R2", metric_value("R2"), icon = icon("chart-line"), color = "light-blue")
  })

  output$quality_r2a <- renderInfoBox({
    infoBox("R2 ajuste", metric_value("R2a"), icon = icon("sliders-h"), color = "aqua")
  })

  output$quality_press <- renderInfoBox({
    infoBox("PRESS", metric_value("PRESS"), icon = icon("bullseye"), color = "orange")
  })

  output$quality_r2pred <- renderInfoBox({
    infoBox("R2 prediction", metric_value("R2_pred"), icon = icon("crosshairs"), color = "red")
  })

  output$quality_diag_plot <- renderPlot({
    req(rv$active_model)

    fit <- rv$active_model$fit_obj
    resid <- as.numeric(residuals(fit))
    fitted_vals <- as.numeric(fitted(fit))
    std_resid <- as.numeric(scale(resid))

    diag_df <- data.frame(std_resid = std_resid, fitted = fitted_vals, resid = resid)

    p1 <- ggplot(diag_df, aes(sample = std_resid)) +
      stat_qq(color = APP_COLORS["dark"], alpha = 0.75) +
      stat_qq_line(color = APP_COLORS["danger"]) +
      labs(title = "QQ-plot residus standardises")

    p2 <- ggplot(diag_df, aes(x = fitted, y = resid)) +
      geom_point(color = APP_COLORS["primary"], alpha = 0.75) +
      geom_hline(yintercept = 0, linetype = "dashed", color = APP_COLORS["danger"]) +
      labs(title = "Residus vs ajustees", x = "Ajustees", y = "Residus")

    gridExtra::grid.arrange(p1, p2, ncol = 2)
  })

  output$quality_shapiro <- renderPrint({
    req(rv$active_model)

    r <- as.numeric(residuals(rv$active_model$fit_obj))
    r <- r[is.finite(r)]

    if (length(r) < 3 || length(r) > 5000) {
      cat("Shapiro-Wilk non applicable (taille residus hors [3, 5000]).")
      return(invisible(NULL))
    }

    print(shapiro.test(r))
  })

  output$quality_signif <- renderDT({
    req(rv$active_model)
    datatable(rv$active_model$coef_table, options = list(pageLength = 8), rownames = FALSE)
  })

  output$quality_anova <- renderDT({
    req(rv$active_model)

    if (!identical(rv$active_model$type, "LP")) {
      return(datatable(data.frame(message = "ANOVA disponible uniquement pour LP."), options = list(dom = "t"), rownames = FALSE))
    }

    aov_tbl <- safe_run(
      {
        a <- anova(rv$active_model$fit_obj)
        data.frame(terme = rownames(a), as.data.frame(a, check.names = FALSE), row.names = NULL, check.names = FALSE)
      },
      fallback = data.frame(message = "ANOVA indisponible."),
      context = "ANOVA LP"
    )

    datatable(aov_tbl, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  get_mle_target <- reactive({
    if (identical(input$mle_target, "lp")) {
      rv$lp_model
    } else if (identical(input$mle_target, "nlp")) {
      rv$nlp_model
    } else {
      rv$active_model
    }
  })

  observeEvent(input$run_mle, {
    model_obj <- get_mle_target()
    if (is.null(model_obj)) {
      showNotification("Aucun modele disponible pour MLE.", type = "error")
      return()
    }

    fit <- model_obj$fit_obj
    df <- model_obj$data
    y_col <- model_obj$meta$y_var %||% "y"

    if (!(y_col %in% names(df))) {
      showNotification("Variable reponse introuvable pour MLE.", type = "error")
      return()
    }

    if (identical(model_obj$family, "linear")) {
      base_coef <- coef(fit)
      design_terms <- delete.response(terms(fit))

      pred_fun <- function(newdata, theta) {
        mm <- model.matrix(design_terms, data = newdata)
        beta <- base_coef
        common <- intersect(names(beta), names(theta))
        beta[common] <- theta[common]

        beta_full <- rep(0, ncol(mm))
        names(beta_full) <- colnames(mm)

        common2 <- intersect(names(beta_full), names(beta))
        beta_full[common2] <- beta[common2]

        as.numeric(mm %*% beta_full)
      }

      start <- c(base_coef, sigma = model_obj$sigma_hat %||% sd(residuals(fit), na.rm = TRUE))
    } else {
      base_coef <- coef(fit)

      pred_fun <- function(newdata, theta) {
        theta_full <- base_coef
        common <- intersect(names(theta_full), names(theta))
        theta_full[common] <- theta[common]
        predict_nls_with_params(fit, newdata, theta_full)
      }

      start <- c(base_coef, sigma = model_obj$sigma_hat %||% sd(residuals(fit), na.rm = TRUE))
    }

    lower_in <- parse_named_numeric(input$mle_lower)
    upper_in <- parse_named_numeric(input$mle_upper)

    lower <- rep(-Inf, length(start))
    upper <- rep(Inf, length(start))
    names(lower) <- names(start)
    names(upper) <- names(start)

    lower[names(lower_in)] <- lower_in
    upper[names(upper_in)] <- upper_in

    if (!is.finite(start["sigma"]) || start["sigma"] <= 0) {
      start["sigma"] <- max(sd(df[[y_col]], na.rm = TRUE), 1e-3)
    }
    if (!is.finite(lower["sigma"]) || lower["sigma"] <= 0) {
      lower["sigma"] <- 1e-8
    }

    mle_obj <- safe_run(
      mle_fit_gaussian(
        df = df,
        pred_fun = pred_fun,
        start = start,
        method = input$mle_method,
        lower = lower,
        upper = upper,
        y_col = y_col
      ),
      fallback = NULL,
      context = "fit MLE"
    )

    if (is.null(mle_obj)) {
      showNotification("Echec estimation MLE.", type = "error")
      return()
    }

    mle_obj$model_ref <- model_obj
    rv$mle <- mle_obj

    updateSelectInput(
      session,
      "mle_profile_param",
      choices = names(rv$mle$par),
      selected = names(rv$mle$par)[1]
    )

    showNotification("MLE estime via optim().", type = "message")
  })

  output$mle_status <- renderText({
    if (is.null(rv$mle)) {
      return("Aucun resultat MLE.")
    }

    sprintf(
      "Methode: %s | Convergence: %s",
      rv$mle$method,
      rv$mle$convergence
    )
  })

  output$mle_metrics_table <- renderDT({
    req(rv$mle)

    met <- data.frame(
      metrique = c("logLik", "AIC", "BIC", "Convergence"),
      valeur = c(rv$mle$logLik, rv$mle$AIC, rv$mle$BIC, rv$mle$convergence),
      row.names = NULL,
      check.names = FALSE
    )

    datatable(met, options = list(dom = "t"), rownames = FALSE)
  })

  output$mle_params_table <- renderDT({
    req(rv$mle)

    se <- rep(NA_real_, length(rv$mle$par))
    names(se) <- names(rv$mle$par)

    if (!is.null(rv$mle$vcov)) {
      d <- suppressWarnings(sqrt(diag(rv$mle$vcov)))
      common <- intersect(names(se), names(d))
      se[common] <- d[common]
    }

    out <- data.frame(
      parametre = names(rv$mle$par),
      estimate = as.numeric(rv$mle$par),
      se = as.numeric(se),
      row.names = NULL,
      check.names = FALSE
    )

    datatable(out, options = list(pageLength = 10), rownames = FALSE)
  })

  output$mle_compare_table <- renderDT({
    req(rv$mle)

    model_ref <- rv$mle$model_ref
    ref_coef <- coef(model_ref$fit_obj)
    mle_coef <- rv$mle$par[names(ref_coef)]

    out <- data.frame(
      parametre = names(ref_coef),
      MCO = as.numeric(ref_coef),
      MLE = as.numeric(mle_coef),
      delta = as.numeric(mle_coef - ref_coef),
      row.names = NULL,
      check.names = FALSE
    )

    datatable(out, options = list(pageLength = 10), rownames = FALSE)
  })

  observeEvent(input$run_profile, {
    req(rv$mle, input$mle_profile_param)

    param <- input$mle_profile_param
    est <- rv$mle$par[[param]]

    se <- NA_real_
    if (!is.null(rv$mle$vcov) && param %in% rownames(rv$mle$vcov)) {
      se <- sqrt(abs(rv$mle$vcov[param, param]))
    }

    if (!is.finite(se) || se <= 0) {
      se <- max(abs(est) * 0.25, 0.1)
    }

    lower <- est - 3 * se
    upper <- est + 3 * se

    if (identical(param, "sigma")) {
      lower <- max(lower, 1e-8)
    }

    grid <- seq(lower, upper, length.out = as.integer(input$mle_profile_points))

    prof <- safe_run(
      profile_loglik_1d(rv$mle, param_name = param, grid = grid),
      fallback = NULL,
      context = "profil logLik"
    )

    if (is.null(prof)) {
      showNotification("Echec calcul du profil de log-vraisemblance.", type = "error")
      return()
    }

    rv$mle$profile <- prof
  })

  output$mle_profile_plot <- renderPlotly({
    req(rv$mle, rv$mle$profile)

    prof <- rv$mle$profile

    p <- ggplot(prof, aes(x = param_value, y = logLik)) +
      geom_line(color = APP_COLORS["primary"], linewidth = 1.1) +
      geom_point(color = APP_COLORS["dark"], alpha = 0.7) +
      labs(x = input$mle_profile_param, y = "log-vraisemblance")

    ggplotly(p)
  })
}
