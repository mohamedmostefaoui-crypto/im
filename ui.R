# ============================================================
# UI.R — Interface utilisateur Shiny Dashboard
# Identification de Modèles Paramétriques
# ============================================================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Identification de Modèles", titleWidth = 300),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      menuItem("Accueil", tabName = "accueil", icon = icon("home")),
      menuItem("Données", tabName = "donnees", icon = icon("database")),
      menuItem("Modèle Linéaire (LP)", tabName = "lp", icon = icon("chart-line")),
      menuItem("Modèle Non-Linéaire (NLP)", tabName = "nlp", icon = icon("bezier-curve")),
      menuItem("Linéarisation", tabName = "linearisation", icon = icon("exchange-alt")),
      menuItem("Bootstrap", tabName = "bootstrap", icon = icon("dice")),
      menuItem("Qualité du Modèle", tabName = "qualite", icon = icon("clipboard-check")),
      menuItem("Maximum de Vraisemblance", tabName = "mle", icon = icon("flask"))
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f4f6f9; }
      .box { border-top: 3px solid #3c8dbc; }
      .info-box { min-height: 90px; }
      .nav-tabs-custom > .tab-content { padding: 15px; }
      .formula-display { font-size: 16px; font-family: 'Courier New', monospace;
        background: #f8f9fa; padding: 10px; border-radius: 5px; border-left: 4px solid #3c8dbc; }
    "))),
    
    tabItems(
      # === ACCUEIL ===
      tabItem(tabName = "accueil",
        fluidRow(
          box(width = 12, title = "Bienvenue", status = "primary", solidHeader = TRUE,
            h3("Application d'Identification de Modèles Paramétriques"),
            p("Cette application permet d'explorer les concepts fondamentaux de l'identification de modèles :"),
            tags$ul(
              tags$li(tags$b("Modèles LP"), " — Régression linéaire, MCO, intervalles de confiance/prédiction"),
              tags$li(tags$b("Modèles NLP"), " — Régression non-linéaire, algorithmes itératifs"),
              tags$li(tags$b("Linéarisation"), " — Transformation et comparaison LP vs NLP"),
              tags$li(tags$b("Bootstrap"), " — Estimation non-paramétrique des incertitudes"),
              tags$li(tags$b("Qualité"), " — R²a, PRESS, R²préd, analyse des résidus"),
              tags$li(tags$b("MLE"), " — Maximum de vraisemblance, AIC, BIC")
            ),
            hr(),
            p("Commencez par charger vos données dans l'onglet ", tags$b("Données"), ".")
          )
        )
      ),
      
      # === DONNÉES ===
      tabItem(tabName = "donnees",
        fluidRow(
          box(width = 4, title = "Source des données", status = "primary", solidHeader = TRUE,
            radioButtons("data_source", "Choisir la source :",
              choices = c("Jeu intégré" = "integre", "Importer CSV" = "csv", "Saisie manuelle" = "manuel")),
            conditionalPanel("input.data_source == 'integre'",
              selectInput("dataset_choice", "Jeu de données :", choices = names(datasets_integres))
            ),
            conditionalPanel("input.data_source == 'csv'",
              fileInput("csv_file", "Fichier CSV :", accept = ".csv"),
              checkboxInput("csv_header", "En-têtes", TRUE),
              selectInput("csv_sep", "Séparateur :", choices = c("Virgule" = ",", "Point-virgule" = ";", "Tab" = "\t"))
            ),
            conditionalPanel("input.data_source == 'manuel'",
              textAreaInput("manual_x", "Valeurs x (séparées par des virgules) :", "1, 2, 3, 4, 5, 6, 7, 8, 9, 10"),
              textAreaInput("manual_y", "Valeurs y :", "2.1, 4.3, 5.8, 8.2, 9.7, 12.1, 14.5, 15.8, 18.3, 20.1")
            ),
            actionButton("load_data", "Charger les données", class = "btn-primary", icon = icon("upload"))
          ),
          box(width = 8, title = "Aperçu des données", status = "info",
            tabsetPanel(
              tabPanel("Tableau", DT::dataTableOutput("data_table")),
              tabPanel("Graphique", plotlyOutput("data_plot", height = "400px")),
              tabPanel("Statistiques", verbatimTextOutput("data_summary"))
            )
          )
        )
      ),
      
      # === MODÈLE LINÉAIRE (LP) ===
      tabItem(tabName = "lp",
        fluidRow(
          box(width = 4, title = "Paramètres LP", status = "primary", solidHeader = TRUE,
            selectInput("lp_type", "Type de modèle :",
              choices = c("Linéaire simple (y = a + bx)" = "simple",
                          "Polynomial degré 2" = "poly2",
                          "Polynomial degré 3" = "poly3",
                          "Multilinéaire (si >2 colonnes)" = "multi")),
            sliderInput("lp_conf_level", "Niveau de confiance :", min = 0.80, max = 0.99, value = 0.95, step = 0.01),
            checkboxInput("lp_show_ic", "Afficher IC de la moyenne", TRUE),
            checkboxInput("lp_show_ip", "Afficher IP nouvelle valeur", TRUE),
            actionButton("fit_lp", "Ajuster le modèle LP", class = "btn-success", icon = icon("play")),
            hr(),
            div(class = "formula-display", textOutput("lp_formula"))
          ),
          box(width = 8, title = "Résultats LP", status = "info",
            tabsetPanel(
              tabPanel("Ajustement", plotlyOutput("lp_fit_plot", height = "450px")),
              tabPanel("Résumé", verbatimTextOutput("lp_summary")),
              tabPanel("Résidus", 
                fluidRow(
                  column(6, plotOutput("lp_resid_hist")),
                  column(6, plotOutput("lp_resid_qq"))
                ),
                plotOutput("lp_resid_fitted")
              ),
              tabPanel("Coefficients", DT::dataTableOutput("lp_coef_table"))
            )
          )
        )
      ),
      
      # === MODÈLE NON-LINÉAIRE (NLP) ===
      tabItem(tabName = "nlp",
        fluidRow(
          box(width = 4, title = "Paramètres NLP", status = "primary", solidHeader = TRUE,
            selectInput("nlp_model", "Modèle prédéfini :", choices = names(modeles_nlp)),
            checkboxInput("nlp_custom", "Formule personnalisée", FALSE),
            conditionalPanel("input.nlp_custom",
              textInput("nlp_custom_formula", "Formule (ex: y ~ a*exp(b*x)) :", "y ~ a * exp(b * x)"),
              textInput("nlp_custom_start", "Valeurs initiales (ex: a=1,b=-0.5) :", "a=1, b=-0.5")
            ),
            selectInput("nlp_algo", "Algorithme :",
              choices = c("Gauss-Newton (nls)" = "gn", "Levenberg-Marquardt (nlsLM)" = "lm")),
            sliderInput("nlp_conf_level", "Niveau de confiance :", min = 0.80, max = 0.99, value = 0.95, step = 0.01),
            numericInput("nlp_maxiter", "Itérations max :", value = 200, min = 50, max = 2000),
            actionButton("fit_nlp", "Ajuster le modèle NLP", class = "btn-success", icon = icon("play")),
            hr(),
            div(class = "formula-display", textOutput("nlp_formula_display"))
          ),
          box(width = 8, title = "Résultats NLP", status = "info",
            tabsetPanel(
              tabPanel("Ajustement", plotlyOutput("nlp_fit_plot", height = "450px")),
              tabPanel("Résumé", verbatimTextOutput("nlp_summary")),
              tabPanel("Résidus",
                fluidRow(
                  column(6, plotOutput("nlp_resid_hist")),
                  column(6, plotOutput("nlp_resid_qq"))
                ),
                plotOutput("nlp_resid_fitted")
              ),
              tabPanel("Variance-Covariance", verbatimTextOutput("nlp_vcov"))
            )
          )
        )
      ),
      
      # === LINÉARISATION ===
      tabItem(tabName = "linearisation",
        fluidRow(
          box(width = 4, title = "Linéarisation", status = "primary", solidHeader = TRUE,
            selectInput("lin_transform", "Transformation :",
              choices = c("log(y) vs x" = "log_y", "log(y) vs log(x)" = "log_log",
                          "1/y vs x" = "inv_y", "1/y vs 1/x" = "inv_inv",
                          "sqrt(y) vs x" = "sqrt_y")),
            selectInput("lin_nlp_model", "Modèle NLP pour comparaison :", choices = names(modeles_nlp)),
            actionButton("fit_lin", "Comparer", class = "btn-warning", icon = icon("balance-scale"))
          ),
          box(width = 8, title = "Comparaison LP linéarisé vs NLP", status = "info",
            tabsetPanel(
              tabPanel("Graphiques",
                fluidRow(
                  column(6, plotOutput("lin_transformed_plot")),
                  column(6, plotOutput("lin_original_plot"))
                )
              ),
              tabPanel("Comparaison", DT::dataTableOutput("lin_comparison_table")),
              tabPanel("Résumés", 
                fluidRow(
                  column(6, h4("Modèle linéarisé"), verbatimTextOutput("lin_lm_summary")),
                  column(6, h4("Modèle NLP direct"), verbatimTextOutput("lin_nlp_summary"))
                )
              )
            )
          )
        )
      ),
      
      # === BOOTSTRAP ===
      tabItem(tabName = "bootstrap",
        fluidRow(
          box(width = 4, title = "Paramètres Bootstrap", status = "primary", solidHeader = TRUE,
            selectInput("boot_model_type", "Type de modèle :", choices = c("LP" = "lp", "NLP" = "nlp")),
            conditionalPanel("input.boot_model_type == 'nlp'",
              selectInput("boot_nlp_model", "Modèle NLP :", choices = names(modeles_nlp))
            ),
            selectInput("boot_type", "Type de Bootstrap :",
              choices = c("Non-paramétrique (résidus)" = "residual", "Paramétrique" = "parametric")),
            sliderInput("boot_n", "Nombre d'itérations :", min = 100, max = 5000, value = 1000, step = 100),
            sliderInput("boot_conf", "Niveau de confiance :", min = 0.80, max = 0.99, value = 0.95, step = 0.01),
            actionButton("run_boot", "Lancer Bootstrap", class = "btn-danger", icon = icon("random")),
            hr(),
            verbatimTextOutput("boot_progress")
          ),
          box(width = 8, title = "Résultats Bootstrap", status = "info",
            tabsetPanel(
              tabPanel("Distributions", plotOutput("boot_hist_plot", height = "500px")),
              tabPanel("IC Bootstrap", DT::dataTableOutput("boot_ci_table")),
              tabPanel("Comparaison IC", DT::dataTableOutput("boot_compare_table"))
            )
          )
        )
      ),
      
      # === QUALITÉ DU MODÈLE ===
      tabItem(tabName = "qualite",
        fluidRow(
          box(width = 12, title = "Indicateurs de qualité", status = "primary", solidHeader = TRUE,
            actionButton("calc_quality", "Calculer les indicateurs", class = "btn-info", icon = icon("calculator")),
            hr(),
            fluidRow(
              infoBoxOutput("r2_box", width = 3),
              infoBoxOutput("r2a_box", width = 3),
              infoBoxOutput("press_box", width = 3),
              infoBoxOutput("r2pred_box", width = 3)
            )
          )
        ),
        fluidRow(
          box(width = 6, title = "Diagnostics des résidus", status = "info",
            plotOutput("qual_resid_plot", height = "400px"),
            hr(),
            verbatimTextOutput("qual_shapiro")
          ),
          box(width = 6, title = "Significativité des paramètres", status = "info",
            DT::dataTableOutput("qual_signif_table"),
            hr(),
            verbatimTextOutput("qual_anova")
          )
        )
      ),
      
      # === MAXIMUM DE VRAISEMBLANCE (MLE) ===
      tabItem(tabName = "mle",
        fluidRow(
          box(width = 4, title = "Paramètres MLE", status = "primary", solidHeader = TRUE,
            selectInput("mle_model_type", "Type de modèle :", choices = c("LP" = "lp", "NLP" = "nlp")),
            conditionalPanel("input.mle_model_type == 'nlp'",
              selectInput("mle_nlp_model", "Modèle NLP :", choices = names(modeles_nlp))
            ),
            selectInput("mle_method", "Méthode d'optimisation :",
              choices = c("Nelder-Mead", "BFGS", "L-BFGS-B", "CG")),
            actionButton("fit_mle", "Estimer par MLE", class = "btn-primary", icon = icon("cogs"))
          ),
          box(width = 8, title = "Résultats MLE", status = "info",
            tabsetPanel(
              tabPanel("Résumé MLE", verbatimTextOutput("mle_summary")),
              tabPanel("Comparaison MCO vs MLE", DT::dataTableOutput("mle_compare_table")),
              tabPanel("Vraisemblance",
                plotOutput("mle_loglik_plot", height = "400px"),
                fluidRow(
                  infoBoxOutput("mle_aic_box", width = 4),
                  infoBoxOutput("mle_bic_box", width = 4),
                  infoBoxOutput("mle_loglik_box", width = 4)
                )
              )
            )
          )
        )
      )
    ) # fin tabItems
  ) # fin dashboardBody
) # fin dashboardPage
