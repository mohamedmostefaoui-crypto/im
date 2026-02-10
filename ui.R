ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Identification des modeles"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("1. Donnees", tabName = "donnees", icon = icon("database")),
      menuItem("2. Modele Lineaire (LP)", tabName = "lp", icon = icon("chart-line")),
      menuItem("3. Modele Non-Lineaire (NLP)", tabName = "nlp", icon = icon("bezier-curve")),
      menuItem("4. Linearisation", tabName = "linearisation", icon = icon("sliders-h")),
      menuItem("5. Bootstrap", tabName = "bootstrap", icon = icon("random")),
      menuItem("6. Qualite du modele", tabName = "qualite", icon = icon("check-circle")),
      menuItem("7. Maximum de Vraisemblance (MLE)", tabName = "mle", icon = icon("calculator"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(
        "
        .skin-blue .main-header .logo { background-color: #2c3e50 !important; color: #ffffff !important; }
        .skin-blue .main-header .navbar { background-color: #3498db !important; }
        .skin-blue .main-sidebar { background-color: #2c3e50 !important; }
        .skin-blue .sidebar-menu > li.active > a,
        .skin-blue .sidebar-menu > li:hover > a { border-left-color: #e67e22 !important; background: #34495e !important; }
        .box.box-primary { border-top-color: #3498db !important; }
        .box.box-danger { border-top-color: #e74c3c !important; }
        .box.box-warning { border-top-color: #e67e22 !important; }
        .small-box.bg-primary-custom { background-color: #3498db !important; color: #fff !important; }
        .small-box.bg-danger-custom { background-color: #e74c3c !important; color: #fff !important; }
        .small-box.bg-dark-custom { background-color: #2c3e50 !important; color: #fff !important; }
        .small-box.bg-warning-custom { background-color: #e67e22 !important; color: #fff !important; }
        "
      ))
    ),
    tabItems(
      tabItem(
        tabName = "donnees",
        fluidRow(
          box(
            width = 4,
            title = "Source des donnees",
            status = "primary",
            solidHeader = TRUE,
            selectInput(
              "data_source",
              "Source",
              choices = c("Jeu integre", "CSV", "Saisie manuelle"),
              selected = "Jeu integre"
            ),
            conditionalPanel(
              "input.data_source == 'Jeu integre'",
              selectInput(
                "builtin_dataset",
                "Jeu integre",
                choices = c(
                  "Puromycin" = "puromycin",
                  "Exponentielle" = "exponentielle",
                  "Polynomiale" = "polynomiale",
                  "Lineaire" = "lineaire"
                ),
                selected = "lineaire"
              ),
              numericInput("builtin_n", "Nombre de points", value = 60, min = 20, max = 1000, step = 10),
              numericInput("builtin_seed", "Seed", value = 123, min = 1, max = 999999, step = 1),
              numericInput("builtin_noise", "Bruit (ecart-type)", value = 1, min = 0, max = 20, step = 0.1)
            ),
            conditionalPanel(
              "input.data_source == 'CSV'",
              fileInput("csv_file", "Importer CSV"),
              checkboxInput("csv_header", "En-tete", value = TRUE),
              selectInput("csv_sep", "Separateur", choices = c("Virgule" = ",", "Point-virgule" = ";", "Tabulation" = "\t"), selected = ","),
              selectInput("csv_dec", "Separateur decimal", choices = c("Point" = ".", "Virgule" = ","), selected = ".")
            ),
            conditionalPanel(
              "input.data_source == 'Saisie manuelle'",
              textAreaInput(
                "manual_text",
                "Saisir CSV (avec en-tete x,y,z)",
                value = "x,y,z\n1,2,0\n2,4,1\n3,5,0",
                rows = 8,
                resize = "vertical"
              ),
              actionButton("apply_manual", "Charger la saisie", icon = icon("upload"))
            ),
            hr(),
            uiOutput("var_selectors")
          ),
          box(
            width = 8,
            title = "Apercu des donnees",
            status = "primary",
            solidHeader = TRUE,
            textOutput("data_source_note"),
            DTOutput("data_table"),
            br(),
            plotlyOutput("data_plot", height = "330px")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Statistiques descriptives",
            status = "warning",
            solidHeader = TRUE,
            DTOutput("desc_stats")
          )
        )
      ),
      tabItem(
        tabName = "lp",
        fluidRow(
          box(
            width = 3,
            title = "Parametres LP",
            status = "primary",
            solidHeader = TRUE,
            selectInput(
              "lp_type",
              "Type de modele",
              choices = c(
                "Simple" = "simple",
                "Polynomiale degre 2" = "poly2",
                "Polynomiale degre 3" = "poly3",
                "Multiple" = "multi"
              ),
              selected = "simple"
            ),
            selectInput("lp_y", "Variable reponse", choices = NULL),
            selectInput("lp_x", "Predicteur principal", choices = NULL),
            conditionalPanel(
              "input.lp_type == 'multi'",
              selectizeInput("lp_multi_x", "Predicteurs additionnels", choices = NULL, multiple = TRUE)
            ),
            actionButton("run_lp", "Estimer MCO (lm)", icon = icon("play"), class = "btn-primary")
          ),
          box(
            width = 9,
            title = "Coefficients et metriques",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("lp_coef_table"),
            br(),
            DTOutput("lp_fit_stats")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Ajustement LP avec intervalle de confiance et prediction",
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("lp_fit_plot", height = "360px")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Diagnostics des residus (histogramme, QQ, residus vs ajustees)",
            status = "danger",
            solidHeader = TRUE,
            plotOutput("lp_diag_plot", height = "330px")
          )
        )
      ),
      tabItem(
        tabName = "nlp",
        fluidRow(
          box(
            width = 3,
            title = "Parametres NLP",
            status = "primary",
            solidHeader = TRUE,
            selectInput(
              "nlp_model_type",
              "Modele",
              choices = c(
                "Exponentiel" = "exponentiel",
                "Michaelis-Menten" = "michaelis",
                "Logistique" = "logistique",
                "Puissance" = "puissance",
                "Formule personnalisee" = "custom"
              ),
              selected = "exponentiel"
            ),
            selectInput("nlp_y", "Variable reponse", choices = NULL),
            selectInput("nlp_x", "Predicteur principal", choices = NULL),
            textInput("nlp_formula", "Formule NLP", value = "y ~ a * exp(b * x)"),
            textInput("nlp_start", "Valeurs initiales (a=1,b=0.1)", value = "a=1,b=0.1"),
            selectInput("nlp_algo", "Algorithme", choices = c("nls", "nlsLM"), selected = "nlsLM"),
            actionButton("run_nlp", "Estimer NLP", icon = icon("play"), class = "btn-primary"),
            br(), br(),
            textOutput("nlp_status")
          ),
          box(
            width = 9,
            title = "Resultats NLP",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("nlp_coef_table"),
            br(),
            fluidRow(
              column(6, DTOutput("nlp_vcov_table")),
              column(6, DTOutput("nlp_corr_table"))
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Ajustement NLP avec IC/IP (methode delta)",
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("nlp_fit_plot", height = "360px")
          )
        )
      ),
      tabItem(
        tabName = "linearisation",
        fluidRow(
          box(
            width = 3,
            title = "Parametres de linearisation",
            status = "primary",
            solidHeader = TRUE,
            selectInput("lin_y", "Variable reponse", choices = NULL),
            selectInput("lin_x", "Predicteur", choices = NULL),
            selectInput(
              "lin_transform",
              "Transformation",
              choices = c(
                "log(y)" = "log_y",
                "log-log" = "loglog",
                "1/y vs x" = "inv_y_x",
                "1/y vs 1/x" = "inv_y_inv_x",
                "sqrt(y)" = "sqrt_y"
              ),
              selected = "log_y"
            ),
            actionButton("run_linearize", "Comparer linearisation / NLP", icon = icon("balance-scale"), class = "btn-primary"),
            br(), br(),
            textOutput("lin_status")
          ),
          box(
            width = 9,
            title = "Comparaison des performances",
            status = "warning",
            solidHeader = TRUE,
            DTOutput("lin_comp_table"),
            br(),
            plotlyOutput("lin_plot", height = "360px")
          )
        )
      ),
      tabItem(
        tabName = "bootstrap",
        fluidRow(
          box(
            width = 3,
            title = "Parametres bootstrap",
            status = "primary",
            solidHeader = TRUE,
            selectInput(
              "boot_type",
              "Type",
              choices = c("Non-parametrique (residus)" = "nonparam", "Parametrique" = "param"),
              selected = "nonparam"
            ),
            sliderInput("boot_R", "Iterations", min = 100, max = 5000, value = 1000, step = 100),
            actionButton("run_boot", "Lancer bootstrap", icon = icon("sync"), class = "btn-primary"),
            br(), br(),
            textOutput("boot_status")
          ),
          box(
            width = 9,
            title = "IC analytiques vs IC bootstrap",
            status = "warning",
            solidHeader = TRUE,
            DTOutput("boot_ci_table"),
            br(),
            plotOutput("boot_hist_plot", height = "360px")
          )
        )
      ),
      tabItem(
        tabName = "qualite",
        fluidRow(
          infoBoxOutput("quality_r2", width = 3),
          infoBoxOutput("quality_r2a", width = 3),
          infoBoxOutput("quality_press", width = 3),
          infoBoxOutput("quality_r2pred", width = 3)
        ),
        fluidRow(
          box(
            width = 12,
            title = "Diagnostics des residus standardises",
            status = "danger",
            solidHeader = TRUE,
            plotOutput("quality_diag_plot", height = "330px")
          )
        ),
        fluidRow(
          box(
            width = 4,
            title = "Test Shapiro-Wilk",
            status = "primary",
            solidHeader = TRUE,
            verbatimTextOutput("quality_shapiro")
          ),
          box(
            width = 4,
            title = "Significativite des parametres",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("quality_signif")
          ),
          box(
            width = 4,
            title = "ANOVA (LP)",
            status = "warning",
            solidHeader = TRUE,
            DTOutput("quality_anova")
          )
        )
      ),
      tabItem(
        tabName = "mle",
        fluidRow(
          box(
            width = 3,
            title = "Parametres MLE",
            status = "primary",
            solidHeader = TRUE,
            selectInput(
              "mle_target",
              "Modele cible",
              choices = c("Modele actif" = "active", "LP" = "lp", "NLP" = "nlp"),
              selected = "active"
            ),
            selectInput(
              "mle_method",
              "optim() methode",
              choices = c("Nelder-Mead", "BFGS", "L-BFGS-B", "CG"),
              selected = "BFGS"
            ),
            textInput("mle_lower", "Bornes inferieures (a=0,sigma=0.01)", value = "sigma=1e-6"),
            textInput("mle_upper", "Bornes superieures (optionnel)", value = ""),
            actionButton("run_mle", "Estimer MLE", icon = icon("play"), class = "btn-primary"),
            hr(),
            selectInput("mle_profile_param", "Parametre du profil", choices = NULL),
            sliderInput("mle_profile_points", "Points du profil", min = 20, max = 200, value = 60, step = 10),
            actionButton("run_profile", "Tracer profil logLik", icon = icon("chart-area"), class = "btn-warning"),
            br(), br(),
            textOutput("mle_status")
          ),
          box(
            width = 9,
            title = "Resultats MLE",
            status = "warning",
            solidHeader = TRUE,
            DTOutput("mle_metrics_table"),
            br(),
            DTOutput("mle_params_table"),
            br(),
            DTOutput("mle_compare_table"),
            br(),
            plotlyOutput("mle_profile_plot", height = "320px")
          )
        )
      )
    )
  )
)
