#' Data Analysis Module UI
#'
#' @description Shiny module UI for the Data Analysis tab. Provides tools for
#'   descriptive statistics, linear regression, basic cost-utility analysis
#'   (ICER), and probabilistic sensitivity analysis (PSA) with CEAC.
#'   A single shared dataset upload at the top feeds all analysis tabs.
#'
#' @param id Character. The Shiny module namespace ID.
#'
#' @return A Shiny \code{tabPanel} UI element.
#' @export
Analyses_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tabPanel(
    "6. Data Analysis",

    shiny::fluidRow(
      shiny::column(12,
        shiny::wellPanel(
          style = "background-color: #f0f4f8; border: 1px solid #b0c4de;",
          shiny::h4("Upload Dataset"),
          shiny::p("Upload your dataset once here. It will be available across all analysis tabs below.",
            style = "color: #555; margin-bottom: 8px;"),
          shiny::fluidRow(
            shiny::column(6,
              shiny::fileInput(ns("shared_file"),
                "Choose a .csv, .xlsx, or .sav file",
                accept = c(".csv", ".xlsx", ".sav"),
                width = "100%")
            ),
            shiny::column(6,
              shiny::br(),
              shiny::uiOutput(ns("upload_status"))
            )
          )
        )
      )
    ),

    shiny::navlistPanel(widths = c(2, 10),

      shiny::tabPanel(
        "Descriptive statistics",
        shiny::sidebarPanel(
          shiny::uiOutput(ns("desc_upload_note")),
          shiny::tags$hr(),
          shiny::selectInput(ns("xy_vars"), "Select Variables (X)", choices = NULL, multiple = TRUE),
          shiny::actionButton(ns("run_descriptive_analysis"), "Run Analysis"),
          width = 4
        ),
        shiny::mainPanel(
          shiny::h6("Descriptive Output"),
          shiny::verbatimTextOutput(ns("descriptive_output")),
          shiny::plotOutput(ns("histogram_plot")),
          width = 8
        )
      ),

      shiny::tabPanel(
        "Linear regression analysis",
        shiny::sidebarPanel(
          shiny::uiOutput(ns("reg_upload_note")),
          shiny::tags$hr(),
          shiny::selectInput(ns("y_var"),  "Select Dependent Variable (Y)",    choices = NULL),
          shiny::selectInput(ns("x_vars"), "Select Independent Variables (X)", choices = NULL, multiple = TRUE),
          shiny::actionButton(ns("run_analysis"), "Run Analysis"),
          width = 4
        ),
        shiny::mainPanel(
          shiny::h6("Regression output"),
          shiny::verbatimTextOutput(ns("regression_output")),
          width = 8
        )
      ),

      shiny::tabPanel(
        "Basic Cost Utility Analysis",
        shiny::sidebarPanel(
          shiny::uiOutput(ns("cua_upload_note")),
          shiny::uiOutput(ns("var_select_ui")),
          shiny::actionButton(ns("run_cua_data"), "Run CUA from Data"),
          width = 4
        ),
        shiny::mainPanel(
          shiny::h6("CUA Results from Dataset"),
          shiny::verbatimTextOutput(ns("cua_data_output")),
          shiny::uiOutput(ns("text1_cua")),
          shiny::plotOutput(ns("quadrant_plot")),
          width = 8
        )
      ),

      shiny::tabPanel(
        title = "Probabilistic Sensitivity Analysis",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::h4("PSA Settings"),
            shiny::h5("Treatment Group"),
            shiny::numericInput(ns("mean_cost_trt"),  "Mean Cost",    value = 1000, min = 0),
            shiny::numericInput(ns("sd_cost_trt"),    "Cost SD",      value = 200,  min = 1),
            shiny::numericInput(ns("mean_util_trt"),  "Mean Utility", value = 0.7,  min = 0, max = 1, step = 0.01),
            shiny::numericInput(ns("sd_util_trt"),    "Utility SD",   value = 0.1,  min = 0.01, max = 0.5, step = 0.01),
            shiny::hr(),
            shiny::h5("Control Group"),
            shiny::numericInput(ns("mean_cost_ctrl"), "Mean Cost",    value = 850,  min = 0),
            shiny::numericInput(ns("sd_cost_ctrl"),   "Cost SD",      value = 200,  min = 1),
            shiny::numericInput(ns("mean_util_ctrl"), "Mean Utility", value = 0.6,  min = 0, max = 1, step = 0.01),
            shiny::numericInput(ns("sd_util_ctrl"),   "Utility SD",   value = 0.1,  min = 0.01, max = 0.5, step = 0.01),
            shiny::hr(),
            shiny::numericInput(ns("n_sim"), "Number of PSA Simulations", value = 5000, min = 100, max = 50000, step = 500),
            shiny::sliderInput(ns("wtp"), "Willingness-to-Pay (per QALY)", min = 0, max = 100000, value = 50000, step = 5000, pre = "$"),
            width = 3
          ),
          shiny::mainPanel(
            shiny::tabsetPanel(
              shiny::tabPanel("Summary",
                shiny::h4("Summary Statistics"),
                shiny::tableOutput(ns("summary_table"))
              ),
              shiny::tabPanel("ICUR Distribution",
                shiny::h4("ICUR Histogram"),
                shiny::plotOutput(ns("icur_dist_plot"))
              ),
              shiny::tabPanel("CEAC",
                shiny::h4("Cost-Effectiveness Acceptability Curve"),
                shiny::plotOutput(ns("ceac_plot"))
              )
            )
          )
        )
      )
    )
  )
}


#' Data Analysis Module Server
#'
#' @description Shiny module server for the Data Analysis tab. All analysis
#'   tabs share a single uploaded dataset. Covers descriptive statistics,
#'   linear regression, cost-utility analysis, and probabilistic sensitivity
#'   analysis.
#'
#' @param id Character. The Shiny module namespace ID.
#'
#' @return A Shiny module server function (called for its side effects).
#' @export
Analyses_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------------------------------------------------------------------ #
    #   Shared dataset                                                    #
    # ------------------------------------------------------------------ #

    uploaded_data <- shiny::reactive({
      shiny::req(input$shared_file)
      ext <- tools::file_ext(input$shared_file$name)
      if (ext == "csv") {
        utils::read.csv(input$shared_file$datapath)
      } else if (ext == "xlsx") {
        readxl::read_excel(input$shared_file$datapath)
      } else if (ext == "sav") {
        haven::read_sav(input$shared_file$datapath)
      } else {
        stop("Invalid file; please upload a .csv, .xlsx, or .sav file")
      }
    })

    output$upload_status <- shiny::renderUI({
      if (!is.null(input$shared_file)) {
        df <- tryCatch(uploaded_data(), error = function(e) NULL)
        if (!is.null(df)) {
          shiny::tags$span(
            style = "color: green; font-weight: bold;",
            paste0("'", input$shared_file$name, "' loaded -- ",
                   nrow(df), " rows x ", ncol(df), " columns")
          )
        } else {
          shiny::tags$span(style = "color: red;", "Failed to read file.")
        }
      } else {
        shiny::tags$span(style = "color: #888;", "No file uploaded yet.")
      }
    })

    upload_note_ui <- function() {
      shiny::renderUI({
        if (is.null(input$shared_file)) {
          shiny::tags$div(style = "color: orange; font-weight: bold;",
            "Please upload a dataset at the top of the page first.")
        } else {
          shiny::tags$div(style = "color: green;",
            paste0("Using: ", input$shared_file$name))
        }
      })
    }

    output$desc_upload_note <- upload_note_ui()
    output$reg_upload_note  <- upload_note_ui()
    output$cua_upload_note  <- upload_note_ui()

    # ------------------------------------------------------------------ #
    #   Descriptive statistics                                            #
    # ------------------------------------------------------------------ #

    shiny::observeEvent(uploaded_data(), {
      df           <- uploaded_data()
      numeric_cols <- names(df)[sapply(df, is.numeric) & names(df) != "id"]
      shiny::updateSelectInput(session, "xy_vars", choices = numeric_cols)
    })

    shiny::observeEvent(input$run_descriptive_analysis, {
      shiny::req(uploaded_data(), input$xy_vars)
      df       <- uploaded_data()
      selected <- df[, input$xy_vars, drop = FALSE]
      output$descriptive_output <- shiny::renderPrint({ summary(selected) })
    })

    output$histogram_plot <- shiny::renderPlot({
      shiny::req(uploaded_data(), input$xy_vars)
      df <- uploaded_data()
      n  <- length(input$xy_vars)

      # Save and restore graphical parameters on exit -- required by CRAN
      oldpar <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(oldpar))

      rows <- ceiling(n / 3)
      cols <- ifelse(n >= 3, 3, n)
      graphics::par(mfrow = c(rows, cols), mar = c(4, 4, 2, 1))

      for (var in input$xy_vars) {
        graphics::hist(df[[var]],
          main   = paste("Histogram of", var),
          xlab   = var,
          col    = "skyblue",
          border = "white")
      }
    })

    # ------------------------------------------------------------------ #
    #   Linear regression                                                 #
    # ------------------------------------------------------------------ #

    shiny::observeEvent(uploaded_data(), {
      df           <- uploaded_data()
      numeric_cols <- names(df)[sapply(df, is.numeric) & names(df) != "id"]
      shiny::updateSelectInput(session, "y_var",  choices = numeric_cols)
      shiny::updateSelectInput(session, "x_vars", choices = numeric_cols)
    })

    shiny::observeEvent(input$run_analysis, {
      shiny::req(uploaded_data(), input$y_var, input$x_vars)
      df          <- uploaded_data()
      formula_str <- paste(input$y_var, "~", paste(input$x_vars, collapse = " + "))
      model       <- stats::lm(stats::as.formula(formula_str), data = df)
      output$regression_output <- shiny::renderPrint({ summary(model) })
    })

    output$regression_output <- shiny::renderPrint({
      if (is.null(input$shared_file)) return("Please upload a file at the top of the page to begin.")
      if (is.null(input$y_var) || is.null(input$x_vars)) return("Please select variables and click 'Run Analysis'.")
      return(NULL)
    })

    # ------------------------------------------------------------------ #
    #   Basic Cost Utility Analysis                                       #
    # ------------------------------------------------------------------ #

    output$var_select_ui <- shiny::renderUI({
      shiny::req(uploaded_data())
      df   <- uploaded_data()
      cols <- names(df)
      shiny::tagList(
        shiny::tags$hr(),
        shiny::selectInput(ns("cost"),      "Select 'Cost' Variable",            choices = cols),
        shiny::selectInput(ns("utility"),   "Select 'Utility' Variable",         choices = cols),
        shiny::selectInput(ns("treatment"), "Select 'Treatment' Group Variable", choices = cols)
      )
    })

    shiny::observeEvent(input$run_cua_data, {
      shiny::req(uploaded_data(), input$cost, input$utility, input$treatment)
      df <- uploaded_data()

      if (!all(df[[input$treatment]] %in% c(0, 1))) {
        output$cua_data_output <- shiny::renderPrint({
          cat("Error: Treatment variable must be coded as 0 (control) and 1 (treatment).\n")
        })
        return()
      }

      df_treat   <- df[df[[input$treatment]] == 1, ]
      df_control <- df[df[[input$treatment]] == 0, ]

      cost_treat         <- mean(df_treat[[input$cost]],        na.rm = TRUE)
      cost_treat_sd      <- stats::sd(df_treat[[input$cost]],   na.rm = TRUE)
      cost_control       <- mean(df_control[[input$cost]],      na.rm = TRUE)
      cost_control_sd    <- stats::sd(df_control[[input$cost]], na.rm = TRUE)
      utility_treat      <- mean(df_treat[[input$utility]],        na.rm = TRUE)
      utility_treat_sd   <- stats::sd(df_treat[[input$utility]],   na.rm = TRUE)
      utility_control    <- mean(df_control[[input$utility]],      na.rm = TRUE)
      utility_control_sd <- stats::sd(df_control[[input$utility]], na.rm = TRUE)

      incremental_qaly <- utility_treat - utility_control
      incremental_cost <- cost_treat    - cost_control
      ICER             <- incremental_cost / incremental_qaly

      output$cua_data_output <- shiny::renderPrint({
        cat("Cost in the Treatment\n")
        cat("Mean:", cost_treat,    "\n")
        cat("SD:  ", cost_treat_sd, "\n")
        cat("Cost in the Control\n")
        cat("Mean:", cost_control,    "\n")
        cat("SD:  ", cost_control_sd, "\n")
        cat("Incremental Cost:", incremental_cost, "\n\n")
        cat("Utility in the Treatment\n")
        cat("Mean:", utility_treat,    "\n")
        cat("SD:  ", utility_treat_sd, "\n")
        cat("Utility in the Control\n")
        cat("Mean:", utility_control,    "\n")
        cat("SD:  ", utility_control_sd, "\n")
        cat("Incremental QALYs:", incremental_qaly, "\n\n")
        cat("Incremental Cost-Effectiveness Ratio (ICER):", ICER, "Dollar per QALY.\n")
      })

      output$quadrant_plot <- shiny::renderPlot({
        # Save and restore graphical parameters on exit -- required by CRAN
        oldpar <- graphics::par(no.readonly = TRUE)
        on.exit(graphics::par(oldpar))

        graphics::plot(0, 0, xlim = c(-500, 500), ylim = c(-1, 1),
          xlab = "Incremental Cost", ylab = "Incremental QALYs",
          main = "Cost-Effectiveness Plane", type = "n")
        graphics::abline(h = 0, v = 0, col = "gray", lty = 2)
        graphics::points(incremental_cost, incremental_qaly, pch = 9, col = "blue")
        graphics::text(-400,  0.8, "Southwest (--)", col = "darkgreen")
        graphics::text( 400,  0.8, "Northwest (-+)", col = "red")
        graphics::text(-400, -0.8, "Southeast (+-)", col = "blue")
        graphics::text( 400, -0.8, "Northeast (++)", col = "orange")
      })

      output$text1_cua <- shiny::renderUI({
        shiny::HTML(paste0(
          "<br><br>",
          "<h4 style='color: green;'>ICER = $", round(ICER, 2), " per QALY</h4>",
          "NOTE: The ICER of $", round(ICER),
          " per QALY tells us the cost of gaining one additional QALY through the treatment compared to control.",
          "<br><br>",
          "1. Northeast Quadrant (++): More effective and more costly. ICER judgment required.<br>",
          "2. Southeast Quadrant (+-): More effective and less costly (dominant).<br>",
          "3. Northwest Quadrant (-+): Less effective and more costly (dominated).<br>",
          "4. Southwest Quadrant (--): Less effective and less costly (trade-off).",
          "<br><br>",
          "In Singapore, an ICER of up to S$75,000/QALY appears to be acceptable. ",
          "Reference: Viswambaram A et al. Value Health Reg Issues. 2020;22:S72. ",
          "doi: 10.1016/j.vhri.2020.07.378.<br>"
        ))
      })
    })

    # ------------------------------------------------------------------ #
    #   Probabilistic Sensitivity Analysis                                #
    # ------------------------------------------------------------------ #

    validate_inputs <- shiny::reactive({
      shiny::validate(
        shiny::need(input$mean_cost_trt >= 0,  "Treatment cost mean must be >= 0"),
        shiny::need(input$sd_cost_trt > 0,     "Treatment cost SD must be > 0"),
        shiny::need(input$mean_util_trt > 0 && input$mean_util_trt < 1,
                    "Treatment utility mean must be in (0,1)"),
        shiny::need(input$sd_util_trt > 0 && input$sd_util_trt < 0.5,
                    "Treatment utility SD must be > 0 and < 0.5"),
        shiny::need(input$mean_cost_ctrl >= 0, "Control cost mean must be >= 0"),
        shiny::need(input$sd_cost_ctrl > 0,    "Control cost SD must be > 0"),
        shiny::need(input$mean_util_ctrl > 0 && input$mean_util_ctrl < 1,
                    "Control utility mean must be in (0,1)"),
        shiny::need(input$sd_util_ctrl > 0 && input$sd_util_ctrl < 0.5,
                    "Control utility SD must be > 0 and < 0.5"),
        shiny::need(input$n_sim >= 100, "At least 100 simulations required"),
        shiny::need(input$wtp >= 0,     "WTP threshold must be >= 0")
      )
      TRUE
    })

    psa_sim <- shiny::reactive({
      validate_inputs()
      n_sim     <- input$n_sim
      cost_trt  <- stats::rnorm(n_sim, mean = input$mean_cost_trt,  sd = input$sd_cost_trt)
      cost_ctrl <- stats::rnorm(n_sim, mean = input$mean_cost_ctrl, sd = input$sd_cost_ctrl)

      util_moments <- function(mean, sd) {
        var   <- sd^2
        alpha <- ((1 - mean) / var - 1 / mean) * mean^2
        beta  <- alpha * (1 / mean - 1)
        alpha <- ifelse(alpha <= 0, 1, alpha)
        beta  <- ifelse(beta  <= 0, 1, beta)
        c(alpha = alpha, beta = beta)
      }
      ab_trt    <- util_moments(input$mean_util_trt,  input$sd_util_trt)
      ab_ctrl   <- util_moments(input$mean_util_ctrl, input$sd_util_ctrl)
      util_trt  <- stats::rbeta(n_sim, ab_trt[1],  ab_trt[2])
      util_ctrl <- stats::rbeta(n_sim, ab_ctrl[1], ab_ctrl[2])

      d_cost <- cost_trt  - cost_ctrl
      d_qaly <- util_trt  - util_ctrl
      icur   <- d_cost / d_qaly
      icur[is.infinite(icur) | is.nan(icur)] <- NA

      tibble::tibble(cost_trt, cost_ctrl, util_trt, util_ctrl, d_cost, d_qaly, icur)
    })

    output$summary_table <- shiny::renderTable({
      df  <- psa_sim()
      wtp <- input$wtp
      tibble::tibble(
        Metric = c("Incremental Cost (DeltaCost)", "Incremental QALY (DeltaQALY)", "ICUR"),
        Mean   = c(mean(df$d_cost, na.rm = TRUE), mean(df$d_qaly, na.rm = TRUE), mean(df$icur, na.rm = TRUE)),
        SD     = c(stats::sd(df$d_cost, na.rm = TRUE), stats::sd(df$d_qaly, na.rm = TRUE), stats::sd(df$icur, na.rm = TRUE)),
        `2.5%`  = c(stats::quantile(df$d_cost, 0.025, na.rm = TRUE), stats::quantile(df$d_qaly, 0.025, na.rm = TRUE), stats::quantile(df$icur, 0.025, na.rm = TRUE)),
        `97.5%` = c(stats::quantile(df$d_cost, 0.975, na.rm = TRUE), stats::quantile(df$d_qaly, 0.975, na.rm = TRUE), stats::quantile(df$icur, 0.975, na.rm = TRUE)),
        `Prob Cost-effective (WTP)` = c(NA, NA, mean(df$d_cost < wtp * df$d_qaly, na.rm = TRUE))
      )
    }, digits = 3)

    output$icur_dist_plot <- shiny::renderPlot({
      df <- psa_sim()
      ggplot2::ggplot(df, ggplot2::aes(x = icur)) +
        ggplot2::geom_histogram(bins = 50, fill = "#81C784", color = "black", alpha = 0.8, na.rm = TRUE) +
        ggplot2::labs(
          x     = "Incremental Cost-Effectiveness Ratio (ICER)",
          y     = "Frequency",
          title = "Distribution of ICER (DeltaCost / DeltaQALY)"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::xlim(
          -2 * abs(stats::median(df$icur, na.rm = TRUE)),
           2 * abs(stats::median(df$icur, na.rm = TRUE))
        )
    })

    output$ceac_plot <- shiny::renderPlot({
      df      <- psa_sim()
      wtp_seq <- seq(0, 2 * input$wtp, by = input$wtp / 10)
      prob_ce <- sapply(wtp_seq, function(wtp) mean(df$d_cost < wtp * df$d_qaly, na.rm = TRUE))
      ceac_df <- data.frame(WTP = wtp_seq, Probability = prob_ce)
      ggplot2::ggplot(ceac_df, ggplot2::aes(x = WTP, y = Probability)) +
        ggplot2::geom_line(linewidth = 1.2, color = "#1565C0") +
        ggplot2::geom_point(size = 2,       color = "#1565C0") +
        ggplot2::labs(
          x     = "Willingness-to-Pay Threshold",
          y     = "Probability Cost-Effective",
          title = "Cost-Effectiveness Acceptability Curve (CEAC)"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::ylim(0, 1)
    })

  })
}
