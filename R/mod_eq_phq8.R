# R/mod_eq_phq8.R
# MappingCalc v2.0 -- Tab 4: EQ_PHQ8 Calculator
# Maps PHQ-8 to EQ-5D-5L using a 2-component Beta Mixture Model

# ------------------------------------------------------------
# MODEL COEFFICIENTS (from Supplementary Table 2)
# ------------------------------------------------------------

betamix_coefs <- list(
  # Component 1 mean (logit scale)
  C1_mu = c(
    PHQ       = -0.08069,
    PHQ2      = -0.00447,
    Age       = -0.05567,
    Female    =  1.219305,
    Intercept =  3.190162
  ),
  # Component 1 log-precision
  C1_lnphi = c(Intercept = 1.226452),

  # Component 2 mean (logit scale)
  C2_mu = c(
    PHQ       =  0.02692,
    PHQ2      = -0.00394,
    Age       = -0.00611,
    Female    = -0.09023,
    Intercept =  2.078374
  ),
  # Component 2 log-precision
  C2_lnphi = c(Intercept = 2.225145),

  # Mixing probability logit (C1 vs C2)
  Prob_C1 = c(Intercept = -1.93758),

  # Point mass at upper bound (ub = 1)
  PM_ub = c(
    PHQ       =  0.470157,
    PHQ2      = -0.11835,
    Age       = -0.07526,
    Female    = -1.08338,
    Intercept =  3.045425
  ),

  # Point mass at threshold bound (tb = 0.883)
  PM_tb = c(
    PHQ       =  0.062733,
    PHQ2      = -0.01213,
    Age       = -0.00324,
    Female    =  0.327139,
    Intercept = -0.85054
  )
)

# Model bounds
PHQ8_LB <- -0.851   # lbound
PHQ8_UB <-  1.000   # ubound
PHQ8_TB <-  0.883   # tbound
PHQ8_A  <- PHQ8_LB
PHQ8_B  <- PHQ8_TB

# ------------------------------------------------------------
# PREDICTION FUNCTION
# ------------------------------------------------------------

predict_betamix <- function(PHQ, Age, Female) {
  PHQ2 <- PHQ^2

  # Design matrix (order: PHQ, PHQ2, Age, Female, Intercept)
  X <- cbind(PHQ, PHQ2, Age, Female, Intercept = 1)

  # --- Component means (mu) ---
  eta_mu1 <- X %*% betamix_coefs$C1_mu
  eta_mu2 <- X %*% betamix_coefs$C2_mu
  mu1 <- plogis(eta_mu1) * (PHQ8_B - PHQ8_A) + PHQ8_A
  mu2 <- plogis(eta_mu2) * (PHQ8_B - PHQ8_A) + PHQ8_A

  # --- Component precisions (phi) ---
  phi1 <- exp(betamix_coefs$C1_lnphi[["Intercept"]])
  phi2 <- exp(betamix_coefs$C2_lnphi[["Intercept"]])

  # --- Mixing probabilities ---
  logit_c1 <- exp(betamix_coefs$Prob_C1[["Intercept"]])
  sumpr    <- 1 + logit_c1
  p1       <- as.vector(logit_c1 / sumpr)
  p2       <- 1 - p1

  # --- Point mass probabilities ---
  t_ub <- exp(X %*% betamix_coefs$PM_ub)
  t_tb <- exp(X %*% betamix_coefs$PM_tb)
  pm_sum <- 1 + t_ub + t_tb

  pr_ub  <- as.vector(t_ub / pm_sum)
  pr_tb  <- as.vector(t_tb / pm_sum)
  pr_oth <- 1 - pr_ub - pr_tb

  # --- Mixture mean of continuous Beta part ---
  meanb <- p1 * as.vector(mu1) + p2 * as.vector(mu2)

  # --- Final predicted score ---
  yhat <- pr_ub * PHQ8_UB + pr_tb * PHQ8_TB + pr_oth * meanb

  data.frame(
    EQ5D  = round(yhat, 6),
    mu1   = round(as.vector(mu1), 6),
    mu2   = round(as.vector(mu2), 6),
    phi1  = round(phi1, 6),
    phi2  = round(phi2, 6),
    p1    = round(p1, 6),
    p2    = round(p2, 6),
    pr_ub = round(pr_ub, 6),
    pr_tb = round(pr_tb, 6)
  )
}

# ------------------------------------------------------------
# UI MODULE
# ------------------------------------------------------------

#' EQ_PHQ8 Calculator UI
#'
#' @description Shiny module UI for the EQ_PHQ8 calculator tab. Maps PHQ-8
#'   scores to EQ-5D-5L utility values using a 2-component Beta Mixture Model.
#'
#' @param id Character. The Shiny module namespace ID.
#'
#' @return A Shiny \code{tabPanel} UI element.
#' @export
mod_eq_phq8_ui <- function(id) {
  ns <- NS(id)

  tabPanel("4. EQ_PHQ8 Calculator",
    navlistPanel(widths = c(2, 10),

      # ---- Manual Entry ----
      tabPanel("EQ_PHQ8 Calculator",
        sidebarPanel(
          HTML("<h6 style='color: green;'>This calculator estimates EQ-5D-5L utility scores from PHQ-8 scores
               using a 2-component Beta Mixture Model.</h6>"),
          numericInput(ns("phq"),    "PHQ-8 score (0-24):", value = 10, min = 0, max = 24, step = 1),
          numericInput(ns("age"),    "Age (years):",        value = 45, min = 18, max = 100, step = 1),
          selectInput(ns("female"),  "Sex:",
                      choices = c("Male" = 0, "Female" = 1), selected = 1),
          actionButton(ns("calc_manual"), "Calculate the score")
        ),
        mainPanel(
          br(),
          h6("EQ_PHQ8 Calculator Summary:"),
          uiOutput(ns("manual_result")),
          br(),
          h6("Full output table (all model components):"),
          DT::DTOutput(ns("manual_table")),
          br(),
          downloadButton(ns("dl_manual"), "Download your data"),
          br()
        )
      ),

      # ---- Upload Dataset ----
      tabPanel("Upload your dataset and generate scores",
        sidebarLayout(
          sidebarPanel(
            HTML("<h4 style='color: gray;'>Follow the 3 steps.</h4>"),
            HTML("<h4 style='color: gray;'>(Browse > Run > Download).</h4>"),
            HTML("<p style='color: blue; font-size: 0.85em;'>Note: Please ensure your column headers use the following names
                 (case sensitive): <b>PHQ</b>, <b>Age</b>, <b>Female</b>
                 (Female: 1 = female, 0 = male). PHQ range: 0-24.</p>"),
            fileInput(ns("csv_file"), "Upload CSV or Excel file:",
                      accept = c(".csv", ".xlsx")),
            uiOutput(ns("file_error_message")),
            br(),
            downloadButton(ns("dl_template"), "Download Template CSV"),
            br(), br(),
            HTML("<h6 style='color: green;'>Press Run to calculate the utility scores.</h6>"),
            actionButton(ns("calc_csv"), "Run"),
            br(),
            HTML("<h6 style='color: green;'>DOWNLOAD the processed file.</h6>"),
            downloadButton(ns("dl_csv"), "Download")
          ),
          mainPanel(
            HTML("<p style='color: blue; font-size: 0.85em;'>Your calculated EQ-5D-5L index values will be displayed here.
                 Please check before downloading.</p>"),
            tags$h6("Active Dataset"),
            uiOutput(ns("csv_result_summary")),
            br(),
            DT::DTOutput(ns("csv_table"))
          )
        )
      )
    )
  )
}

# ------------------------------------------------------------
# SERVER MODULE
# ------------------------------------------------------------

#' EQ_PHQ8 Calculator Server
#'
#' @description Shiny module server for the EQ_PHQ8 calculator. Handles
#'   individual score prediction and batch dataset upload/download using
#'   a 2-component Beta Mixture Model.
#'
#' @param id Character. The Shiny module namespace ID.
#'
#' @return A Shiny module server function (called for its side effects).
#' @export
mod_eq_phq8_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Manual prediction ----
    manual_pred <- eventReactive(input$calc_manual, {
      phq <- input$phq
      age <- input$age
      fem <- as.numeric(input$female)

      if (is.na(phq) || phq < 0 || phq > 24)
        return(list(error = "PHQ-8 must be between 0 and 24."))
      if (is.na(age) || age < 18)
        return(list(error = "Please enter a valid age (18 or older)."))

      res <- predict_betamix(PHQ = phq, Age = age, Female = fem)
      list(data = res)
    })

    output$manual_result <- renderUI({
      req(manual_pred())
      mp <- manual_pred()
      if (!is.null(mp$error)) {
        tags$p(style = "color:red;", mp$error)
      } else {
        HTML(paste0(
          "<h2 style='color: green;'>", sprintf("%.4f", mp$data$EQ5D), "</h2>",
          "<b>Predicted EQ-5D-5L utility score</b>",
          "<br>PHQ-8: ", input$phq,
          " &nbsp;|&nbsp; Age: ", input$age,
          " &nbsp;|&nbsp; Sex: ", ifelse(input$female == "1", "Female", "Male"),
          "<br><br>",
          "<small style='color:grey;'>Reference: Beta Mixture Model (2-component). ",
          "See Supplementary Table 2 for model coefficients.</small>"
        ))
      }
    })

    output$manual_table <- DT::renderDT({
      req(manual_pred())
      mp <- manual_pred()
      if (!is.null(mp$error)) return(NULL)
      col_labels <- c(
        EQ5D  = "Predicted EQ-5D Score (EQ5D)",
        mu1   = "\u03bc\u2081 (Component 1 Mean)",
        mu2   = "\u03bc\u2082 (Component 2 Mean)",
        phi1  = "\u03c6\u2081 (Precision 1)",
        phi2  = "\u03c6\u2082 (Precision 2)",
        p1    = "\u03c0\u2081 (Mixing Weight 1)",
        p2    = "\u03c0\u2082 (Mixing Weight 2)",
        pr_ub = "P(score=1.0)",
        pr_tb = "P(score=0.883)"
      )
      d <- mp$data
      names(d) <- col_labels[names(d)]
      DT::datatable(d, rownames = FALSE,
                    options = list(dom = "t", scrollX = TRUE),
                    class = "stripe hover compact")
    }, server = FALSE)

    output$dl_manual <- downloadHandler(
      filename = function() paste0("EQ_PHQ8_result_", Sys.Date(), ".xlsx"),
      content = function(file) {
        mp <- manual_pred()
        req(!is.null(mp$data))
        writexl::write_xlsx(mp$data, file)
      }
    )

    # ---- CSV Upload prediction ----
    data_phq8 <- reactiveVal()

    observeEvent(input$csv_file, {
      req(input$csv_file)
      inFile <- input$csv_file
      ext    <- tools::file_ext(inFile$datapath)
      error_msg <- NULL

      if (ext %in% c("csv", "xlsx")) {
        df <- tryCatch(
          switch(ext,
            "csv"  = read.csv(inFile$datapath, stringsAsFactors = FALSE),
            "xlsx" = readxl::read_excel(inFile$datapath)
          ),
          error = function(e) NULL
        )

        if (is.null(df)) {
          error_msg <- "Could not read the file. Please check the format."
          data_phq8(NULL)
        } else {
          required_cols <- c("PHQ", "Age", "Female")
          missing <- setdiff(required_cols, names(df))
          if (length(missing) > 0) {
            error_msg <- paste("\u26a0\ufe0f Missing columns:", paste(missing, collapse = ", "))
            data_phq8(NULL)
          } else if (any(is.na(df$PHQ) | df$PHQ < 0 | df$PHQ > 24)) {
            error_msg <- "Some PHQ-8 values are missing or outside 0-24. Please check your data."
            data_phq8(NULL)
          } else if (any(!df$Female %in% c(0, 1))) {
            error_msg <- "Female column must contain only 0 or 1."
            data_phq8(NULL)
          } else {
            data_phq8(df)
          }
        }
      } else {
        error_msg <- "\u274c Invalid file type. Please upload a .csv or .xlsx file."
        data_phq8(NULL)
      }

      output$file_error_message <- renderUI({
        if (!is.null(error_msg)) tags$p(error_msg, style = "color:red;") else NULL
      })
    })

    observeEvent(input$calc_csv, {
      req(data_phq8())
      df <- data_phq8()
      res <- predict_betamix(PHQ = df$PHQ, Age = df$Age, Female = df$Female)
      data_phq8(cbind(df, res))
    })

    output$csv_result_summary <- renderUI({
      req(data_phq8())
      df <- data_phq8()
      if (!"EQ5D" %in% names(df)) return(NULL)
      HTML(paste0(
        "<h2 style='color: green;'>", sprintf("%.4f", mean(df$EQ5D)), "</h2>",
        "<b>Mean predicted EQ-5D-5L score across ", nrow(df), " observations</b>",
        "<br>Range: ", sprintf("%.4f", min(df$EQ5D)), " \u2013 ", sprintf("%.4f", max(df$EQ5D))
      ))
    })

    output$csv_table <- DT::renderDT({
      req(data_phq8())
      df <- data_phq8()
      if (!"EQ5D" %in% names(df)) return(NULL)
      DT::datatable(df, rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE),
                    class = "stripe hover compact")
    }, server = FALSE)

    output$dl_csv <- downloadHandler(
      filename = function() paste0("EQ_PHQ8_results_", Sys.Date(), ".xlsx"),
      content = function(file) {
        req(data_phq8())
        writexl::write_xlsx(data_phq8(), file)
      }
    )

    output$dl_template <- downloadHandler(
      filename = "EQ_PHQ8_template.csv",
      content = function(file) {
        tmpl <- data.frame(PHQ = c(5, 10, 18), Age = c(35, 45, 60), Female = c(1, 0, 1))
        write.csv(tmpl, file, row.names = FALSE)
      }
    )
  })
}
