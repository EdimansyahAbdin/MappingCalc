# R/mod_eq_sqls.R

#' EQ_SQLS Calculator UI
#' @param id Character. The Shiny module namespace ID.
#' @return A \code{tabPanel} UI element.
#' @export
mod_eq_sqls_ui <- function(id) {
ns <- NS(id)
tabPanel(
"2.EQ_SQLS Calculator",
navlistPanel(widths = c(2, 10),
tabPanel(
  "EQ_SQLS Calculator",
  sidebarPanel(
    HTML("<h6 style='color: green;'> This calculator estimates EQ-5D-5L utility score from SQLS subscale, age and gender </h6>"),
    numericInput(ns('psychosocial'), 'Enter SQLS psychosocial scores here (range: 0-100)', 50, min = 0, max = 100, step = 1),
    numericInput(ns('motivation'), 'Enter SQLS motivation scores here (range: 0-100)', 50, min = 0, max = 100, step = 1),
    numericInput(ns('symptoms'), 'Enter SQLS symptoms scores here (range: 0-100)', 50, min = 0, max = 100, step = 1),
    numericInput(ns('age'), 'Age', 21, min = 21, max = 100, step = 1),
    numericInput(ns('gender'), 'Enter gender code here (1 = Female, 0 = Male)', 1, min = 0, max = 1, step = 1),
    actionButton(ns("btn_calculate2"), "Calculate the scores")
  ),
  mainPanel(br(),
            h6('EQ_SQLS Calculator Summary:'),
            uiOutput(ns("text2")),
            br(),
            downloadButton(ns("downloadData2"), "Download your data")
  )),
tabPanel(
  "Upload your dataset and generate scores",
  sidebarLayout(
    sidebarPanel(
      HTML("<h4 style='color: gray;'> Follow the 3 steps.</h4>"),
      HTML("<h4 style='color: gray;'> (Browse > Run > Download).</h4>"),
      HTML("<p style='color: blue;; font-size: 0.85em;'> Note: Please ensure your column headers use the following names. The names are case sensitive:
      psychosocial, motivation, symptoms, age, gender.</p>"),
      fileInput(ns("file_upload_sqls"), "Upload Excel File in csv or xlsx excel format"),
      accept =c(".csv", ".xlsx"),
      uiOutput(ns("file_error_message")),  # <- This line displays the error
      br(),
      HTML("<h6 style='color: green;'> Press Run to calculate the utility scores.</h6>"),
      actionButton(ns("btn_calculate_sqls_upload"), "Run"), 
      br(),
      HTML("<h6 style='color: green;'> DOWNLOAD the processed excel.</h6>"),
      downloadButton(ns("file_download_sqls_upload"), "Download")
    ),
    mainPanel(
      HTML("<p style='color: blue;; font-size: 0.85em;'> Your calculated EQ-5D-5L index value will be displayed here! Please check before downloading your processed dataset.</p>"),
      tags$h6("Active Dataset"),
      tableOutput(ns("table_output_sqls"))
    )))))
}

# Server for EQ_SQLS calculator

#' EQ_SQLS Calculator Server
#' @param id Character. The Shiny module namespace ID.
#' @return Called for its side effects.
#' @export
mod_eq_sqls_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    a_reactive <- reactive({sqls_utility_score(input$psychosocial,input$motivation,input$symptoms,input$age,input$gender)})
    ab_reactive <- reactive({sqls_utility_comparison(input$psychosocial,input$motivation,input$symptoms,input$age,input$gender)})    
    
    aab_reactive <- reactive({data.frame(
      psychosocial = c(input$psychosocial),
      motivation = c(input$motivation), 
      symptoms = c(input$symptoms), 
      age = c(input$age), 
      gender = c(input$gender),
      utility = c(isolate(a_reactive())),
      timezone = c(Sys.timezone()),
      time = c(Sys.time())
    )})
    
    output$downloadData2 <- downloadHandler(
      filename=function() {
        paste("data-", Sys.Date(), ".xlsx", sep="")},
      content = function(file) {
        writexl::write_xlsx(aab_reactive(),file)
      })
    
    observeEvent(input$btn_calculate2, {
      a <- isolate(a_reactive())
      ab <- isolate(ab_reactive())
      output$text2 <- renderUI({
        HTML(paste0(
          "<h2 style='color: green;'> ", a  ," point", "</h2>",
          "<b>", "Your utility value is ",a,". It is ",ab," the population mean. 
          The mean Singapore utility-based EQ-5D value was 0.95 and ranges from -0.769 to 1."),   "</b>",
          "<br>",
          "<br>",
          "Note: The EQ-5D-5L utility value was predicted by OLS regression model",
          "<br>",
          "<br>",        ("<p style='color: grey;; font-size: 0.85em;'> 
          Reference: Seow et al. (2023) Mapping the schizophrenia quality of life scale to EQ-5D, HUI3 and SF-6D utility scores in patients with schizophrenia. Expert Rev Pharmacoecon Outcomes Res. 23(7):813-821. 
          doi: <a  href = https://pubmed.ncbi.nlm.nih.gov/37216213/, > 10.1080/14737167.2023.2215430 </a>
          </p>")
        )
      })
    })
    

    
# Reactive value to store uploaded SQLS data
data_sqls <- reactiveVal()
    
# Observe file upload and validate
    observeEvent(input$file_upload_sqls, {
      req(input$file_upload_sqls)
      inFile <- input$file_upload_sqls
      ext <- tools::file_ext(inFile$datapath)
      
      # Initialize error message
      error_msg <- NULL
      
      if (ext %in% c("csv", "xlsx")) {
        df_sqls <- switch(ext,
                           "csv" = read.csv(inFile$datapath),
                           "xlsx" = readxl::read_excel(inFile$datapath)
        )
        
        # Normalize column names
        colnames(df_sqls) <- tolower(trimws(colnames(df_sqls)))
        
        # Required columns
        required_cols <- c("psychosocial", "motivation", "symptoms", "age", "gender")
        missing_cols <- setdiff(required_cols, colnames(df_sqls))
        
        if (length(missing_cols) > 0) {
          error_msg <- paste("WARNING: Missing columns:", paste(missing_cols, collapse = ", "))
          data_sqls(NULL)
        } else {
          data_sqls(df_sqls)
        }
      } else {
        error_msg <- "ERROR: Invalid file. Please upload a .csv or .xlsx file format."
        data_sqls(NULL)
      }
      
      # Render error message if any
      # Render means display
      output$file_error_message <- renderUI({
        if (!is.null(error_msg)) {
          p(error_msg, style = "color: red;")
        } else {
          NULL
        }
      })
    })
    
# Display uploaded data preview
    output$table_output_sqls <- renderTable({
      req(data_sqls())
      data_sqls()
    })
    
# Observe available dataset uploaded for calculation using     
    observeEvent(input$btn_calculate_sqls_upload, {
      req(data_sqls())
      df_sqls <- data_sqls()
      
      # Apply the utility function to the uploaded data
      df_sqls$utility_score <- mapply(sqls_utility_score, df_sqls$psychosocial, df_sqls$motivation, df_sqls$symptoms, df_sqls$age, df_sqls$gender)
      
      # Add timezone and time
      df_sqls$timezone <- Sys.timezone()
      df_sqls$time <- Sys.time()
      
      data_sqls(df_sqls)
      # No direct write to file here, just update reactiveVal
    })
    
    output$file_download_sqls_upload <- downloadHandler(
      filename = function() {
        "processed_sqls_data.xlsx"
      },
      content = function(file) {
      req(data_sqls())
      writexl::write_xlsx(data_sqls(), file)
      }
    )
  })
}
