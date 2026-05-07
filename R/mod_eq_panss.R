# R/mod_eq_panss.R

#' EQ_PANSS Calculator UI
#' @param id Character. The Shiny module namespace ID.
#' @return A \code{tabPanel} UI element.
#' @export
mod_eq_panss_ui <- function(id) {
ns <- NS(id)
tabPanel(
"1. EQ_PANSS calculator",
navlistPanel(widths = c(2, 10),
tabPanel(
  "EQ_PANSS calculator",
  sidebarPanel(
    HTML("<h6 style='color: green;'> This calculator estimates EQ-5D-5L utility score from PANSS subscale, age and gender</h6>"),
    numericInput(ns('positive'), 'PANSS positive scores (range: 7-49)', 34, min = 7, max = 49, step = 1),
    numericInput(ns('negative'), 'PANSS negative scores (range: 7-49)', 17, min = 7, max = 49, step = 1),
    numericInput(ns('gps'), 'PANSS general psychopathology scores (range: 16-100)', 80, min = 16, max = 100, step = 1),
    numericInput(ns('age'), 'Age', 21, min = 21, max = 100, step = 1),
    numericInput(ns('gender'), 'Enter gender code here (1 = female, 0 = male)', 1, min =0, max = 1, step = 1),
    actionButton(ns("btn_calculate1"), "Calculate the scores")
  ),
  mainPanel(br(),
            h6('EQ_PANSS calculator Summary:'),
            uiOutput(ns("text1")),
            br(),
            downloadButton(ns("downloadData1"), "Download your data")
  )
),

tabPanel(
"Upload your dataset and generate scores",
sidebarLayout(
  sidebarPanel(
    HTML("<h4 style='color: gray;'> Follow the 3 steps.</h4>"),
    HTML("<h4 style='color: gray;'> (Browse > Run > Download).</h4>"),
    HTML("<p style='color: blue;; font-size: 0.85em;'> Note: Please ensure your column headers use the following names. The names are case sensitive:
    positive, negative, gps, age, gender.</p>"),
    fileInput(ns("file_upload_panss"), "Upload Excel File in csv or xlsx excel format"),
    accept =c(".csv", ".xlsx"),
    uiOutput(ns("file_error_message")),  # <- This line displays the error
    br(),
    HTML("<h6 style='color: green;'> Press Run to calculate the utility scores.</h6>"),
    actionButton(ns("btn_calculate_panss_upload"), "Run"), 
    br(),
    HTML("<h6 style='color: green;'> DOWNLOAD the processed excel.</h6>"),
    downloadButton(ns("file_download_panss_upload"), "Download")
  ),
  mainPanel(
    HTML("<p style='color: blue;; font-size: 0.85em;'> Your calculated EQ-5D-5L index value will be displayed here! Please check before downloading your processed dataset.</p>"),
    tags$h6("Active Dataset"),
    tableOutput(ns("table_output_panss"))
  )))))
}






# Server for EQ_PANSS calculator

#' EQ_PANSS Calculator Server
#' @param id Character. The Shiny module namespace ID.
#' @return Called for its side effects.
#' @export
mod_eq_panss_server <- function(id) {
moduleServer(id, function(input, output, session) {
ns <- session$ns
    
# Reactive expressions for individual calculator
x_reactive <- reactive({panss_utility_score(input$positive, input$negative, input$gps, input$age, input$gender)})
z_reactive <- reactive({panss_utility_comparison(input$positive, input$negative, input$gps, input$age, input$gender)})
    
xyz_reactive <- reactive({
      data.frame(
      positive = c(input$positive),
      negative = c(input$negative), 
      gps = c(input$gps), 
      age = c(input$age), # Use 'age' for consistency with upload
      gender = c(input$gender),
      utility = c(isolate(x_reactive())),
      timezone = c(Sys.timezone()),
      time = c(Sys.time())
      )
    })
    
    output$downloadData1 <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        writexl::write_xlsx(xyz_reactive(), file)
      }
    )
    
    observeEvent(input$btn_calculate1, {
      x <- isolate(x_reactive())
      z <- isolate(z_reactive())
      output$text1 <- renderUI({
        HTML(paste0(
          "<h2 style='color: green;'> ", x  ," point", "</h2>",
          "<b>", "Your utility value is ",x,". The value is ",z," the population mean. 
          The mean Singapore utility-based EQ-5D value was 0.95 and ranges from -0.769 to 1."),   "</b>",
          "<br>",
          "<br>",
          "Note: The EQ-5D-5L utility value was predicted by OLS regression model",
          "<br>",
          "<br>", 
          ("<p style='color: grey;; font-size: 0.85em;'> 
          Reference: Abdin  et al. (2019) Mapping the Positive and Negative Syndrome Scale scores
          to EQ-5D-5L and SF-6D utility scores in patients with schizophrenia. Qual Life Res. 28(1):177-186. 
          doi: <a href=https://pubmed.ncbi.nlm.nih.gov/30382480/, > 10.1007/s11136-018-2037-7 </a>.  
          </p>")
        )
      })
    })
    
    # Reactive value to store uploaded PANSS data
    data_panss <- reactiveVal()
    
    # Observe file upload and validate
    observeEvent(input$file_upload_panss, {
      req(input$file_upload_panss)
      inFile <- input$file_upload_panss
      ext <- tools::file_ext(inFile$datapath)
      
      # Initialize error message
      error_msg <- NULL
      
      if (ext %in% c("csv", "xlsx")) {
        df_panss <- switch(ext,
                           "csv" = read.csv(inFile$datapath),
                           "xlsx" = readxl::read_excel(inFile$datapath)
        )
        
        # Normalize column names
        colnames(df_panss) <- tolower(trimws(colnames(df_panss)))
        
        # Required columns
        required_cols <- c("positive", "negative", "gps", "age", "gender")
        missing_cols <- setdiff(required_cols, colnames(df_panss))
        
        if (length(missing_cols) > 0) {
          error_msg <- paste("WARNING: Missing columns:", paste(missing_cols, collapse = ", "))
          data_panss(NULL)
        } else {
          data_panss(df_panss)
        }
      } else {
        error_msg <- "ERROR: Invalid file. Please upload a .csv or .xlsx file format."
        data_panss(NULL)
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
output$table_output_panss <- renderTable({
req(data_panss())
data_panss()
})

# Observe available dataset uploaded for calculation using     
observeEvent(input$btn_calculate_panss_upload, {
      req(data_panss())
      df_panss <- data_panss()
      
      # Apply the utility function to the uploaded data
      df_panss$utility_score <- mapply(panss_utility_score, df_panss$positive, df_panss$negative, df_panss$gps, df_panss$age, df_panss$gender)
      
      # Add timezone and time
      df_panss$timezone <- Sys.timezone()
      df_panss$time <- Sys.time()
      
      data_panss(df_panss)
      # No direct write to file here, just update reactiveVal
    })
    
    output$file_download_panss_upload <- downloadHandler(
      filename = function() {
        "processed_panss_data.xlsx"
      },
      content = function(file) {
        req(data_panss())
        writexl::write_xlsx(data_panss(), file)
      }
    )
  })
}
