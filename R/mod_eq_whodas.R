# R/mod_eq_whodas.R

#' EQ_WHODAS Calculator UI
#' @param id Character. The Shiny module namespace ID.
#' @return A \code{tabPanel} UI element.
#' @export
mod_eq_whodas_ui <- function(id) {
ns <- NS(id)
tabPanel(
"3. EQ_WHODAS Calculator",
navlistPanel(widths = c(2, 10),
tabPanel("EQ_WHODAS Calculator",
sidebarPanel(
  HTML("<h6 style='color: green;'> This calculator estimates EQ-5D-5L utility score from WHODAS 2.0 total scores </h6>"),
  numericInput(ns('whodas_scores'), 'Enter WHODAS total scores here (range: 0-48)', 10, min = 0, max = 48, step = 1),
  actionButton(ns("btn_calculate3"), "Calculate the scores"),
),
mainPanel(br(),
          h6('EQ_WHODAS Calculator Summary:'),
          uiOutput(ns("text3")),
          br(),
          downloadButton(ns("downloadData3"), "Download your data"),
          br(),
)),
                 
#tabPanel(
#  "3. EQ_WHODAS Calculator based on individual items",
#  sidebarPanel(
#    h4("12-item of the WHODAS 2.0"),
#    h6("In the past 30 days, how much difficulty did you have in:"),
#    h6("0=None,1=Mild,2=Moderate, 3=Severe,4=Extremely or cannot do"),
#    sliderInput(ns('whodas1'), '1.Standing for long periods such as 30 minutes?', 1, min = 0, max = 4, step = 1),
#    sliderInput(ns('whodas2'), '2.Taking care of your household responsibilities?', 1, min = 0, max = 4, step = 1),
#    sliderInput(ns('whodas3'), '3.Learning new task, for example learning how to get to a new place?', 1, min = 0, max = 4, step = 1),
#    sliderInput(ns('whodas4'), '4.How much of a problem did you have joining in community activities 
#     (for example, festivities, religious or other activities) in the same way as anyone else can?', 1, min = 0, max = 4, step = 1),
#    sliderInput(ns('whodas5'), '5.How much have you been emotionally affected by your health problems?', 1, min = 0, max = 4, step = 1),
#    sliderInput(ns('whodas6'), '6.5.Concentrating on doing something for ten minutes your health problems?', 1, min = 0, max = 4, step = 1),
#    sliderInput(ns('whodas7'), '7.Walking a long distance such as a kilometre [or equivalent]?', 1, min = 0, max = 4, step = 1),
#    sliderInput(ns('whodas8'), '8.Washing your whole body', 1, min = 0, max = 4, step = 1),
#    sliderInput(ns('whodas9'), '9.Getting dressed?', 1, min = 0, max = 4, step = 1),
#    sliderInput(ns('whodas10'), '10.Dealing with people you do not know?', 1, min = 0, max = 4, step = 1),
#    sliderInput(ns('whodas11'), '11.Maintaining a friendship', 1, min = 0, max = 4, step = 1),
#    sliderInput(ns('whodas12'), '12.Your day-to-day work?', 1, min = 0, max = 4, step = 1),
#    actionButton(ns("btn_calculate4"), "Calculate the scores"),
#  ),
#  mainPanel(br(),
#  h6('EQ_WHODAS Calculator Summary:'),
#  uiOutput(ns("text4")),
#  br(),
#  downloadButton(ns("downloadData4"), "Download your data"),
#  br(),
#)
tabPanel(
  "Upload your dataset and generate scores",
  sidebarLayout(
    sidebarPanel(
      HTML("<h4 style='color: gray;'> Follow the 3 steps.</h4>"),
      HTML("<h4 style='color: gray;'> (Browse > Run > Download).</h4>"),
      HTML("<p style='color: blue;; font-size: 0.85em;'> Note: Please ensure your column headers use the following names. The names are case sensitive:
      psychosocial, motivation, symptoms, age, gender.</p>"),
      fileInput(ns("file_upload_whodas"), "Upload Excel File in csv or xlsx excel format"),
      accept =c(".csv", ".xlsx"),
      uiOutput(ns("file_error_message")),  # <- This line displays the error
      br(),
      HTML("<h6 style='color: green;'> Press Run to calculate the utility scores.</h6>"),
      actionButton(ns("btn_calculate_whodas_upload"), "Run"), 
      br(),
      HTML("<h6 style='color: green;'> DOWNLOAD the processed excel.</h6>"),
      downloadButton(ns("file_download_whodas_upload"), "Download")
    ),
    mainPanel(
      HTML("<p style='color: blue;; font-size: 0.85em;'> Your calculated EQ-5D-5L index value will be displayed here! Please check before downloading your processed dataset.</p>"),
      tags$h6("Active Dataset"),
      tableOutput(ns("table_output_whodas"))
    )))))
}



# Server for EQ_WHODAS calculator

#' EQ_WHODAS Calculator Server
#' @param id Character. The Shiny module namespace ID.
#' @return Called for its side effects.
#' @export
mod_eq_whodas_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Total Scores Calculator
    c_reactive <- reactive({whodas_total_utility_score(input$whodas_scores)})
    cd_reactive <- reactive({whodas_total_utility_comparison(input$whodas_scores)})
    
    ccd_reactive <- reactive({data.frame(
      whodas_scores = c(input$whodas_scores),
      utility = c(isolate(c_reactive())),
      timezone = c(Sys.timezone()),
      time = c(Sys.time())
    )})
    
    output$downloadData3 <- downloadHandler(
      filename=function() {
        paste("data-", Sys.Date(), ".xlsx", sep="")},
      content = function(file) {
        writexl::write_xlsx(ccd_reactive(),file)
      })
    
    observeEvent(input$btn_calculate3, {
      c_val <- isolate(c_reactive())
      cd_val <- isolate(cd_reactive())
      output$text3 <- renderUI({
        HTML(paste0(
          "<h2 style='color: green;'> ", c_val  ," point", "</h2>",
          "<b>", "Your utility value is ",c_val,". It is ",cd_val," the population mean. 
          The mean Singapore utility-based EQ-5D value was 0.95 and ranges from -0.769 to 1."),   "</b>",
          "<br>",
          "<br>",
          "Note: The EQ-5D-5L utility value was predicted by robust regression with MM estimator",
          "<br>",
          "<br>",
          ("<p style='color: grey;; font-size: 0.85em;'> 
          Reference: Abdin  et al. (2024) Mapping the World Health Organizationi Disability Assessment Scale 2.0 to
          the EQ-5D-5L in patients with mental disorders. Expert Review of Pharmacoeconomics & Outcome Research. 
          doi: <a  href = https://pubmed.ncbi.nlm.nih.gov/38967393/, > 10.1080/14737167.2024.2376100 </a>
          </p>")
        )
      })
  })
    

# Reactive value to store uploaded SQLS data
data_whodas <- reactiveVal()
    
    # Observe file upload and validate
    observeEvent(input$file_upload_whodas, {
      req(input$file_upload_whodas)
      inFile <- input$file_upload_whodas
      ext <- tools::file_ext(inFile$datapath)
      
      # Initialize error message
      error_msg <- NULL
      
      if (ext %in% c("csv", "xlsx")) {
        df_whodas <- switch(ext,
                          "csv" = read.csv(inFile$datapath),
                          "xlsx" = readxl::read_excel(inFile$datapath)
        )
        
        # Normalize column names
        colnames(df_whodas) <- tolower(trimws(colnames(df_whodas)))
        
        # Required columns
        required_cols <- c("whodas_scores")
        missing_cols <- setdiff(required_cols, colnames(df_whodas))
        
        if (length(missing_cols) > 0) {
          error_msg <- paste("WARNING: Missing columns:", paste(missing_cols, collapse = ", "))
          data_whodas(NULL)
        } else {
          data_whodas(df_whodas)
        }
      } else {
        error_msg <- "ERROR: Invalid file. Please upload a .csv or .xlsx file format."
        data_whodas(NULL)
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
    output$table_output_whodas <- renderTable({
      req(data_whodas())
      data_whodas()
    })
    
    # Observe available dataset uploaded for calculation using     
    observeEvent(input$btn_calculate_whodas_upload, {
      req(data_whodas())
      df_whodas <- data_whodas()
      
      # Apply the utility function to the uploaded data
      df_whodas$utility_score <- mapply(whodas_total_utility_score, df_whodas$whodas_scores)
      
      # Add timezone and time
      df_whodas$timezone <- Sys.timezone()
      df_whodas$time <- Sys.time()
      
      data_whodas(df_whodas)
      # No direct write to file here, just update reactiveVal
    })
    
    output$file_download_whodas_upload <- downloadHandler(
      filename = function() {
        "processed_whodas_data.xlsx"
      },
      content = function(file) {
        req(data_whodas())
        writexl::write_xlsx(data_whodas(), file)
      }
    )
  })
}    
    
        # Individual Items Calculator
#    d_reactive <- reactive({whodas_items_utility_score(input$whodas1, input$whodas2, input$whodas3, input$whodas4,
#                                                       input$whodas5, input$whodas6, input$whodas7, input$whodas8,
#                                                       input$whodas9, input$whodas10, input$whodas11, input$whodas12)})
#    
#    dd_reactive <- reactive({
#      whodas_items_utility_comparison(input$whodas1, input$whodas2, input$whodas3, input$whodas4,
#                                      input$whodas5, input$whodas6, input$whodas7, input$whodas8,
#                                      input$whodas9, input$whodas10, input$whodas11, input$whodas12)
#    })
#    
#    ddd_reactive <- reactive({
#      whodas_items_total_score(input$whodas1, input$whodas2, input$whodas3, input$whodas4,
#                               input$whodas5, input$whodas6, input$whodas7, input$whodas8,
#                               input$whodas9, input$whodas10, input$whodas11, input$whodas12)
#    })
#    
#    observeEvent(input$btn_calculate4, {
#      d_val <- isolate(d_reactive())
#      dd_val <- isolate(dd_reactive())
#      output$text4 <- renderUI({
#        HTML(paste0(
#          "<h2 style='color: green;'> ", d_val  ," point", "</h2>",
#          "<b>", "Your utility value is ",d_val,". It is ",dd_val," the population mean. 
#          The mean Singapore utility-based EQ-5D value was 0.95 and ranges from -0.769 to 1."),   "</b>",
#          "<br>",
#          "<br>",
#          "Note: The EQ-5D-5L utility value was predicted by robust regression with MM estimator",
#          "<br>",
#          "<br>",
#          ("<p style='color: grey;; font-size: 0.85em;'> 
#          Reference: Abdin  et al. (2024) Mapping the World Health Organizationi Disability Assessment Scale 2.0 to
#          the EQ-5D-5L in patients with mental disorders. Expert Review of Pharmacoeconomics & Outcome Research. 
#          doi: <a  href = https://pubmed.ncbi.nlm.nih.gov/38967393/, > 10.1080/14737167.2024.2376100 </a>
#          </p>")
#        )
#      })
#    })
#    
#    eef_reactive <- reactive({data.frame(
#      whodas1 = c(input$whodas1),
#      whodas2 = c(input$whodas2),
#      whodas3 = c(input$whodas3),
#      whodas4 = c(input$whodas4),
#      whodas5 = c(input$whodas5),
#      whodas6 = c(input$whodas6),
#      whodas7 = c(input$whodas7),
#      whodas8 = c(input$whodas8),
#      whodas9 = c(input$whodas9),
#      whodas10 = c(input$whodas10),
#      whodas11 = c(input$whodas11),
#      whodas12 = c(input$whodas12),
#      totalscores =  c(isolate(ddd_reactive())),
#      utility =  c(isolate(d_reactive())),
#      timezone = c(Sys.timezone()),
#      time = c(Sys.time())
#    )})
#    output$downloadData4 <- downloadHandler(
#      filename=function() {
#        paste("data-", Sys.Date(), ".xlsx", sep="")},
#      content = function(file) {
#        writexl::write_xlsx(eef_reactive(),file)
#      })
  
#})
#}
