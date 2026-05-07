# R/mod_eq_5d_5l.R

#' EQ_5D_5L Calculator UI
#' @param id Character. The Shiny module namespace ID.
#' @return A \code{tabPanel} UI element.
#' @export
mod_eq_5d_5l_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "4. EQ_5D_5L Calculator",
    navlistPanel(widths = c(2, 10),
                 tabPanel("EQ_5D_5L Calculator",
                          sidebarPanel(
                            HTML("<h6 style='color: green;'> This calculator estimates EQ-5D-5L index value </h6>"),
                            HTML("<p style='color: gray;; font-size: 0.85em;'> (1=no problems, 2=slight problems, 3=moderate problems, 4=severe problems, 5=extreme problems) </p>"),
                            sliderInput(ns('Mobility'), 'Mobility', 3, min = 1, max = 5, step = 1),
                            sliderInput(ns('SelfCare'), 'Self Care', 3, min = 1, max = 5, step = 1),
                            sliderInput(ns('UsualActivities'), 'Usual Activities', 3, min = 1, max = 5, step = 1),
                            sliderInput(ns('PainDiscomfort'), 'Pain Discomfort', 3, min = 1, max = 5, step = 1),
                            sliderInput(ns('AnxietyDepression'), 'Anxiety Depression', 3, min = 1, max = 5, step = 1),
                            actionButton(ns("btn_calculate5"), "Calculate values"),
                          ),
                          mainPanel(br(),
                                    h6('EQ-5D-5L Calculator Summary:'),
                                    uiOutput(ns("text5")),
                                    br(),
                                    downloadButton(ns("downloadData5"), "Download your data"),
                                    br(),
                          )
                 ),
                 
                 tabPanel(
                   "Upload your dataset and generate the scores",
                   sidebarLayout(
                     sidebarPanel(
                       HTML("<h4 style='color: gray;'> Follow the 3 steps.</h4>"),
                       HTML("<h4 style='color: gray;'> (BROWSE > RUN > DOWNLOAD).</h4>"),
                       HTML("<p style='color: blue;; font-size: 0.85em;'> Note: Please ensure your column header using following names. The name is case sensitive:
                  mobility, selfcare, usualactivities, paindiscomfort, anxietydepression.</p>"),
                       fileInput(ns("file_upload1"), "Upload Excel File (data.csv) in CSV format"),
                       br(),
                       HTML("<h6 style='color: green;'> Press RUN to calculate the utility scores.</h6>"),
                       actionButton(ns("btn_calculate6"), "RUN"), 
                       br(),
                       HTML("<h6 style='color: green;'> DOWNLOAD the processed excel.</h6>"),
                       downloadButton(ns("file_download6"), "DOWNLOAD")
                     ),
                     mainPanel(
                       HTML("<p style='color: blue;; font-size: 0.85em;'> Your calculated EQ-5D-5L index value will be display here!. Please check before download your processed dataset.</p>"),
                       tags$h6("Active Dataset"),
                       tableOutput(ns("table_output"))
                     )
                   )
                 )
    )
  )
}

# Server for EQ_5D_5L calculator

#' EQ_5D_5L Calculator Server
#' @param id Character. The Shiny module namespace ID.
#' @return Called for its side effects.
#' @export
mod_eq_5d_5l_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load profile data once
    profile <- read.csv(system.file("app/data/profile.csv", package = "MappingCalc"))
    
    e_reactive <- reactive({eq5d5l_index_value(input$Mobility,input$SelfCare,input$UsualActivities,input$PainDiscomfort,input$AnxietyDepression, profile)})
    ee_reactive <- reactive({eq5d5l_index_comparison(input$Mobility,input$SelfCare,input$UsualActivities,input$PainDiscomfort,input$AnxietyDepression, profile)})
    eee_reactive <- reactive({eq5d5l_profile_string(input$Mobility,input$SelfCare,input$UsualActivities,input$PainDiscomfort,input$AnxietyDepression)})
    
    observeEvent(input$btn_calculate5, {
      e_val <- isolate(e_reactive())
      ee_val <- isolate(ee_reactive())
      eee_val <- isolate(eee_reactive())
      
      output$text5 <- renderUI({
        HTML(paste0(
          "<h2 style='color: green;'> ", e_val  ," point", "</h2>",
          "<b>", "Given your EQ-5D-5L profile is ",eee_val,", your utility value is ",e_val,". The value is ",ee_val,
          " the population mean. 
          The mean Singapore utility-based EQ-5D value was 0.95 and ranges from -0.769 to 1."),   
          "</b>",
          "<br>",
          "<br>",
          "Note: The EQ-5D-5L utility value was calculated using cross-walk method (van Hout et al, 2012)  
          by means of a mapping or a crosswalk approach to the currently available three-level version of the EQ-5D (EQ-5D-3L) values sets (Luo et al, 2014).",
          "<br>",
          "<br>",
          ("<p style='color: grey;; font-size: 0.85em;'> 
          Reference:
          </p>"),
          "<br>",
          ("<p style='color: grey;; font-size: 0.85em;'> 
          1. van Hout B et al. Interim scoring for the EQ-5D-5L: mapping the EQ-5D-5L to EQ-5D-3L value sets. 
          Value Health. 2012 Jul-Aug;15(5):708-15.
          doi: <a  href = https://pubmed.ncbi.nlm.nih.gov/22867780/, > 10.1016/j.jval.2012.02.008 </a>
          </p>"),
          "<br>",
          ("<p style='color: grey;; font-size: 0.85em;'> 
          2. Luo N, Vasan Thakumar A, Cheng LJ, Yang Z, Rand K, Cheung YB, Thumboo J. Developing an EQ-5D-5L Value Set for Singapore. Pharmacoeconomics. 2025 Dec;43(12):1419-1431.
          doi: <a href='https://doi.org/10.1007/s40273-025-01519-7'>10.1007/s40273-025-01519-7</a>
          </p>")
        )
      })
    })
    
    dddf_reactive <- reactive({data.frame(
      Mobility = c(input$Mobility),
      SelfCare = c(input$SelfCare),
      UsualActivities = c(input$UsualActivities),
      PainDiscomfort = c(input$PainDiscomfort),
      AnxietyDepression = c(input$AnxietyDepression),
      Profile =  c(isolate(eee_reactive())),
      Utility =  c(isolate(e_reactive())),
      timezone = c(Sys.timezone()),
      time = c(Sys.time())
    )})
    output$downloadData5 <- downloadHandler(
      filename=function() {
        paste("data-", Sys.Date(), ".xlsx", sep="")},
      content = function(file) {
        writexl::write_xlsx(dddf_reactive(),file)
      })
    
    
    

# Upload Data Logic
    data_eq5d5l_upload <- reactiveVal()
    
    observeEvent(input$file_upload1, {
      req(input$file_upload1)
      inFile <- input$file_upload1
      ext <- tools::file_ext(inFile$datapath)
      
      # Initialize error message
      error_msg <- NULL
      
      if (ext %in% c("csv", "xlsx")) {
        df_eq5d <- switch(ext,
                           "csv" = read.csv(inFile$datapath),
                           "xlsx" = readxl::read_excel(inFile$datapath)
        )
        
        # Normalize column names
        colnames(df_eq5d) <- tolower(trimws(colnames(df_eq5d)))
        
        # Required columns
        required_cols <- c("selfcare", "usualactivities", "paindiscomfort", "anxietydepression")
        missing_cols <- setdiff(required_cols, colnames(df_eq5d))
        
        if (length(missing_cols) > 0) {
          error_msg <- paste("WARNING: Missing columns:", paste(missing_cols, collapse = ", "))
          data_eq5d5l_upload(NULL)
        } else {
          data_eq5d5l_upload(df_eq5d)
        }
      } else {
        error_msg <- "ERROR: Invalid file. Please upload a .csv or .xlsx file format."
        data_eq5d5l_upload(NULL)
      }
      
    })
    
    output$table_output <- renderTable({
      req(data_eq5d5l_upload())
      data_eq5d5l_upload()
    })
    
    observeEvent(input$btn_calculate6, {
      req(data_eq5d5l_upload())
      df <- data_eq5d5l_upload()
      
      df$EQ5D5L_Profile <- paste(df$mobility, df$selfcare, df$usualactivities, 
                                 df$paindiscomfort, df$anxietydepression, sep="")
      
      df$eq5d5l_index_value <- sapply(df$EQ5D5L_Profile, function(profile_str) {
        match <- profile$values[profile$profile == profile_str]
        if (length(match) == 0) NA else match
      })
      
      df$timezone <- Sys.timezone()
      df$time <- Sys.time()
      
      data_eq5d5l_upload(df)
    })
    
    output$file_download6 <- downloadHandler(
      filename = function() {
        "processed_eq5d5l_data.xlsx"
      },
      content = function(file) {
        req(data_eq5d5l_upload())
        writexl::write_xlsx(data_eq5d5l_upload(), file)
      }
    )
  })
}
