# inst/app/app.R
# MappingCalc v2.0 -- Shiny application entry point

library(MappingCalc)

ui <- shiny::fluidPage(
  theme = bslib::bs_theme(bootswatch = "minty"),
  shiny::navbarPage(
    "MAPPING CALCULATOR (MappingCalc v2.0)",

    shiny::tabPanel("HOME",
      shiny::navlistPanel(widths = c(2, 10),
        shiny::tabPanel("ABOUT",
          shiny::HTML("<p style='color: green;'>
            The EuroQol Five-Dimension (EQ-5D) is the most widely used
            generic preference-based measure for assessing health-related
            quality of life in clinical studies. This application provides
            validated calculators for PANSS, SQLS, WHODAS 2.0, PHQ-8,
            and EQ-5D-5L instruments.
          </p>"),
          shiny::tags$figure(
            shiny::tags$img(src = "qrcode_resize.jpg", width = 200),
            shiny::tags$figcaption("https://eastats.shinyapps.io/MappingCalc")
          ),
          shiny::HTML("<p style='color: grey; font-size: 0.85em;'>
            Disclaimer: For informational and educational purposes only.
            Not intended as medical advice.
          </p>"),
          shiny::HTML("<p style='color: grey; font-size: 0.85em;'>
            Maintainer: Edimansyah Abdin
            (edimansyah.bin.abdin@gmail.com)
          </p>")
        )
      )
    ),

    MappingCalc::mod_eq_panss_ui("panss_module"),
    MappingCalc::mod_eq_sqls_ui("sqls_module"),
    MappingCalc::mod_eq_whodas_ui("whodas_module"),
    MappingCalc::mod_eq_phq8_ui("phq8_module"),
    MappingCalc::mod_eq_5d_5l_ui("eq5d5l_module"),
    MappingCalc::Analyses_ui("Analyses")
  )
)

server <- function(input, output, session) {
  MappingCalc::mod_eq_panss_server("panss_module")
  MappingCalc::mod_eq_sqls_server("sqls_module")
  MappingCalc::mod_eq_whodas_server("whodas_module")
  MappingCalc::mod_eq_phq8_server("phq8_module")
  MappingCalc::mod_eq_5d_5l_server("eq5d5l_module")
  MappingCalc::Analyses_server("Analyses")
}

shiny::shinyApp(ui = ui, server = server)
