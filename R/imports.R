#' @import shiny
#' @importFrom bslib bs_theme
#' @importFrom ggplot2 ggplot aes geom_histogram geom_line geom_point labs theme_minimal xlim ylim
#' @importFrom tibble tibble
#' @importFrom readxl read_excel
#' @importFrom haven read_sav
#' @importFrom writexl write_xlsx
#' @importFrom graphics abline hist par points text
#' @importFrom stats as.formula lm median quantile rbeta rnorm sd plogis
#' @importFrom utils read.csv write.csv
"_PACKAGE"

utils::globalVariables(c("WTP", "Probability"))
