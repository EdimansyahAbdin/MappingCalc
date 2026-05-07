#' Launch the MappingCalc Shiny Application
#'
#' @description Launches the MappingCalc interactive Shiny calculator in the
#'   default web browser. Provides validated mapping calculators for PANSS,
#'   SQLS, WHODAS 2.0, PHQ-8, and EQ-5D-5L instruments, plus a Data Analysis
#'   module for descriptive statistics, linear regression, cost-utility
#'   analysis, and probabilistic sensitivity analysis.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}},
#'   such as \code{port} or \code{host}.
#'
#' @return Called for its side effect of launching a Shiny application.
#'   Returns the value of \code{shiny::runApp()} invisibly.
#'
#' @export
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
run_app <- function(...) {
  app_dir <- system.file("app", package = "MappingCalc")
  if (app_dir == "") {
    stop("Cannot find app directory. Try reinstalling MappingCalc.",
         call. = FALSE)
  }
  shiny::runApp(app_dir, launch.browser = TRUE, ...)
}
