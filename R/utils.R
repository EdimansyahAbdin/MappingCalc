# R/utils.R
# MappingCalc v2.0 -- Core utility calculation functions

#' Estimate EQ-5D-5L Utility Score from PANSS Subscales
#'
#' @description Predicts the EQ-5D-5L utility index value using an OLS
#'   regression model from Abdin et al. (2019). Predicted scores above 1.000
#'   are capped at 1.000.
#'
#' @param positive Numeric. PANSS Positive subscale score (range: 7--49).
#' @param negative Numeric. PANSS Negative subscale score (range: 7--49).
#' @param gps Numeric. PANSS General Psychopathology score (range: 16--112).
#' @param age1 Numeric. Patient age in years (minimum: 21).
#' @param gender Numeric. Gender code: 1 = female, 0 = male.
#'
#' @return Numeric scalar. EQ-5D-5L utility value rounded to 3 decimal places,
#'   capped at a maximum of 1.000.
#'
#' @references
#'   Abdin E, Chong SA, Seow E et al. (2019). Mapping the Positive and
#'   Negative Syndrome Scale scores to EQ-5D-5L and SF-6D utility scores
#'   in patients with schizophrenia. \emph{Quality of Life Research},
#'   28, 177--186. \doi{10.1007/s11136-018-2037-7}
#'
#' @examples
#' panss_utility_score(34, 17, 80, 40, 1)
#' panss_utility_score(positive = 20, negative = 15, gps = 40,
#'                     age1 = 35, gender = 0)
#'
#' @export
panss_utility_score <- function(positive, negative, gps, age1, gender) {
  values10 <- (1.3103 - 0.0044 * positive + 0.0025 * negative -
                 0.0146 * gps - 0.0029 * age1 + 0.0149 * gender)
  values10[values10 > 1.000] <- 1.000
  round(values10, 3)
}

#' Compare PANSS-Predicted Utility to Singapore Population Mean
#'
#' @description Returns whether the predicted EQ-5D-5L utility is above or
#'   below the Singapore population mean of 0.95.
#'
#' @inheritParams panss_utility_score
#'
#' @return Character. \code{"above"} if predicted utility > 0.95,
#'   \code{"below"} otherwise.
#'
#' @examples
#' panss_utility_comparison(34, 17, 80, 40, 1)
#'
#' @export
panss_utility_comparison <- function(positive, negative, gps, age1, gender) {
  values20 <- (1.3103 - 0.0044 * positive + 0.0025 * negative -
                 0.0146 * gps - 0.0029 * age1 + 0.0149 * gender)
  ifelse(values20 > 0.95, "above", "below")
}

#' Estimate EQ-5D-5L Utility Score from SQLS Subscales
#'
#' @description Predicts the EQ-5D-5L utility index value using an OLS
#'   regression model from Seow et al. (2023).
#'
#' @param psychosocial Numeric. SQLS Psychosocial subscale score (range: 0--100).
#' @param motivation   Numeric. SQLS Motivation subscale score (range: 0--100).
#' @param symptoms     Numeric. SQLS Symptoms subscale score (range: 0--100).
#' @param age          Numeric. Patient age in years.
#' @param gender       Numeric. Gender code: 1 = female, 0 = male.
#'
#' @return Numeric scalar. EQ-5D-5L utility value rounded to 3 decimal places.
#'
#' @references
#'   Seow E, Abdin E, Subramaniam M, Chong SA (2023). Mapping the schizophrenia
#'   quality of life scale to EQ-5D, HUI3 and SF-6D utility scores in patients
#'   with schizophrenia. \emph{Expert Review of Pharmacoeconomics and Outcomes
#'   Research}, 23(7), 813--821. \doi{10.1080/14737167.2023.2215430}
#'
#' @examples
#' sqls_utility_score(50, 50, 50, 40, 1)
#'
#' @export
sqls_utility_score <- function(psychosocial, motivation, symptoms, age, gender) {
  values0 <- (1.013 - 0.004 * psychosocial + 0.006 * motivation +
                0.002 * symptoms + 0.00003 * (psychosocial^2) -
                0.0001 * (motivation^2) - 0.0001 * (symptoms^2) -
                0.003 * age + 0.030 * gender)
  round(values0, 3)
}

#' Compare SQLS-Predicted Utility to Singapore Population Mean
#'
#' @description Returns whether the predicted EQ-5D-5L utility is above or
#'   below the Singapore population mean of 0.95.
#'
#' @inheritParams sqls_utility_score
#'
#' @return Character. \code{"above"} if predicted utility > 0.95,
#'   \code{"below"} otherwise.
#'
#' @examples
#' sqls_utility_comparison(50, 50, 50, 40, 1)
#'
#' @export
sqls_utility_comparison <- function(psychosocial, motivation, symptoms, age, gender) {
  values0 <- (1.013 - 0.004 * psychosocial + 0.006 * motivation +
                0.002 * symptoms + 0.00003 * (psychosocial^2) -
                0.0001 * (motivation^2) - 0.0001 * (symptoms^2) -
                0.003 * age + 0.030 * gender)
  values19 <- round(values0, 3)
  ifelse(values19 > 0.95, "above", "below")
}

#' Estimate EQ-5D-5L Utility Score from WHODAS 2.0 Total Score
#'
#' @description Predicts the EQ-5D-5L utility index value from the WHODAS 2.0
#'   12-item total score using a robust regression model from Abdin et al. (2024).
#'   Predicted values below -0.584 are floored at -0.584.
#'
#' @param whodas_scores Numeric. WHODAS 2.0 total score (range: 0--48).
#'
#' @return Numeric scalar. EQ-5D-5L utility value rounded to 3 decimal places,
#'   floored at a minimum of -0.584.
#'
#' @references
#'   Abdin E et al. (2024). Mapping the World Health Organization Disability
#'   Assessment Schedule 2.0 to the EQ-5D-5L in patients with mental disorders.
#'   \emph{Expert Review of Pharmacoeconomics and Outcomes Research}.
#'   \doi{10.1080/14737167.2024.2376100}
#'
#' @examples
#' whodas_total_utility_score(10)
#' whodas_total_utility_score(48)
#'
#' @export
whodas_total_utility_score <- function(whodas_scores) {
  values5 <- (0.9564739 + (-0.0235729 * whodas_scores) +
                (-0.000266 * (whodas_scores^2)))
  values5[values5 < -0.584] <- -0.584
  round(values5, 3)
}

#' Compare WHODAS-Predicted Utility to Singapore Population Mean
#'
#' @description Returns whether the predicted EQ-5D-5L utility is above or
#'   below the Singapore population mean of 0.95.
#'
#' @inheritParams whodas_total_utility_score
#'
#' @return Character. \code{"above"} if predicted utility > 0.95,
#'   \code{"below"} otherwise.
#'
#' @examples
#' whodas_total_utility_comparison(10)
#'
#' @export
whodas_total_utility_comparison <- function(whodas_scores) {
  values5 <- (0.9564739 + (-0.0235729 * whodas_scores) +
                (-0.000266 * (whodas_scores^2)))
  values5[values5 < -0.584] <- -0.584
  values7 <- round(values5, 3)
  ifelse(values7 > 0.95, "above", "below")
}

#' Look Up EQ-5D-5L Index Value from Profile
#'
#' @description Returns the EQ-5D-5L index value for a given five-digit
#'   health state profile using the Singapore value set (Luo et al., 2014)
#'   via the crosswalk method (van Hout et al., 2012).
#'
#' @param Mobility           Integer 1--5. Mobility dimension level.
#' @param SelfCare           Integer 1--5. Self-care dimension level.
#' @param UsualActivities    Integer 1--5. Usual activities dimension level.
#' @param PainDiscomfort     Integer 1--5. Pain/discomfort dimension level.
#' @param AnxietyDepression  Integer 1--5. Anxiety/depression dimension level.
#' @param profile_data       Data frame with columns \code{profile} (character)
#'   and \code{values} (numeric), loaded from the package profile lookup table.
#'
#' @return Numeric scalar. EQ-5D-5L index value for the specified profile.
#'
#' @examples
#' profile_file <- system.file("extdata", "profile.csv",
#'                             package = "MappingCalc")
#' profile_data <- utils::read.csv(profile_file)
#' eq5d5l_index_value(1, 1, 1, 1, 1, profile_data)
#' eq5d5l_index_value(3, 3, 3, 3, 3, profile_data)
#'
#' @export
eq5d5l_index_value <- function(Mobility, SelfCare, UsualActivities,
                                PainDiscomfort, AnxietyDepression, profile_data) {
  profile_str <- paste(Mobility, SelfCare, UsualActivities,
                       PainDiscomfort, AnxietyDepression, sep = "")
  profile_data$values[profile_data$profile == profile_str]
}

#' Compare EQ-5D-5L Profile Value to Singapore Population Mean
#'
#' @description Returns whether the index value for a given EQ-5D-5L health
#'   state profile is above or below the Singapore population mean of 0.95.
#'
#' @inheritParams eq5d5l_index_value
#'
#' @return Character. \code{"above"} if index value > 0.95, \code{"below"}
#'   otherwise.
#'
#' @examples
#' profile_file <- system.file("extdata", "profile.csv",
#'                             package = "MappingCalc")
#' profile_data <- utils::read.csv(profile_file)
#' eq5d5l_index_comparison(1, 1, 1, 1, 1, profile_data)
#' eq5d5l_index_comparison(5, 5, 5, 5, 5, profile_data)
#'
#' @export
eq5d5l_index_comparison <- function(Mobility, SelfCare, UsualActivities,
                                     PainDiscomfort, AnxietyDepression, profile_data) {
  profile_str <- paste(Mobility, SelfCare, UsualActivities,
                       PainDiscomfort, AnxietyDepression, sep = "")
  val <- profile_data$values[profile_data$profile == profile_str]
  ifelse(val > 0.95, "above", "below")
}

#' Build EQ-5D-5L Profile String
#'
#' @description Concatenates the five EQ-5D-5L dimension levels into a
#'   five-digit profile string (e.g., \code{"11111"} for full health).
#'
#' @inheritParams eq5d5l_index_value
#'
#' @return Character scalar. Five-digit EQ-5D-5L profile string.
#'
#' @examples
#' eq5d5l_profile_string(1, 2, 3, 2, 1)
#' eq5d5l_profile_string(3, 3, 3, 3, 3)
#'
#' @export
eq5d5l_profile_string <- function(Mobility, SelfCare, UsualActivities,
                                   PainDiscomfort, AnxietyDepression) {
  paste(Mobility, SelfCare, UsualActivities,
        PainDiscomfort, AnxietyDepression, sep = "")
}
