###############################################################################
# data_processing.R
# Functions for processing and analyzing data in the Gammarus-Phosphorum project
# Author: Titouan Dionet
# Date: April 2025
###############################################################################

#' Calculate calibration coefficient
#'
#' @description This function calculates the calibration coefficient from a calibration curve
#' using linear regression through the origin.
#'
#' @param calibration_data A data frame containing calibration curve data with
#' columns for concentration and absorbance
#'
#' @return A list containing the coefficient and model summary
#' @export
cal_coef <- function(calibration_data) {
  # Ensure column names are correctly identified
  if (
    "conc" %in% names(calibration_data) && "abs" %in% names(calibration_data)
  ) {
    # If columns are named "conc" and "abs"
    fit <- lm(formula = "conc ~ 0 + abs", data = calibration_data)
  } else if (
    "concentration" %in%
      names(calibration_data) &&
      "absorbance" %in% names(calibration_data)
  ) {
    # If columns are named "concentration" and "absorbance"
    fit <- lm(
      formula = "concentration ~ 0 + absorbance",
      data = calibration_data
    )
  } else {
    # Try to infer column names - assuming first column is conc, second is abs
    col_names <- names(calibration_data)
    fit <- lm(
      formula = paste(col_names[1], "~ 0 +", col_names[2]),
      data = calibration_data
    )
  }

  # Get model summary
  model_summary <- summary(fit)

  # Extract coefficient
  coef_value <- coef(fit)[1]

  # Calculate R²
  r_squared <- model_summary$r.squared

  return(list(
    coef = coef_value,
    r_squared = r_squared,
    model = fit,
    summary = model_summary
  ))
}

#' Convert size classes to ordered factor
#'
#' @description
#' Converts the class column to an ordered factor with predefined levels.
#' If samples with "neo" in their ID are detected, they are assigned to the
#' "neo-J1" class prior to factor conversion.
#'
#' @param data A data table containing a \code{class} column and an \code{id} column
#'
#' @return A data table with the \code{class} column converted to an ordered factor
#' @export
convert_class_to_factor <- function(data) {
  processed_data <- copy(data)

  if (!'class' %in% colnames(processed_data)) {
    warning("No 'class' column found in data. Returning data unchanged.")
    return(processed_data)
  }

  processed_data[grepl("neo", id), class := "neo-J1"]

  processed_data[,
    class := factor(
      class,
      levels = c("neo-J1", "J1", "J2", "A1", "A2", "A3")
    )
  ]

  return(processed_data)
}

#' Calculate phosphorus concentration, mass, and percentage
#'
#' @description
#' Computes phosphorus concentration (µg/ml), total phosphorus mass (µg), and
#' phosphorus as a percentage of dry mass from raw absorbance measurements.
#' Control samples identified by "spinach" in their ID are removed prior to
#' returning the processed data.
#'
#' @param raw_data A data table containing raw phosphorus measurements, with columns:
#'   \code{absorbance}, \code{dilution_factor}, \code{sample_volume_ml},
#'   \code{dry_sample_mass_microg}, and \code{id}
#' @param calib_coef Numeric. Calibration coefficient derived from the standard curve
#'
#' @return A data table with three additional columns: \code{P_conc_microg_ml},
#'   \code{P_mass_microg}, and \code{P_percent}; control samples are excluded
#' @export
calculate_phosphorus <- function(raw_data, calib_coef) {
  processed_data <- copy(raw_data)

  processed_data[,
    P_conc_microg_ml := calib_coef * absorbance * (1 + dilution_factor)
  ]
  processed_data[, P_mass_microg := P_conc_microg_ml * sample_volume_ml / 1000]
  processed_data[, P_percent := P_mass_microg / dry_sample_mass_microg]

  # Remove control samples
  processed_data <- processed_data[!grepl("spinach", id)]

  return(processed_data)
}
