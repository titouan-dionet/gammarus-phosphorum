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
  if("conc" %in% names(calibration_data) && "abs" %in% names(calibration_data)) {
    # If columns are named "conc" and "abs"
    fit <- lm(formula = "conc ~ 0 + abs", data = calibration_data)
  } else if("concentration" %in% names(calibration_data) && "absorbance" %in% names(calibration_data)) {
    # If columns are named "concentration" and "absorbance"
    fit <- lm(formula = "concentration ~ 0 + absorbance", data = calibration_data)
  } else {
    # Try to infer column names - assuming first column is conc, second is abs
    col_names <- names(calibration_data)
    fit <- lm(formula = paste(col_names[1], "~ 0 +", col_names[2]), data = calibration_data)
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

#' Process raw phosphorus data
#'
#' @description 
#' This function takes raw phosphorus measurement data and a calibration coefficient
#' to calculate phosphorus concentration, mass, and percentage in samples.
#'
#' @param raw_data A data table containing raw phosphorus measurements
#' @param calib_coef Calibration coefficient from standard curve
#'
#' @return A data table with processed phosphorus data
#' @export
process_phosphorus_data <- function(raw_data, calib_coef) {
  # Create a copy to avoid modifying the original data
  processed_data <- copy(raw_data)
  
  # Add neo-J1 class designation
  processed_data[, class := ifelse(grepl("neo", id), "neo-J1", class)]
  
  # Convert class to factor with proper ordering
  processed_data$class <- factor(processed_data$class, 
                                 levels = c("neo-J1", "J1", "J2", "A1", "A2", "A3"))
  
  # Calculate phosphorus concentration and mass
  processed_data[, P_conc_microg_ml := calib_coef * absorbance * (1 + dilution_factor)]  # Concentration in µg/ml
  processed_data[, P_mass_microg := P_conc_microg_ml * sample_volume_ml/1000] # Total P mass in sample (µg)
  processed_data[, P_percent := P_mass_microg/dry_sample_mass_microg] # P percentage of dry mass
  
  # Remove control samples
  processed_data <- processed_data[!grepl("spinach", id),]
  
  return(processed_data)
}