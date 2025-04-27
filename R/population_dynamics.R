###############################################################################
# POPULATION DYNAMICS FUNCTIONS
# Description: Functions for matrix population modeling in Gammarus fossarum
# Author: Titouan Dionet
# Date: April 2025
###############################################################################

# _____________________________________________________________________________
# GROWTH RATES MATRIX FUNCTION
# _____________________________________________________________________________

#' Population growth rates matrix determination
#' 
#' @description 
#' This function determines the transition matrix from one size class to another for individuals 
#' of a population of gammarus whose growth is governed by a temperature-dependent logistic model.
#' 
#' @param N_ind Integer. Total number of individuals in each class needed for determining 
#'        the transition rate (default: 10,000).
#' @param L_max Numeric. Maximum size of individuals for the logistic growth model.
#' @param delta_t Numeric. Time step in days.
#' @param theta Numeric. Target temperature in Celsius.
#' @param class_lim Numeric vector. Boundaries of the different classes; note that the upper limit 
#'        must be specified.
#' @param class_names Character vector. Names of the different classes (default: \code{NULL}).
#' @param growth_rate_coef Numeric. Coefficient for temperature in growth rate equation (default: 0.0014).
#' @param growth_rate_intercept Numeric. Intercept in growth rate equation (default: -0.0024).
#' 
#' @return A data frame representing the transition matrix for the given parameters, with rows and
#'         columns named according to class_names if provided.
#'
#' @details
#' The function calculates transition probabilities between size classes based on a logistic growth model
#' where the growth rate depends on temperature according to the equation: r(θ) = 0.0014*θ - 0.0024.
#' The size of individuals after the time step delta_t is calculated using the logistic equation:
#' L(t) = L_max / (1 + ((L_max/L_init) - 1) * exp(-r*t))
#' 
#' @examples
#' \dontrun{
#' # Calculate transition matrix for gammarus with 5 size classes
#' class_lim <- c(1.5, 3.5, 5.2, 6, 7, 11)
#' class_names <- c("J1", "J2", "A1", "A2", "A3")
#' trans_mat <- growth_rates_matrix(
#'   L_max = 8.5, 
#'   delta_t = 30, 
#'   theta = 12, 
#'   class_lim = class_lim, 
#'   class_names = class_names
#' )
#' }
#' 
#' @export
growth_rates_matrix <- function(
    N_ind = 10000, L_max, delta_t, theta, class_lim, class_names = NULL,
    growth_rate_coef = 0.0014, growth_rate_intercept = -0.0024) {
  
  # Raise an error if the number of class names does not correspond to the number of class limit intervals.
  if (!is.null(class_names) & length(class_lim)-1 != length(class_names)) {
    stop(sprintf("The number of class names (%i) does not correspond to the number of class limit intervals (%i): 'length(class_names) != length(class_lim)-1'. Did you forget one class limit (usually the upper limit)?", length(class_names), length(class_lim)-1))
  }
  
  # Initialization
  MAT = data.frame(matrix(data = 0, 
                          nrow = length(class_lim)-1, 
                          ncol = length(class_lim)-1, 
                          dimnames = list(class_names, class_names)))
  
  for (cla in 1:(length(class_lim)-1)) {
    size_init = seq(class_lim[cla], class_lim[cla+1], length.out = N_ind)
    
    # Calculate growth rate
    growth_rate = growth_rate_coef * theta + growth_rate_intercept
    
    size_end = L_max / (1 + ((L_max/size_init) - 1) * exp(-growth_rate * delta_t))
    
    for (i in 1:(length(class_lim)-1)) {
      MAT[i, cla] = length(which(size_end >= class_lim[i] & size_end < class_lim[i+1]))/N_ind
    }
  }
  return(MAT)
}

#' Calculate transition rates for a range of temperatures
#'
#' @description 
#' This function calculates transition rates between size classes for a range of temperatures
#' using the growth_rates_matrix function.
#'
#' @param temp_range Numeric vector of temperatures to calculate transition rates for
#' @param L_max Maximum size in mm
#' @param delta_t Time step in days
#' @param class_lim Vector of size class limits
#' @param class_names Vector of size class names
#'
#' @return A data frame in long format containing transition rates for each temperature
#' 
#' @export
#' @importFrom dplyr mutate
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
calculate_transition_rates <- function(temp_range, L_max, delta_t, class_lim, class_names) {
  # Initialize empty data frame
  data_for_plot <- data.frame()
  
  # Calculate transition rates for each temperature
  for (theta in temp_range) {
    # Calculate transition matrix
    trans_mat <- growth_rates_matrix(
      L_max = L_max,
      delta_t = delta_t,
      theta = theta,
      class_lim = class_lim,
      class_names = class_names
    )
    
    # Convert to long format
    temp_data <- as.data.frame(trans_mat) |> 
      tibble::rownames_to_column(var = "X") |> 
      tidyr::pivot_longer(cols = -X, names_to = "Y", values_to = "Z") |> 
      dplyr::mutate(theta = theta)
    
    # Add to result data frame
    data_for_plot <- rbind(data_for_plot, temp_data)
  }
  
  # Ensure proper factor ordering
  data_for_plot$X <- factor(data_for_plot$X, levels = class_names)
  data_for_plot$Y <- factor(data_for_plot$Y, levels = class_names)
  
  return(data_for_plot)
}

# _____________________________________________________________________________
# FECONDITY RATES MATRIX FUNCTION
# _____________________________________________________________________________

#' Population fecundity rates matrix determination
#' 
#' @description 
#' This function determines the fecundity matrix for a population of gammarus.
#' 
#' @param sexratio Numeric. Sex ratio in the population (proportion of females).
#' @param gravid Numeric. Proportion of gravid females among the females.
#' @param fertil Numeric vector. Number of embryos per female for each class, note that ALL classes 
#'        must be taken into account.
#' @param delta_t Numeric. Time step in days.
#' @param theta Numeric. Target temperature in Celsius.
#' @param class_names Character vector. Names of the different classes (default: \code{NULL}).
#' @param molt_cycle_a Numeric. Parameter 'beta_1' in molt cycle equation (default: 30.61).
#' @param molt_cycle_b Numeric. Parameter 'alpha_1' in molt cycle equation (default: -0.39).
#' @param molt_cycle_c Numeric. Parameter 'beta_2' in molt cycle equation (default: 0.01).
#' @param molt_cycle_d Numeric. Parameter 'alpha_2' in molt cycle equation (default: 0.05).
#'  
#' @return A data frame representing the fecundity matrix for the given parameters, with rows and
#'         columns named according to class_names if provided.
#'
#' @details
#' The function creates a fecundity matrix where only the first row contains non-zero values,
#' representing the production of juveniles (first size class) by adults of different classes.
#' The molting cycle duration (which affects fecundity) is calculated based on temperature using:
#' `d = (m_a + m_b * \u03b8) / (m_c + m_d * \u03b8)`
#' 
#' @examples
#' \dontrun{
#' # Calculate fecundity matrix for gammarus with 5 size classes
#' fertil <- c(0, 0, 3.6, 5.1, 9.2)  # Only adults (last 3 classes) can reproduce
#' class_names <- c("J1", "J2", "A1", "A2", "A3")
#' feco_mat <- fecondity_rates_matrix(
#'   sexratio = 0.5,
#'   gravid = 0.5,
#'   fertil = fertil,
#'   delta_t = 30,
#'   theta = 12,
#'   class_names = class_names
#' )
#' }
#' 
#' @export
fecondity_rates_matrix <- function(
    sexratio, gravid, fertil, delta_t, theta, class_names = NULL,
    molt_cycle_a = 30.61, molt_cycle_b = -0.39, molt_cycle_c = 0.01, molt_cycle_d = 0.05) {
  
  # Raise an error if number of class names does not correspond to the number of fertility rates.
  if (!is.null(class_names) & length(fertil) != length(class_names)) {
    stop(sprintf("The number of class names (%i) does not correspond to the number of fertility rates (%i): 'length(class_names) != length(fertil)'. Did you forget something?", length(class_names), length(fertil)))
  }
  
  # Initialization
  MAT = data.frame(matrix(data = 0, 
                          nrow = length(fertil), 
                          ncol = length(fertil), 
                          dimnames = list(class_names, class_names)))
  
  # Molting cycle duration (= reproduction cycle)
  mold_cycle_time = (molt_cycle_a + molt_cycle_b * theta) / (molt_cycle_c + molt_cycle_d * theta)
  
  MAT[1,] = sexratio * fertil * gravid * delta_t / mold_cycle_time
  
  return(MAT)
}

# _____________________________________________________________________________
# SURVIVAL RATES MATRIX FUNCTION
# _____________________________________________________________________________

#' Survival rates matrix determination
#' 
#' @description 
#' This function transforms a vector containing the survival rates of each class of a population 
#' of gammarus into a matrix.
#' 
#' @param survival_rates Numeric vector. Survival rates for each class, ranging from 0 to 1.
#'        Note that ALL classes must be taken into account.
#' @param class_names Character vector. Names of the different classes (default: \code{NULL}).
#' 
#' @return A data frame representing the survival rates matrix for the given survival rates,
#'         with rows and columns named according to class_names if provided.
#'
#' @details
#' The function creates a diagonal matrix where each row contains the same survival rate
#' repeated across all columns. This matrix is later used in combination with the transition
#' and fecundity matrices to create the complete Leslie matrix.
#' 
#' @examples
#' \dontrun{
#' # Calculate survival matrix for gammarus with 5 size classes
#' surv_rates <- c(0.8, 0.85, 0.78, 0.64, 0.39)
#' class_names <- c("J1", "J2", "A1", "A2", "A3")
#' surv_mat <- survival_rates_matrix(
#'   survival_rates = surv_rates,
#'   class_names = class_names
#' )
#' }
#' 
#' @export
survival_rates_matrix = function(survival_rates, class_names = NULL) {
  # Raise an error if number of class names does not correspond to the number of survival rates.
  if (!is.null(class_names) & length(survival_rates) != length(class_names)) {
    stop(sprintf("The number of class names (%i) does not correspond to the number of survival rates (%i): 'length(class_names) != length(survival_rates)'. Did you forget something?", length(class_names), length(survival_rates)))
  }
  
  # Transformation
  MAT = data.frame(matrix(data = survival_rates, 
                          nrow = length(survival_rates), 
                          ncol = length(survival_rates), byrow = T,
                          dimnames = list(class_names, class_names)))
  
  return(MAT)
}

#' Extract Sub-Matrix for a Specific Temperature
#' 
#' @description 
#' This function extracts a subset of rows from a matrix that corresponds to a specific 
#' temperature value when matrices for multiple temperatures have been concatenated.
#' 
#' @param matrix_data Matrix or data frame. The concatenated matrix containing data for multiple temperatures.
#' @param temps Numeric vector. Vector of temperature values corresponding to the order of concatenation in the matrix.
#' @param current_temp Numeric. The specific temperature value for which to extract the sub-matrix.
#' 
#' @return A matrix containing only the rows corresponding to the specified temperature.
#'
#' @details
#' When using targets with dynamic branching, matrices for different temperature values are 
#' often concatenated into a single matrix. This function extracts the subset of rows 
#' that correspond to a specific temperature value.
#' 
#' The function assumes that:
#' 1. The matrix rows are organized by temperature
#' 2. Each temperature has the same number of rows
#' 3. Temperatures are in the same order as provided in the 'temps' parameter
#' 
#' @examples
#' \dontrun{
#' # Assuming matrices have been concatenated for temps = c(8, 12, 16)
#' # Extract matrix for temperature = 12
#' sub_matrix <- extract_matrix_for_temp(
#'   matrix_data = concatenated_matrix,
#'   temps = c(8, 12, 16),
#'   current_temp = 12
#' )
#' }
#' 
#' @export
extract_matrix_for_temp <- function(matrix_data, temps, current_temp) {
  # Find the index of the current temperature in the temperature list
  temp_index <- match(current_temp, temps)
  
  # Calculate start and end indices for rows
  rows_per_temp <- nrow(matrix_data) / length(temps)
  start_row <- (temp_index - 1) * rows_per_temp + 1
  end_row <- temp_index * rows_per_temp
  
  # Extract the corresponding rows
  sub_matrix <- matrix_data[start_row:end_row, ]
  
  # Restore row names to correct values
  rownames(sub_matrix) <- paste0("Class", 1:rows_per_temp)
  
  return(sub_matrix)
}

# _____________________________________________________________________________
# LESLIE MATRIX FUNCTION
# _____________________________________________________________________________

#' Leslie matrix determination
#' 
#' @description 
#' This function calculates the Leslie matrix of a population of gammarus based on 
#' the transition, fecundity, and survival matrices.
#' 
#' @param transition_matrix Matrix or data frame. Growth rates matrix.
#' @param fecondity_matrix Matrix or data frame. Fecundity rates matrix.
#' @param survival_matrix Matrix or data frame. Survival rates matrix.
#' @param class_names Character vector. Names of the different classes. Default is NULL.
#' 
#' @return A data frame representing the Leslie matrix for the given parameters.
#'
#' @details
#' The function combines the transition, fecundity, and survival matrices to create a complete
#' Leslie matrix. The formula used is:
#' `Leslie_matrix = transition_matrix * survival_matrix + fecondity_matrix * sqrt(survival_matrix * survival_matrix[1, 1])`
#' 
#' The survival rate of newborns is modeled as the square root of the product of the survival 
#' rates of adults and juveniles of the first class.
#' 
#' @examples
#' \dontrun{
#' # Calculate a complete Leslie matrix for gammarus
#' trans_mat <- growth_rates_matrix(L_max = 10, delta_t = 30, theta = 12, 
#'                                  class_lim = c(1, 2, 3), 
#'                                  class_names = c("C1", "C2", "C3"))
#' feco_mat <- fecondity_rates_matrix(sexratio = 0.5, gravid = 0.5, 
#'                                   fertil = c(0, 0, 2),
#'                                   delta_t = 30, theta = 12, 
#'                                   class_names = c("C1", "C2", "C3"))
#' surv_mat <- survival_rates_matrix(survival_rates = c(0.8, 0.5, 0.3),
#'                                  class_names = c("C1", "C2", "C3"))
#' leslie_mat <- Leslie_matrix(transition_matrix = trans_mat,
#'                           fecondity_matrix = feco_mat,
#'                           survival_matrix = surv_mat,
#'                           class_names = c("C1", "C2", "C3"))
#' }
#' 
#' @export
Leslie_matrix = function(transition_matrix, fecondity_matrix, survival_matrix, class_names = NULL) {
  # Raise error if the different matrices do not have the same dimensions.
  if (!(setequal(dim(transition_matrix), dim(fecondity_matrix)) & setequal(dim(transition_matrix), dim(survival_matrix)))) {
    stop(sprintf("One or more matrices do not have the same dimensions (transition matrix: (%d, %d), fecondity matrice: (%d, %d), survival matrice: (%d, %d)). Please verify the differents matrices.", 
                 dim(transition_matrix)[1], dim(transition_matrix)[2],
                 dim(fecondity_matrix)[1], dim(fecondity_matrix)[2], 
                 dim(survival_matrix)[1], dim(survival_matrix)[2]))
  }
 
  # Leslie matrix
  MAT = transition_matrix * survival_matrix + fecondity_matrix * sqrt(survival_matrix * survival_matrix[1, 1])
  # The survival rate of newborns is the square root of the product of the survival rates of adults and juveniles of first class.
  
  return(MAT)
}

# _____________________________________________________________________________
# FIND LAMBDA AND SSD FUNCTION
# _____________________________________________________________________________

#' Determination of lambda and stable stage distribution
#' 
#' @description 
#' This function determines the asymptotic growth rate (lambda) and the stable stage distribution (SSD) of 
#' a Leslie population matrix.
#' 
#' @param Leslie_matrix Matrix or data frame. Leslie population matrix. It must be a square matrix.
#' @param class_names Character vector. Names of the different classes (default: \code{NULL}).
#' 
#' @return A list containing two elements:
#'   \item{lambda}{Numeric value representing the asymptotic growth rate of the given matrix.}
#'   \item{SSD}{Numeric vector representing the stable stage distribution of the given matrix.}
#'
#' @details
#' The function calculates the dominant eigenvalue (lambda) and corresponding eigenvector (SSD) of
#' the Leslie matrix. Lambda represents the asymptotic growth rate of the population, while the
#' normalized eigenvector represents the stable stage distribution - the proportion of individuals
#' in each size class when the population reaches equilibrium.
#' 
#' @examples
#' \dontrun{
#' # Calculate lambda and SSD for a Leslie matrix
#' lambda_SSD <- find_lambda_SSD(Leslie_matrix = leslie_mat,
#'                              class_names = c("J1", "J2", "A1", "A2", "A3"))
#' lambda <- lambda_SSD$lambda
#' SSD <- lambda_SSD$SSD
#' }
#' 
#' @export
find_lambda_SSD = function(Leslie_matrix, class_names = NULL) {
  # Raise an error if the given matrix is not square.
  if (dim(Leslie_matrix)[1] != dim(Leslie_matrix)[2]) {
    stop(sprintf("The given matrix is not square (dimensions : %i, %i).", dim(Leslie_matrix)[1], dim(Leslie_matrix)[2]))
  }
  
  # Raise an error if the number of class names does not correspond to the Leslie matrix dimension.
  if (!is.null(class_names) & length(class_names) != dim(Leslie_matrix)[1]) {
    stop(sprintf("The number of class names (%i) does not correspond to the Leslie matrix dimension (%i). Did you forget something?", length(class_names), dim(Leslie_matrix)[1]))
  }
  
  # Position of the maximal eigenvalue of the Leslie matrix
  pos_val_max = which.max(Re(eigen(Leslie_matrix)[['values']]))
  
  # Asymptotic growth rate (lambda) of the Leslie matrix
  lambda = Re(eigen(Leslie_matrix)[["values"]][pos_val_max])
  
  # Left eigenvector of the Leslie matrix
  w = abs(Re(eigen(Leslie_matrix)[["vectors"]][,pos_val_max]))
  # w needs to be converted to positive values for the calculation of the SSD

  # Stable stage distribution (SSD) of the Leslie matrix
  SSD = w/sum(w)
  names(SSD) = class_names
  
  return(list(lambda = lambda, SSD = SSD))
}

# _____________________________________________________________________________
# ELASTICITY MATRIX FUNCTION
# _____________________________________________________________________________

#' Elasticity matrix determination
#' 
#' @description 
#' This function calculates the elasticity matrix of a given Leslie matrix.
#' 
#' @param Leslie_matrix Matrix or data frame. A Leslie matrix.
#' 
#' @return A numeric matrix representing the elasticity matrix of the given Leslie matrix.
#'
#' @details
#' The elasticity matrix quantifies how sensitive the asymptotic growth rate (lambda) is to
#' proportional changes in each element of the Leslie matrix. The calculation involves:
#' 1. Finding the dominant eigenvalue (lambda) and right eigenvector (w)
#' 2. Finding the left eigenvector (v) by transposing the Leslie matrix
#' 3. Calculating the sensitivity matrix as (v ⊗ w) / <v,w>
#' 4. Converting to elasticity: (a_ij/λ) * sensitivity_ij
#'
#' Where a_ij is the element in row i, column j of the Leslie matrix.
#' 
#' @examples
#' \dontrun{
#' # Calculate elasticity matrix
#' elasti_mat <- elasticity_matrix(Leslie_matrix = leslie_mat)
#' 
#' # Calculate elasticities by category
#' survival_elasticity <- sum(diag(as.matrix(elasti_mat)))
#' growth_elasticity <- sum(elasti_mat[lower.tri(elasti_mat)])
#' fecondity_elasticity <- sum(elasti_mat[upper.tri(elasti_mat)])
#' }
#' 
#' @export
elasticity_matrix = function(Leslie_matrix) {
  # Eigenvalue and right eigenvector
  dominant_eigenvalue_index = which.max(Re(eigen(Leslie_matrix)[["values"]])) # where is the dominant eigenvalue
  dominant_eigenvalue = Re(eigen(Leslie_matrix)[["values"]][dominant_eigenvalue_index]) # i.e. lambda
  right_eigenvector = Re(eigen(Leslie_matrix)[["vectors"]][, dominant_eigenvalue_index]) # i.e. vector w
  
  # Left eigenvector
  transposed_Leslie_matrix = t(Leslie_matrix)
  dominant_eigenvalue_index = which.max(Re(eigen(transposed_Leslie_matrix)[["values"]])) # where is the dominant eigenvalue of the transposed matrix
  left_eigenvector = Re(eigen(transposed_Leslie_matrix)[["vectors"]][, dominant_eigenvalue_index]) # i.e. vector v
  
  # Sensibility matrix
  sensibility_matrix = (left_eigenvector %*% t(right_eigenvector)) / as.numeric(left_eigenvector %*% right_eigenvector)
  
  # Elasticity matrix
  elasticity_matrix = (Leslie_matrix/dominant_eigenvalue) * sensibility_matrix
  
  return(elasticity_matrix)
}