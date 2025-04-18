###############################################################################
# STOICHIOMETRY FUNCTIONS
# Description: Functions for elemental analysis and stoichiometric calculations
# Author: Titouan Dionet
# Date: April 2025
###############################################################################

#' Elements rates determination
#' 
#' @description 
#' This function calculates the mean individual biomass, mean individual element masses, 
#' and element rates in a stage-structured population.
#' 
#' @param population_vector Numeric vector. Vector containing the number or proportion of 
#'        individuals in each class.
#' @param stoichiometry_array Matrix or data frame. An array with at least 2 columns containing 
#'        the mean biomass of individuals in each class and element mean masses in individuals 
#'        of each class. Each studied element should be in a separate column.
#' @param element_names Character vector. Names of the different elements. Default is NULL.
#' 
#' @return A list containing 5 objects:
#'   \item{mass_elem}{Matrix containing biomasses and all element masses for each class.}
#'   \item{percent_elem}{Matrix containing all element rates for each class.}
#'   \item{mass_elem_pop}{Numeric vector containing total biomass and total mass of each element in the population.}
#'   \item{percent_elem_pop_biomass}{Numeric vector containing element rates in the population as percent of total biomass.}
#'   \item{percent_elem_pop_CWM}{Numeric vector containing element rates in the population as community-weighted mean (weighted by the headcount of each class).}
#'
#' @details
#' This function performs various calculations to determine elemental composition at both the 
#' class and population levels:
#' 
#' 1. For each class, it calculates the total mass of each element by multiplying the mean element 
#'    mass per individual by the number of individuals in that class.
#' 2. For each class, it calculates the percentage of each element relative to biomass.
#' 3. For the whole population, it sums the masses across all classes to get total biomass and 
#'    total element masses.
#' 4. For the whole population, it calculates the overall percentage of each element in two ways:
#'    - As a percentage of total biomass (percent_elem_pop_biomass)
#'    - As a community-weighted mean (percent_elem_pop_CWM), which weights the element percentages 
#'      by the relative abundance of each class
#' 
#' @examples
#' \dontrun{
#' # Calculate elemental composition for a population with 5 size classes
#' # Population vector (stable stage distribution)
#' SSD <- c(0.2, 0.3, 0.2, 0.2, 0.1)
#' 
#' # Stoichiometry array with biomass, C, N, P for each class
#' mat_sto <- data.frame(
#'   biomass = c(500, 1500, 2500, 3500, 4500),
#'   massC = c(200, 600, 1000, 1400, 1800),
#'   massN = c(50, 150, 250, 350, 450),
#'   massP = c(6, 15, 25, 35, 45)
#' )
#' 
#' # Calculate elemental composition
#' elem_results <- elem_rates(
#'   population_vector = SSD,
#'   stoichiometry_array = mat_sto,
#'   element_names = c("C", "N", "P")
#' )
#' 
#' # Extract population-level phosphorus percentage
#' P_percentage <- elem_results$percent_elem_pop_biomass["percentP"] * 100
#' }
#' 
#' @export
elem_rates = function(population_vector, stoichiometry_array, element_names = NULL) {
  # Raise an error if the population vector length does not correspond to the stoichiometry array dimensions.
  if (length(population_vector) != dim(stoichiometry_array)[1]) {
    stop(sprintf("The population vector length (%i) does not correspond to the stoichiometry array dimensions (%i lines). Did you forget something?", length(population_vector), dim(stoichiometry_array)[1]))
  }
  
  # Conversion to matrices
  stoichiometry_array = as.matrix(stoichiometry_array)
  population_vector = as.matrix(population_vector)
  
  # Element mass and rates calculation in each class
  mass_elem = stoichiometry_array * population_vector[,1]
  percent_elem = as.matrix(stoichiometry_array[, -1]/stoichiometry_array[, 1]) # "as.matrix" is necessary if stoichiometry_array[, -1] has a dimension of 1.
  
  # Element mass and rates calculation in the population
  mass_elem_pop = apply(mass_elem, 2, sum)
  percent_elem_pop_biomass = mass_elem_pop[-1]/mass_elem_pop[1]
  
  # Element rates calculation in the population in CWM
  percent_elem_pop_CWM = as.vector(t(percent_elem) %*% population_vector)
  
  # Naming 
  if(!is.null(element_names)) {
    # Raise an error if the number of element names does not correspond to the number of elements in the stoichiometry array
    if (length(element_names) != dim(stoichiometry_array)[2]-1) {
      stop(sprintf("The number of element names (%i) does not correspond to the number of elements in the stoichiometry array (%i). Did you forget something?", length(element_names), dim(stoichiometry_array)[2]-1))
    }
    mass_names = c("biomass", paste("mass", element_names, sep = ""))
    percent_names = paste("percent", element_names, sep = "")
    colnames(mass_elem) = mass_names
    colnames(percent_elem) = percent_names
    names(mass_elem_pop) = mass_names
    names(percent_elem_pop_biomass) = percent_names
    names(percent_elem_pop_CWM) = percent_names
  } else {
    mass_names = c("biomass", paste("mass_elem", 1:(dim(stoichiometry_array)[2]-1), sep = ""))
    percent_names = paste("percent_elem", 1:(dim(stoichiometry_array)[2]-1), sep = "")
    colnames(mass_elem) = mass_names
    colnames(percent_elem) = percent_names
    names(mass_elem_pop) = mass_names
    names(percent_elem_pop_biomass) = percent_names
    names(percent_elem_pop_CWM) = percent_names
  }
  
  return(list(mass_elem = mass_elem,
              percent_elem = percent_elem,
              mass_elem_pop = mass_elem_pop,
              percent_elem_pop_biomass = percent_elem_pop_biomass,
              percent_elem_pop_CWM = percent_elem_pop_CWM))
}