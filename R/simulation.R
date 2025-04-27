###############################################################################
# simulation.R
# Simulation utility functions for the Gammarus-Phosphorum project
# Author: Titouan Dionet
# Date: April 2025
###############################################################################

#' Run a single population simulation
#'
#' @description 
#' This function runs a single population dynamics simulation with the given parameters
#' and returns population metrics (growth rate, SSD, phosphorus content)
#'
#' @param theta Temperature in Celsius
#' @param surv_rates Vector of survival rates for each size class
#' @param transition_matrix Growth transition matrix
#' @param fecondity_matrix Fecundity matrix
#' @param class_names Vector of size class names
#' @param stoichiometry_array Matrix with stoichiometric data
#' @param element_names Vector of element names
#'
#' @return A data.table with simulation results
#' @export
run_single_simulation <- function(theta, surv_rates, transition_matrix, fecondity_matrix, 
                                  class_names, stoichiometry_array, element_names) {
  # Calculate survival matrix and Leslie matrix
  surv_mat <- survival_rates_matrix(
    survival_rates = surv_rates, 
    class_names = class_names
  )
  
  mat_dyn <- Leslie_matrix(
    transition_matrix = transition_matrix, 
    fecondity_matrix = fecondity_matrix,
    survival_matrix = surv_mat, 
    class_names = class_names
  )
  
  # Calculate asymptotic growth rate and stable stage distribution
  lambda_SSD <- find_lambda_SSD(mat_dyn, class_names = class_names)
  lambda <- lambda_SSD[[1]]
  SSD <- lambda_SSD[[2]]
  
  # Calculate stoichiometry at population level
  e_rates <- elem_rates(
    population_vector = SSD, 
    stoichiometry_array = stoichiometry_array, 
    element_names = element_names
  )
  
  # Extract biomass and elemental percentages
  biomass <- e_rates[['mass_elem_pop']]['biomass']
  percent_pop <- e_rates[['percent_elem_pop_biomass']]
  percent_CWM <- e_rates[['percent_elem_pop_CWM']]
  
  # Store results in data table
  results <- data.table(
    # Parameters
    theta = theta,
    surv_rate_J1 = surv_rates[1], 
    surv_rate_J2 = surv_rates[2], 
    surv_rate_A1 = surv_rates[3], 
    surv_rate_A2 = surv_rates[4], 
    surv_rate_A3 = surv_rates[5],
    
    # Results - population dynamics
    lambda = lambda,
    SSD_J1 = SSD[1], 
    SSD_J2 = SSD[2], 
    SSD_A1 = SSD[3], 
    SSD_A2 = SSD[4], 
    SSD_A3 = SSD[5],
    
    # Results - stoichiometry
    mean_biomass = biomass
  )
  
  # Results - stoichiometry
  for (el in element_names) {
    results[[paste0("mean_percent", el)]] <- percent_pop[[paste0("percent", el)]]
    results[[paste0("CWM_percent", el)]] <- percent_CWM[[paste0("percent", el)]]
  }
  
  return(results)
}

#' Generate parameter combinations for single parameter simulations
#'
#' @description 
#' This function generates parameter combinations for single parameter simulations
#' where one parameter varies while others stay constant
#'
#' @param temps Vector of temperatures to simulate
#' @param classes Vector of size classes
#' @param base_surv_rates Vector of base survival rates
#' @param n_iter Number of iterations (parameter values) per combination
#'
#' @return A data.table with parameter combinations
#' @export
generate_single_param_combinations <- function(temps, classes, base_surv_rates, n_iter = 1000) {
  combinations <- data.table()
  
  # Loop through temperatures
  for (temp in temps) {
    # Loop through classes
    for (i in seq_along(classes)) {
      class_name <- classes[i]
      
      # Loop through parameter values
      for (j in 1:n_iter) {
        # Create parameter set
        param_set <- data.table(
          temp = temp,
          focal_class = class_name,
          focal_class_index = i,
          iter = j,
          param_value = j/n_iter  # Linear increase from ~0 to 1
        )
        
        # Add to combinations
        combinations <- rbind(combinations, param_set)
      }
    }
  }
  
  return(combinations)
}

#' Generate random parameter sets for multi-parameter simulations
#'
#' @description 
#' This function generates random parameter sets for multi-parameter simulations
#' where all parameters vary simultaneously
#'
#' @param temps Vector of temperatures to simulate
#' @param n_iter Number of random parameter sets per temperature
#' @param min_val Minimum parameter value
#' @param max_val Maximum parameter value
#'
#' @return A data.table with random parameter sets
#' @export
generate_multi_param_combinations <- function(temps, n_iter = 10000, min_val = 0.001, max_val = 1) {
  combinations <- data.table()
  
  # Set random seed for reproducibility
  set.seed(1)
  
  # Loop through temperatures
  for (temp in temps) {
    # Generate random parameter sets
    for (i in 1:n_iter) {
      # Random survival rates
      surv_rates <- runif(5, min = min_val, max = max_val)
      
      # Create parameter set
      param_set <- data.table(
        temp = temp,
        iter = i,
        surv_rate_J1 = surv_rates[1],
        surv_rate_J2 = surv_rates[2],
        surv_rate_A1 = surv_rates[3],
        surv_rate_A2 = surv_rates[4],
        surv_rate_A3 = surv_rates[5]
      )
      
      # Add to combinations
      combinations <- rbind(combinations, param_set)
    }
  }
  
  return(combinations)
}

#' Sample parameter sets for elasticity analysis
#'
#' @description 
#' This function samples parameter sets from multi-parameter results for elasticity analysis
#'
#' @param multi_param_results Results from multi-parameter simulations
#' @param n_samples Number of parameter sets to sample
#'
#' @return A data.table with sampled parameter sets
#' @export
sample_elasticity_parameters <- function(multi_param_results, n_samples = 10000) {
  # Set random seed for reproducibility
  set.seed(1)
  
  # Sample indices from multi-parameter results
  sample_indices <- sample(1:nrow(multi_param_results), n_samples)
  sampled_results <- multi_param_results[sample_indices]
  
  return(sampled_results)
}

#' Calculate elasticity for different demographic parameters
#'
#' @description 
#' This function calculates elasticity for a given parameter set by reducing
#' survival, fecundity, or growth rates by 10% and measuring the effect on lambda and phosphorus
#'
#' @param param_set A single parameter set
#' @param class_names Vector of size class names
#' @param transition_matrix Growth transition matrix
#' @param fecondity_matrix Fecundity matrix
#' @param stoichiometry_array Matrix with stoichiometric data
#' @param element_names Vector of element names
#' @param parameter_type Type of parameter to modify: "survival", "fecundity", or "growth"
#'
#' @return A data.table with elasticity results
#' @export
calculate_comprehensive_elasticity <- function(param_set, class_names, transition_matrix, 
                                               fecondity_matrix, stoichiometry_array, element_names,
                                               parameter_type = "survival") {
  # Extract parameters
  theta <- param_set$theta
  lambda0 <- param_set$lambda
  percentP0 <- param_set$mean_percentP
  
  surv_rates <- c(
    param_set$surv_rate_J1,
    param_set$surv_rate_J2,
    param_set$surv_rate_A1,
    param_set$surv_rate_A2,
    param_set$surv_rate_A3
  )
  
  # Initialize results
  elasticity_results <- data.table()
  
  # For each class, reduce parameter by 10% and calculate effect
  for (j in seq_along(class_names)) {
    # Initialize matrices with original values
    mod_surv_rates <- surv_rates
    mod_transition_matrix <- transition_matrix
    mod_fecondity_matrix <- fecondity_matrix
    
    # Modify the appropriate parameter based on parameter_type
    if (parameter_type == "survival") {
      # Modify survival rate (as in original function)
      mod_surv_rates[j] <- surv_rates[j] * 0.9  # 10% reduction
      parameter_name <- paste0("surv_", class_names[j])
      
    } else if (parameter_type == "fecundity") {
      # Only apply to classes that have fecundity values > 0
      if (sum(fecondity_matrix[1, j]) > 0) {
        # Modify fecundity matrix
        mod_fecondity_matrix[1, j] <- fecondity_matrix[1, j] * 0.9  # 10% reduction
        parameter_name <- paste0("feco_", class_names[j])
      } else {
        # Skip classes with zero fecundity
        next
      }
      
    } else if (parameter_type == "growth") {
      # Modify growth transition matrix
      # We reduce all transition probabilities FROM this class by 10%
      # We need to maintain column sums = 1
      for (i in 1:nrow(transition_matrix)) {
        if (i == j) {
          # Calculate new diagonal element to maintain column sum
          # First, sum all off-diagonal elements in the column
          off_diag_sum <- sum(transition_matrix[, j]) - transition_matrix[j, j]
          
          # Reduce the off-diagonal sum by 10%
          reduced_off_diag_sum <- off_diag_sum * 0.9
          
          # Calculate how much the off-diagonal sum was reduced by
          reduction_amount <- off_diag_sum - reduced_off_diag_sum
          
          # Increase diagonal element by this amount to maintain column sum
          mod_transition_matrix[j, j] <- transition_matrix[j, j] + reduction_amount
        } else if (i > j) {
          # Reduce transition probabilities to other classes by 10%
          # (only elements below the diagonal, which represent growth)
          mod_transition_matrix[i, j] <- transition_matrix[i, j] * 0.9
        }
        # Elements above diagonal (i < j) should remain 0
      }
      parameter_name <- paste0("growth_", class_names[j])
    }
    
    # Calculate modified survival matrix
    mod_surv_mat <- survival_rates_matrix(
      survival_rates = mod_surv_rates,
      class_names = class_names
    )
    
    # Calculate modified Leslie matrix
    mod_leslie_mat <- Leslie_matrix(
      transition_matrix = mod_transition_matrix,
      fecondity_matrix = mod_fecondity_matrix,
      survival_matrix = mod_surv_mat,
      class_names = class_names
    )
    
    # Calculate lambda and SSD from modified Leslie matrix
    mod_lambda_SSD <- find_lambda_SSD(mod_leslie_mat, class_names)
    mod_lambda <- mod_lambda_SSD[[1]]
    mod_SSD <- mod_lambda_SSD[[2]]
    
    # Calculate phosphorus content with modified SSD
    e_rates <- elem_rates(
      population_vector = mod_SSD,
      stoichiometry_array = stoichiometry_array,
      element_names = element_names
    )
    
    # Extract modified phosphorus percentage
    mod_percentP <- e_rates[['percent_elem_pop_biomass']][[paste0("percent", element_names[1])]]
    
    # Calculate elasticity
    lambda_elasticity <- (lambda0 - mod_lambda) / lambda0
    P_elasticity <- (percentP0 - mod_percentP) / percentP0
    
    # Store results
    tmp <- data.table(
      sample_id = param_set$iter,
      theta = theta,
      parameter_type = parameter_type,
      parameter_name = parameter_name, 
      class_affected = factor(class_names[j], levels = class_names),
      lambda0 = lambda0,
      mod_lambda = mod_lambda,
      percentP0 = percentP0,
      mod_percentP = mod_percentP,
      lambda_elasticity = lambda_elasticity,
      P_elasticity = P_elasticity
    )
    
    elasticity_results <- rbind(elasticity_results, tmp)
  }
  
  return(elasticity_results)
}

#' Calculate elasticity based on underlying model parameters
#'
#' @description 
#' This function calculates elasticity by modifying the underlying model parameters
#' that determine the growth and fecundity matrices, then measuring the effect on lambda and phosphorus.
#'
#' @param param_set A single parameter set
#' @param class_names Vector of size class names
#' @param transition_matrix Original growth transition matrix
#' @param fecondity_matrix Original fecundity matrix
#' @param stoichiometry_array Matrix with stoichiometric data
#' @param element_names Vector of element names
#' @param L_max Maximum size for growth model
#' @param delta_t Time step in days
#' @param theta Current temperature in Celsius
#' @param class_lim Vector of size class limits
#' @param sexratio Sex ratio (proportion of females)
#' @param gravid Proportion of gravid females
#' @param fertil Vector of fertility rates by size class
#' @param parameter_name Name of the parameter to modify
#'
#' @return A data.table with elasticity results
#' @export
calculate_model_parameter_elasticity <- function(param_set, class_names, 
                                                 transition_matrix, fecondity_matrix, 
                                                 stoichiometry_array, element_names,
                                                 L_max, delta_t, theta, class_lim,
                                                 sexratio, gravid, fertil, 
                                                 growth_rate_coef, growth_rate_intercept,          
                                                 molt_cycle_a, molt_cycle_b,                     
                                                 molt_cycle_c, molt_cycle_d,
                                                 parameter_name) {
  # Extract baseline values
  lambda0 <- param_set$lambda
  percentP0 <- param_set$mean_percentP
  
  surv_rates <- c(
    param_set$surv_rate_J1,
    param_set$surv_rate_J2,
    param_set$surv_rate_A1,
    param_set$surv_rate_A2,
    param_set$surv_rate_A3
  )
  
  # Initialize modified parameters with original values
  mod_transition_matrix <- transition_matrix
  mod_fecondity_matrix <- fecondity_matrix
  
  # Modify specific growth parameter based on parameter_name
  if (parameter_name == "growth_rate_coef") {
    growth_rate_coef = growth_rate_coef * 0.9
  } else if (parameter_name == "growth_rate_intercept") {
    growth_rate_intercept = growth_rate_intercept * 0.9
  } else if (parameter_name == "L_max") {
    L_max = L_max * 0.9
  }
  
  # Recalculate growth transition matrix with modified parameter
  mod_transition_matrix <- growth_rates_matrix(
    N_ind = 10000, 
    L_max = L_max, 
    delta_t = delta_t, 
    theta = theta, 
    class_lim = class_lim, 
    class_names = class_names,
    growth_rate_coef = growth_rate_coef,
    growth_rate_intercept = growth_rate_intercept
  )
  
  # Modify specific fecundity parameter based on parameter_name
  if (parameter_name == "molt_cycle_a") {
    molt_cycle_a = molt_cycle_a * 0.9
  } else if (parameter_name == "molt_cycle_b") {
    molt_cycle_b = molt_cycle_b * 0.9
  } else if (parameter_name == "molt_cycle_c") {
    molt_cycle_c = molt_cycle_c * 0.9
  } else if (parameter_name == "molt_cycle_d") {
    molt_cycle_d = molt_cycle_d * 0.9
  } else if (parameter_name == "sexratio") {
    sexratio = sexratio * 0.9
  } else if (parameter_name == "gravid") {
    gravid = gravid * 0.9
  } else if (grepl("^fertil_", parameter_name)) {
    # Modify a specific fertility rate
    # Extract class index from parameter name (e.g., "fertil_A1" -> extract "A1")
    class_id <- sub("fertil_", "", parameter_name)
    class_idx <- match(class_id, class_names)
    
    if (!is.na(class_idx)) {
      # Only modify if the class exists
      fertil[class_idx] <- fertil[class_idx] * 0.9  # 10% reduction
    }
  }
  
  # Recalculate fecundity matrix with modified parameter
  mod_fecondity_matrix <- fecondity_rates_matrix(
    sexratio = sexratio, 
    gravid = gravid, 
    fertil = fertil, 
    delta_t = delta_t, 
    theta = theta, 
    class_names = class_names,
    molt_cycle_a = molt_cycle_a,
    molt_cycle_b = molt_cycle_b,
    molt_cycle_c = molt_cycle_c,
    molt_cycle_d = molt_cycle_d
  )
 
  # Calculate survival matrix (unchanged)
  surv_mat <- survival_rates_matrix(
    survival_rates = surv_rates, 
    class_names = class_names
  )
  
  # Calculate modified Leslie matrix
  mod_leslie_mat <- Leslie_matrix(
    transition_matrix = mod_transition_matrix, 
    fecondity_matrix = mod_fecondity_matrix,
    survival_matrix = surv_mat, 
    class_names = class_names
  )
  
  # Calculate lambda and SSD from modified Leslie matrix
  mod_lambda_SSD <- find_lambda_SSD(mod_leslie_mat, class_names)
  mod_lambda <- mod_lambda_SSD[[1]]
  mod_SSD <- mod_lambda_SSD[[2]]
  
  # Calculate phosphorus content with modified SSD
  e_rates <- elem_rates(
    population_vector = mod_SSD,
    stoichiometry_array = stoichiometry_array,
    element_names = element_names
  )
  
  # Extract modified phosphorus percentage
  mod_percentP <- e_rates[['percent_elem_pop_biomass']][[paste0("percent", element_names[1])]]
  
  # Calculate elasticity
  lambda_elasticity <- (lambda0 - mod_lambda) / lambda0
  P_elasticity <- (percentP0 - mod_percentP) / percentP0
  
  # Store results
  elasticity_result <- data.table(
    sample_id = param_set$iter,
    theta = theta,
    parameter_name = parameter_name,
    lambda0 = lambda0,
    mod_lambda = mod_lambda,
    percentP0 = percentP0,
    mod_percentP = mod_percentP,
    lambda_elasticity = lambda_elasticity,
    P_elasticity = P_elasticity
  )
  
  return(elasticity_result)
}