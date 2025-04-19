###############################################################################
# _targets.R file
# Define the pipeline for the Gammarus phosphorum project
# Author: Titouan Dionet
# Date: April 2025
###############################################################################

# Load packages required for pipeline definition
library(targets)
library(tarchetypes)

future::plan(future::multisession, workers = 6)

# Set pipeline options
tar_option_set(
  packages = c("data.table", "ggplot2"), # Common packages for all targets
  format = "rds",                        # Default storage format
  error = "continue"                     # Prevents pipeline from stopping on error
)

# Load R functions from R/ directory
tar_source()

# Pipeline
tar_plan(
  
  # ______________________________________________________________________________
  # Setup and data preparation targets ----
  # ______________________________________________________________________________
  
  # Load fonts
  tar_target(
    load_fonts,
    setup_fonts(),
    description = "Load fonts",
    cue = tar_cue(mode = "always") # to always load fonts
  ),
  
  # Create outputs directories
  # tar_target(
  #   output_dir,
  #   {
  #     dir_path <- here::here("outputs")
  #     if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  #     dir_path
  #   },
  #   description = "Directory for general outputs"
  # ),
  
  tar_target(
    sim_output_dir,
    {
      dir_path <- here::here("outputs", "simulation_results")
      if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
      dir_path
    },
    description = "Directory for simulation outputs"
  ),
  
  tar_target(
    fig_output_dir,
    {
      dir_path <- here::here("outputs", "figures")
      if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
      dir_path
    },
    description = "Directory for figure outputs"
  ),
  
  # ______________________________________________________________________________
  # Phosphorus data analysis ----
  # ______________________________________________________________________________
  
  # Load calibration data
  tar_target(
    calibration_data,
    fread(here::here("data", "raw_data", "P_conc_range_2023_07.csv"), 
          sep = ";", dec = ",", header = TRUE),
    description = "Phosphorus calibration curve data from July 2023"
  ),
  
  # Calibration coefficient
  tar_target(
    calibration_coefficient,
    cal_coef(calibration_data),
    description = "Calibration coefficient derived from standard curve"
  ),
  
  # Load raw phosphorus data
  tar_target(
    raw_phosphorus_data,
    fread(here::here("data", "raw_data", "phosphorus_measurements_2023_07.csv"), 
          sep = ";", dec = ",", header = TRUE),
    description = "Raw phosphorus measurement data from July 2023"
  ),
  
  # Process phosphorus data
  tar_target(
    processed_phosphorus_data,
    process_phosphorus_data(raw_phosphorus_data, calibration_coefficient$coef),
    description = "Processed phosphorus data with concentrations and percentages calculated"
  ),
  
  # Statistical analysis
  tar_target(
    phosphorus_stats,
    auto_test_groups(processed_phosphorus_data, 
                     processed_phosphorus_data$P_percent, 
                     processed_phosphorus_data$class, 
                     detailed = TRUE),
    description = "Statistical analysis of idividual phosphorus differences between size classes"
  ),
  
  # ______________________________________________________________________________
  # Simulation configuration ----
  # ______________________________________________________________________________
  
  # Base parameters
  tar_target(
    base_params,
    list(
      L_max = 8.5,                              # Maximum size in mm
      delta_t = 30,                             # Time step in days
      lim_class = c(1.5, 3.5, 5.2, 6, 7, 11),   # Size class limits
      sexratio = 0.5,                           # Sex ratio (proportion of females)
      gravid = 0.5,                             # Proportion of gravid females
      fertil = c(0, 0, 3.6, 5.1, 9.2),          # Fertility rates by size class
      names_class = c("J1", "J2", "A1", "A2", "A3"), # Size class names
      element_names = c("P"),                   # Elemental names
      Theta_vec = c(8, 12, 16)                  # Temperatures in °C
    ),
    description = "Base parameters for population model and simulations"
  ),
  
  # Load monthly survival rates
  tar_target(
    monthly_surv_rates,
    fread(here::here("data", "raw_data", "monthly_surv_rates_coulaud_2014.csv"), 
          sep = ";", dec = ","),
    description = "Monthly survival rates for seasonal simulations from Coulaud et al., 2014"
  ),
  
  # Process stoichiometric data
  tar_target(
    mat_sto,
    {
      # First, create a modified class variable that combines neo-J1 and J1
      phosphorus_data_with_combined_j1 <- copy(processed_phosphorus_data)
      phosphorus_data_with_combined_j1[, combined_class := ifelse(grepl("neo-J1|^J1$", class), "J1", as.character(class))]
      
      # Calculate average biomass and phosphorus percentage by size class
      # For J1, this will now include both neo-J1 and J1 data
      size_class_data <- phosphorus_data_with_combined_j1[, .(
        mean_biomass = mean(dry_total_mass_microg/nb_ind, na.rm = TRUE),
        mean_percentP = mean(P_percent, na.rm = TRUE)
      ), by = combined_class]
      
      # Rename the column to match expected naming
      setnames(size_class_data, "combined_class", "class")
      
      # Convert to matrix format for use in elem_rates function
      mat_sto <- size_class_data[, .(
        biomass = mean_biomass,
        massP = mean_biomass * mean_percentP
      )]
      
      return(mat_sto)
    },
    description = "Stoichiometry matrix with biomass and phosphorus content by size class"
  ),
  
  # ______________________________________________________________________________
  # Matrix calculations (common for all simulations) ----
  # ______________________________________________________________________________
  
  # Temperature loop for matrices
  tar_target(
    temp_loop,
    base_params$Theta_vec,
    iteration = "vector",
    description = "Tempreature vector to use for simulations"
  ),
  
  # Transition matrices for each temperature
  tar_target(
    trans_mat_by_temp,
    growth_rates_matrix(
      L_max = base_params$L_max, 
      delta_t = base_params$delta_t, 
      theta = temp_loop, 
      class_lim = base_params$lim_class, 
      class_names = base_params$names_class
    ),
    pattern = map(temp_loop),
    description = "Growth transition matrices for each temperature"
  ),
  
  # Fecundity matrices for each temperature
  tar_target(
    feco_mat_by_temp,
    fecondity_rates_matrix(
      sexratio = base_params$sexratio, 
      gravid = base_params$gravid, 
      fertil = base_params$fertil, 
      delta_t = base_params$delta_t, 
      theta = temp_loop, 
      class_names = base_params$names_class
    ),
    pattern = map(temp_loop),
    description = "Fecundity matrices for each temperature"
  ),
  
  # Annual mean survival rates
  tar_target(
    annual_surv,
    as.numeric(monthly_surv_rates[, lapply(.SD, mean), .SDcols = c(2:6)]),
    description = "Mean annual survival rates based on Coulaud et al., 2014"
  ),
  
  # ______________________________________________________________________________
  # Simulation 1: Single-parameter approach ----
  # ______________________________________________________________________________
  
  # Generate parameter combinations
  tar_target(
    single_param_combinations,
    generate_single_param_combinations(
      temps = base_params$Theta_vec,
      classes = base_params$names_class,
      base_surv_rates = annual_surv,
      n_iter = 1000
    ),
    description = "Parameter combinations for single-parameter simulations"
  ),
  
  # Run all simulations in a single target
  tar_target(
    single_param_results,
    {
      # Process data in batches to efficiently manage memory
      n_rows <- nrow(single_param_combinations)
      batch_size <- 500
      n_batches <- ceiling(n_rows / batch_size)
      
      # Initialize list to store results
      all_results <- vector("list", n_batches)
      
      # Process each batch
      for (batch_idx in 1:n_batches) {
        # Define start and end indices for this batch
        start_idx <- (batch_idx - 1) * batch_size + 1
        end_idx <- min(batch_idx * batch_size, n_rows)
        
        # Process each row in the current batch
        batch_results <- lapply(start_idx:end_idx, function(i) {
          # Extract parameters
          theta <- single_param_combinations$temp[i]
          class_var <- single_param_combinations$focal_class[i]
          class_index <- single_param_combinations$focal_class_index[i]
          param_value <- single_param_combinations$param_value[i]
          
          # Set up survival rates
          surv_rates <- annual_surv
          surv_rates[class_index] <- param_value
          
          # Get the appropriate matrices for this temperature
          transition_matrix <- extract_matrix_for_temp(
            matrix_data = trans_mat_by_temp, 
            temps = base_params$Theta_vec, 
            current_temp = theta
          )
          
          fecondity_matrix <- extract_matrix_for_temp(
            matrix_data = feco_mat_by_temp, 
            temps = base_params$Theta_vec, 
            current_temp = theta
          )
          
          # Run the simulation
          result <- run_single_simulation(
            theta = theta,
            surv_rates = surv_rates,
            transition_matrix = transition_matrix,
            fecondity_matrix = fecondity_matrix,
            class_names = base_params$names_class,
            stoichiometry_array = mat_sto,
            element_names = base_params$element_names
          )
          
          # Add information about which class was varied
          result[, class_var := class_var]
          
          return(result)
        })
        
        # Store the results of this batch
        all_results[[batch_idx]] <- rbindlist(batch_results)
        
        # Display progress
        cat("Simulation 1: Completed batch", batch_idx, "of", n_batches, "\n")
      }
      
      # Combine all results
      final_results <- rbindlist(all_results)
      final_results[, class_var := factor(class_var, levels = base_params$names_class)]
      
      return(final_results)
    },
    description = "Results of single-parameter simulations with all combinations"
  ),
  
  # Save results
  tar_target(
    single_param_results_save_path,
    {
      file_path = file.path(sim_output_dir, "single_param_results.csv")
      fwrite(single_param_results, 
             file = file_path, 
             sep = ";", dec = ",")
      return(file_path)
    },
    description = "Save results of single-parameter simulations"
  ),
  
  # ______________________________________________________________________________
  # Simulation 2: Multi-parameter approach ----
  # ______________________________________________________________________________
  
  # Generate random parameter combinations
  tar_target(
    multi_param_combinations,
    generate_multi_param_combinations(
      temps = base_params$Theta_vec,
      n_iter = 10000,
      min_val = 0.001,
      max_val = 1
    ),
    description = "Randomly generated parameter sets for multi-parameter simulations"
  ),
  
  # Run all multi-parameter simulations in a single target
  tar_target(
    multi_param_results,
    {
      # Process data in batches to efficiently manage memory
      n_rows <- nrow(multi_param_combinations)
      batch_size <- 500
      n_batches <- ceiling(n_rows / batch_size)
      
      # Initialize list to store results
      all_results <- vector("list", n_batches)
      
      # Process each batch
      for (batch_idx in 1:n_batches) {
        # Define start and end indices for this batch
        start_idx <- (batch_idx - 1) * batch_size + 1
        end_idx <- min(batch_idx * batch_size, n_rows)
        
        # Process each row in the current batch
        batch_results <- lapply(start_idx:end_idx, function(i) {
          # Extract parameters
          theta <- multi_param_combinations$temp[i]
          
          surv_rates <- c(
            multi_param_combinations$surv_rate_J1[i],
            multi_param_combinations$surv_rate_J2[i],
            multi_param_combinations$surv_rate_A1[i],
            multi_param_combinations$surv_rate_A2[i],
            multi_param_combinations$surv_rate_A3[i]
          )
          
          # Get the appropriate matrices for this temperature
          transition_matrix <- extract_matrix_for_temp(
            matrix_data = trans_mat_by_temp, 
            temps = base_params$Theta_vec, 
            current_temp = theta
          )
          
          fecondity_matrix <- extract_matrix_for_temp(
            matrix_data = feco_mat_by_temp, 
            temps = base_params$Theta_vec, 
            current_temp = theta
          )
          
          # Run the simulation
          run_single_simulation(
            theta = theta,
            surv_rates = surv_rates,
            transition_matrix = transition_matrix,
            fecondity_matrix = fecondity_matrix,
            class_names = base_params$names_class,
            stoichiometry_array = mat_sto,
            element_names = base_params$element_names
          )
        })
        
        # Store the results of this batch
        all_results[[batch_idx]] <- rbindlist(batch_results)
        
        # Display progress
        cat("Simulation 2: Completed batch", batch_idx, "of", n_batches, "\n")
      }
      
      # Combine all results
      final_results <- rbindlist(all_results)
      
      return(final_results)
    },
    description = "Results of multi-parameter simulations with all parameter sets"
  ),
  
  # Save results
  tar_target(
    multi_param_results_save_path,
    {
      file_path = file.path(sim_output_dir, "multi_param_results.csv")
      fwrite(multi_param_results, 
             file = file_path, 
             sep = ";", dec = ",")
      return(file_path)
    },
    description = "Save results of multi-parameter simulations"
  ),
  
  # ______________________________________________________________________________
  # Simulation 3: Elasticity analysis ----
  # ______________________________________________________________________________
  
  # Sample parameters for elasticity analysis
  tar_target(
    elasticity_samples,
    sample_elasticity_parameters(multi_param_results, n_samples = 10000),
    description = "Sampled parameter sets for elasticity analysis"
  ),
  
  # Run all elasticity calculations in a single target
  tar_target(
    elasticity_results,
    {
      # Process data in batches to efficiently manage memory
      n_rows <- nrow(elasticity_samples)
      batch_size <- 500
      n_batches <- ceiling(n_rows / batch_size)
      
      # Initialize list to store results
      all_results <- vector("list", n_batches)
      
      # Process each batch
      for (batch_idx in 1:n_batches) {
        # Define start and end indices for this batch
        start_idx <- (batch_idx - 1) * batch_size + 1
        end_idx <- min(batch_idx * batch_size, n_rows)
        
        # Process each row in the current batch
        batch_results <- lapply(start_idx:end_idx, function(i) {
          # Extract sample
          param_set <- elasticity_samples[i, ]
          
          # Get temperature
          theta <- param_set$theta
          
          # Get appropriate matrices for this temperature
          transition_matrix <- extract_matrix_for_temp(
            matrix_data = trans_mat_by_temp, 
            temps = base_params$Theta_vec, 
            current_temp = theta
          )
          
          fecondity_matrix <- extract_matrix_for_temp(
            matrix_data = feco_mat_by_temp, 
            temps = base_params$Theta_vec, 
            current_temp = theta
          )
          
          # Calculate elasticity
          calculate_elasticity(
            param_set = param_set,
            class_names = base_params$names_class,
            transition_matrix = transition_matrix,
            fecondity_matrix = fecondity_matrix,
            stoichiometry_array = mat_sto,
            element_names = base_params$element_names
          )
        })
        
        # Combine results from this batch
        all_results[[batch_idx]] <- rbindlist(batch_results)
        
        # Display progress
        cat("Elasticity: Completed batch", batch_idx, "of", n_batches, "\n")
      }
      
      # Combine all results
      final_results <- rbindlist(all_results)
      
      return(final_results)
    },
    description = "Complete elasticity analysis results"
  ),
  
  # Calculate summary statistics for elasticity results
  tar_target(
    elasticity_summary,
    elasticity_results[, .(
      mean_lambda_elasticity = mean(lambda_elasticity),
      sd_lambda_elasticity = sd(lambda_elasticity),
      mean_P_elasticity = mean(P_elasticity),
      sd_P_elasticity = sd(P_elasticity)
    ), by = .(theta, class_affected)],
    description = "Summary statistics of elasticity analysis by temperature and size class"
  ),
  
  # Save elasticity results
  tar_target(
    elasticity_results_save_path,
    {
      file_path = file.path(sim_output_dir, "elasticity_results.csv")
      fwrite(elasticity_results, 
             file = file_path, 
             sep = ";", dec = ",")
      return(file_path)
    },
    description = "Save results of elasticity analysis"
  ),

  # ______________________________________________________________________________
  # Simulation 4: Monthly variation ----
  # ______________________________________________________________________________
  
  # Run monthly simulations for each temperature
  tar_target(
    monthly_simulations,
    {
      # Current temperature
      theta <- temp_loop
      
      # Initialize results
      monthly_results <- data.table()
      
      # Loop through months
      for (m in 1:nrow(monthly_surv_rates)) {
        # Extract monthly survival rates
        month_name <- monthly_surv_rates[m, 1]
        surv_rates <- as.numeric(monthly_surv_rates[m, 2:6])
        
        # Get appropriate transition and fecundity matrices
        transition_matrix <- extract_matrix_for_temp(
          matrix_data = trans_mat_by_temp, 
          temps = base_params$Theta_vec, 
          current_temp = theta
        )
        
        fecondity_matrix <- extract_matrix_for_temp(
          matrix_data = feco_mat_by_temp, 
          temps = base_params$Theta_vec, 
          current_temp = theta
        )
        
        # Run the simulation
        result <- run_single_simulation(
          theta = theta,
          surv_rates = surv_rates,
          transition_matrix = transition_matrix,
          fecondity_matrix = fecondity_matrix,
          class_names = base_params$names_class,
          stoichiometry_array = mat_sto,
          element_names = base_params$element_names
        )
        
        # Add month information
        result[, month := month_name]
        
        # Add to results
        monthly_results <- rbind(monthly_results, result)
      }
      
      return(monthly_results)
    },
    pattern = map(temp_loop),
    description = "Results of simulations using monthly survival rates to capture seasonal patterns"
  ),
  
  # # Combine monthly results
  # tar_target(
  #   monthly_results,
  #   rbindlist(monthly_simulations),
  #   description = "Combine results of monthly simulations"
  # ),
  
  # Save monthly results
  tar_target(
    monthly_results_save_path,
    {
      file_path = file.path(sim_output_dir, "monthly_results.csv")
      fwrite(monthly_simulations, 
             file = file_path, 
             sep = ";", dec = ",")
      return(file_path)
    },
    description = "Save results of monthly simulations"
  ),
  
  # Run annual mean simulation for each temperature
  tar_target(
    annual_simulations,
    {
      # Current temperature
      theta <- temp_loop
      
      # Mean annual survival rates
      surv_rates <- annual_surv
      
      # Get appropriate transition and fecundity matrices
      transition_matrix <- extract_matrix_for_temp(
        matrix_data = trans_mat_by_temp, 
        temps = base_params$Theta_vec, 
        current_temp = theta
      )
      
      fecondity_matrix <- extract_matrix_for_temp(
        matrix_data = feco_mat_by_temp, 
        temps = base_params$Theta_vec, 
        current_temp = theta
      )
      
      # Run the simulation
      result <- run_single_simulation(
        theta = theta,
        surv_rates = surv_rates,
        transition_matrix = transition_matrix,
        fecondity_matrix = fecondity_matrix,
        class_names = base_params$names_class,
        stoichiometry_array = mat_sto,
        element_names = base_params$element_names
      )
      
      return(result)
    },
    pattern = map(temp_loop),
    description = "Results of simulations using mean annual survival rates"
  ),
  
  # # Combine annual results
  # tar_target(
  #   annual_results,
  #   rbindlist(annual_simulations),
  #   description = "Combine results of annual simulation"
  # ),
  
  # Save annual results
  tar_target(
    annual_results_save_path,
    {
      file_path = file.path(sim_output_dir, "annual_results.csv")
      fwrite(annual_simulations, 
             file = file_path, 
             sep = ";", dec = ",")
      return(file_path)
    },
    description = "Save results of annual simulation"
  ),
  
  # ______________________________________________________________________________
  # Figure 1: Individual phosphorus rate ----
  # ______________________________________________________________________________
  
  # Create Figure 1
  tar_target(
    figure_1,
    {
      load_fonts
      create_phosphorus_figure(processed_phosphorus_data, phosphorus_stats$info)
    },
    description = "Figure 1: Phosphorus content across different size classes"
  ),
  
  # ______________________________________________________________________________
  # Figure 2: Sensitivity analysis (Elasticity) ----
  # ______________________________________________________________________________
  
  # Create Figure 2
  tar_target(
    figure_2,
    {
      load_fonts
      create_elasticity_figure(elasticity_results)
    },
    description = "Figure 2: Sensitivity of population growth rate and phosphorus content to survival rates"
  ),
  
  # ______________________________________________________________________________
  # Figure 3: J1 and A3 survival effect ----
  # ______________________________________________________________________________
  
  # Prepare data for Figure 3
  tar_target(
    figure_3_data,
    single_param_results[class_var %in% c("J1", "A3")],
    description = "Filtered data for Figure 3 showing the effect of J1 and A3 survival"
  ),
  
  # Calculate reference points for Figure 3
  tar_target(
    figure_3_reference_points,
    annual_simulations[, .(lambda = lambda, 
                       mean_percentP = mean_percentP, 
                       theta = theta)],
    description = "Annual reference points for population growth and phosphorus content"
  ),
  
  # Create Figure 3
  tar_target(
    figure_3,
    {
      load_fonts
      create_j1_a3_survival_effect(figure_3_data, figure_3_reference_points)
    },
    description = "Figure 3: Effect of J1 and A3 survival on growth rate and phosphorus relationship"
  ),
  
  # ______________________________________________________________________________
  # Figure 4: Survival gradient effect ----
  # ______________________________________________________________________________
  
  # Create Figure 4
  tar_target(
    figure_4,
    {
      load_fonts
      create_survival_gradient_effect(multi_param_results, monthly_simulations)
    },
    description = "Figure 4: Full parameter space exploration of growth rate and phosphorus relationship"
  ),
  
  # ______________________________________________________________________________
  # Save figures ----
  # ______________________________________________________________________________
  
  # Save Figure 1
  tar_target(
    save_figure_1,
    save_figure(
      plot = figure_1,
      basename = "figure_1_phosphorus_by_size_class",
      dir = fig_output_dir,
      width = 720,
      height = 720,
      units = "px",
      dpi = 200
    ),
    description = "Saved Figure 1 in multiple formats"
  ),
  
  # Save Figure 2
  tar_target(
    save_figure_2,
    save_figure(
      plot = figure_2,
      basename = "figure_2_elasticity_analysis",
      dir = fig_output_dir,
      width = 1744,
      height = 1280,
      units = "px",
      dpi = 200
    ),
    description = "Saved Figure 2 in multiple formats"
  ),
  
  # Save Figure 3
  tar_target(
    save_figure_3,
    save_figure(
      plot = figure_3,
      basename = "figure_3_j1_a3_survival_effect",
      dir = fig_output_dir,
      width = 2048,
      height = 1384,
      units = "px",
      dpi = 300
    ),
    description = "Saved Figure 3 in multiple formats"
  ),
  
  # Save Figure 4
  tar_target(
    save_figure_4,
    save_figure(
      plot = figure_4,
      basename = "figure_4_survival_gradient_effect",
      dir = fig_output_dir,
      width = 2048,
      height = 1744,
      units = "px",
      dpi = 200
    ),
    description = "Saved Figure 4 in multiple formats"
  )
)