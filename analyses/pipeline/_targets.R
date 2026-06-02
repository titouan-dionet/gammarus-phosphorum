###############################################################################
# _targets.R file
# Define the pipeline for the Gammarus phosphorum project
# Author: Titouan Dionet
# Date: April 2025
###############################################################################

# Load packages required for pipeline definition
library(targets)
library(tarchetypes)

# future::plan(future::multisession, workers = 6)

# Set pipeline options
tar_option_set(
  packages = c("data.table", "ggplot2", "patchwork"), # Common packages for all targets
  format = "rds", # Default storage format
  error = "continue" # Prevents pipeline from stopping on error
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
      if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE)
      }
      dir_path
    },
    description = "Directory for simulation outputs"
  ),

  tar_target(
    fig_output_dir,
    {
      dir_path <- here::here("outputs", "figures")
      if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE)
      }
      dir_path
    },
    description = "Directory for figure outputs"
  ),

  tar_target(
    tab_output_dir,
    {
      dir_path <- here::here("outputs", "tables")
      if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE)
      }
      dir_path
    },
    description = "Directory for tables outputs"
  ),

  # ______________________________________________________________________________
  # Phosphorus data analysis ----
  # ______________________________________________________________________________

  # Load calibration data
  tar_target(
    calibration_data,
    fread(
      here::here("data", "raw_data", "P_conc_range_2023_07.csv"),
      sep = ",",
      dec = ".",
      header = TRUE
    ),
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
    fread(
      here::here("data", "raw_data", "phosphorus_measurements_2023_07.csv"),
      sep = ",",
      dec = ".",
      header = TRUE
    ),
    description = "Raw phosphorus measurement data from July 2023"
  ),

  # Process phosphorus data
  tar_target(
    phosphorus_class_data,
    convert_class_to_factor(raw_phosphorus_data),
    description = "Phosphorus data with size classes converted to ordered factor"
  ),

  tar_target(
    processed_phosphorus_data,
    calculate_phosphorus(phosphorus_class_data, calibration_coefficient$coef),
    description = "Processed phosphorus data with concentrations and percentages calculated"
  ),

  # Note: In one replicate in the J1 class, the %P value is far lower than
  # other values from the same class, probably due to handling or measurement
  # error. Thus, the authors decided to remove this point from further analysis.

  # Remove outlier point from further analysis
  tar_target(
    clean_processed_phosphorus_data,
    processed_phosphorus_data[id != "G-J1-3"],
    description = "Remove outlier point from further analysis"
  ),

  # Statistical analysis
  tar_target(
    phosphorus_stats,
    auto_test_groups(
      clean_processed_phosphorus_data,
      clean_processed_phosphorus_data$P_percent,
      clean_processed_phosphorus_data$class,
      detailed = TRUE
    ),
    description = "Statistical analysis of individual phosphorus differences between size classes"
  ),

  # Statistical analysis between J1 and others
  tar_target(
    phosphorus_stats_J1_vs_Others,
    {
      # Create a modified class variable that combines neo-J1 and J1, and separate it from others
      phosphorus_data_with_combined_j1 <- copy(clean_processed_phosphorus_data)
      phosphorus_data_with_combined_j1[,
        combined_class := ifelse(grepl("neo-J1|^J1$", class), "J1", "Others")
      ]

      # Statistical analysis
      auto_test_groups(
        phosphorus_data_with_combined_j1,
        phosphorus_data_with_combined_j1$P_percent,
        phosphorus_data_with_combined_j1$combined_class,
        detailed = TRUE
      )
    },
    description = "Statistical analysis of idividual phosphorus differences between J1 and others"
  ),

  # ______________________________________________________________________________
  # Simulation configuration ----
  # ______________________________________________________________________________

  # Base parameters
  tar_target(
    base_params,
    list(
      L_max = 8.5, # Maximum size in mm
      delta_t = 30, # Time step in days
      lim_class = c(1.5, 3.5, 5.2, 6, 7, 11), # Size class limits
      sexratio = 0.5, # Sex ratio (proportion of females)
      gravid = 0.5, # Proportion of gravid females
      fertil = c(0, 0, 3.6, 5.1, 9.2), # Fertility rates by size class
      names_class = c("J1", "J2", "A1", "A2", "A3"), # Size class names
      element_names = c("P"), # Elemental names
      Theta_vec = c(8, 12, 16), # Temperatures in °C
      growth_rate_coef = 0.0014, # Growth rate coefficient in growth model
      growth_rate_intercept = -0.0024, # Growth rate intercept in growth model
      molt_cycle_a = 30.61, # Parameter beta_1 in molt cycle equation
      molt_cycle_b = -0.39, # Parameter alpha_1 in molt cycle equation
      molt_cycle_c = 0.01, # Parameter beta_2 in molt cycle equation
      molt_cycle_d = 0.05 # Parameter alpha_2 in molt cycle equation
    ),
    description = "Base parameters for population model and simulations"
  ),

  # Load monthly survival rates
  tar_target(
    monthly_surv_rates,
    fread(
      here::here("data", "raw_data", "monthly_surv_rates_coulaud_2014.csv"),
      sep = ",",
      dec = "."
    ),
    description = "Monthly survival rates for seasonal simulations from Coulaud et al., 2014"
  ),

  # Process stoichiometric data
  tar_target(
    mat_sto,
    {
      # First, create a modified class variable that combines neo-J1 and J1
      phosphorus_data_with_combined_j1 <- copy(clean_processed_phosphorus_data)
      phosphorus_data_with_combined_j1[,
        combined_class := ifelse(
          grepl("neo-J1|^J1$", class),
          "J1",
          as.character(class)
        )
      ]

      # Calculate average biomass and phosphorus percentage by size class
      # For J1, this will now include both neo-J1 and J1 data
      size_class_data <- phosphorus_data_with_combined_j1[,
        .(
          mean_biomass = mean(dry_total_mass_microg / nb_ind, na.rm = TRUE),
          mean_percentP = mean(P_percent, na.rm = TRUE)
        ),
        by = combined_class
      ]

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
      class_names = base_params$names_class,
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
      final_results[,
        class_var := factor(class_var, levels = base_params$names_class)
      ]

      return(final_results)
    },
    description = "Results of single-parameter simulations with all combinations"
  ),

  # Save results
  tar_target(
    single_param_results_save_path,
    {
      file_path = file.path(sim_output_dir, "single_param_results.csv")
      fwrite(single_param_results, file = file_path, sep = ",", dec = ".")
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
      fwrite(multi_param_results, file = file_path, sep = ",", dec = ".")
      return(file_path)
    },
    description = "Save results of multi-parameter simulations"
  ),

  # ______________________________________________________________________________
  # Simulation 3: Monthly variation ----
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
      fwrite(monthly_simulations, file = file_path, sep = ",", dec = ".")
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
      fwrite(annual_simulations, file = file_path, sep = ",", dec = ".")
      return(file_path)
    },
    description = "Save results of annual simulation"
  ),

  # ______________________________________________________________________________
  # Elasticity analysis: Survival, fecundity and growth ----
  # ______________________________________________________________________________

  # Sample parameters for elasticity analysis
  tar_target(
    elasticity_samples,
    sample_elasticity_parameters(
      multi_param_results,
      n_samples = nrow(multi_param_results)
    ), # can be change to less samples if needed
    description = "Sampled parameter sets for elasticity analysis"
  ),

  # Sample parameters for elasticity analysis (using samples)
  tar_target(
    elasticity_results,
    {
      # Initialize results list
      all_results <- list()

      # List of parameter types to analyze
      param_types <- c("survival", "fecundity", "growth")

      for (param_type in param_types) {
        cat("\nCalculating elasticity for parameter type:", param_type, "\n")

        # Process data in batches to efficiently manage memory
        n_rows <- nrow(elasticity_samples)
        batch_size <- 500
        n_batches <- ceiling(n_rows / batch_size)

        # Initialize list to store results
        type_results <- vector("list", n_batches)

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

            # Calculate elasticity for the current parameter type
            calculate_elasticity(
              param_set = param_set,
              class_names = base_params$names_class,
              transition_matrix = transition_matrix,
              fecondity_matrix = fecondity_matrix,
              stoichiometry_array = mat_sto,
              element_names = base_params$element_names,
              parameter_type = param_type
            )
          })

          # Combine results from this batch
          type_results[[batch_idx]] <- rbindlist(batch_results)

          # Display progress
          cat(
            "Elasticity for",
            param_type,
            ": Completed batch",
            batch_idx,
            "of",
            n_batches,
            "\n"
          )
        }

        # Combine all batches for this parameter type
        all_results[[param_type]] <- rbindlist(type_results)
      }

      # Combine all parameter types
      final_results <- rbindlist(all_results)
      final_results[,
        parameter_type := factor(parameter_type, levels = param_types)
      ]

      return(final_results)
    },
    description = "Complete elasticity analysis for survival, fecundity, and growth parameters"
  ),

  # Calculate summary statistics for comprehensive elasticity results
  tar_target(
    elasticity_summary,
    elasticity_results[,
      .(
        mean_lambda_elasticity = mean(lambda_elasticity),
        sd_lambda_elasticity = sd(lambda_elasticity),
        mean_P_elasticity = mean(P_elasticity),
        sd_P_elasticity = sd(P_elasticity)
      ),
      by = .(theta, parameter_type, parameter_name, class_affected)
    ],
    description = "Summary statistics of comprehensive elasticity analysis"
  ),

  # Save comprehensive elasticity results
  tar_target(
    elasticity_results_save_path,
    {
      file_path = file.path(
        sim_output_dir,
        "elasticity_results.csv"
      )
      fwrite(
        elasticity_results,
        file = file_path,
        sep = ",",
        dec = "."
      )
      return(file_path)
    },
    description = "Save results of comprehensive elasticity analysis"
  ),

  # ______________________________________________________________________________
  # Elasticity analysis: Model Parameter Elasticity Analysis ----
  # ______________________________________________________________________________

  # Define model parameters to analyze
  tar_target(
    model_parameters_to_analyze,
    c(
      # Growth parameters
      "growth_rate_coef", # Coefficient of temperature in growth rate equation (0.0014)
      "growth_rate_intercept", # Intercept in growth rate equation (-0.0024)
      "L_max", # Maximum size

      # Reproduction parameters
      "molt_cycle_a", # Parameter a in molt cycle equation (30.61)
      "molt_cycle_b", # Parameter b in molt cycle equation (-0.39)
      "molt_cycle_c", # Parameter c in molt cycle equation (0.01)
      "molt_cycle_d", # Parameter d in molt cycle equation (0.05)
      "sexratio", # Sex ratio
      "gravid", # Proportion of gravid females

      # Fertility rates by class (only for classes with fertility > 0)
      "fertil_A1", # Fertility of A1 class
      "fertil_A2", # Fertility of A2 class
      "fertil_A3" # Fertility of A3 class
    ),
    description = "List of model parameters to analyze for elasticity"
  ),

  # Run model parameter elasticity analysis
  tar_target(
    model_parameter_elasticity_results,
    {
      # Initialize results list
      all_results <- list()

      # Process a subset of samples for efficiency
      sample_size <- 1000 # Adjust as needed
      set.seed(42)
      selected_samples <- elasticity_samples[sample(
        1:nrow(elasticity_samples),
        sample_size
      )]

      # For each parameter
      for (param in model_parameters_to_analyze) {
        cat("\nCalculating elasticity for model parameter:", param, "\n")

        # Process samples
        param_results <- lapply(1:nrow(selected_samples), function(i) {
          # Extract sample
          param_set <- selected_samples[i, ]

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

          # Calculate elasticity for this parameter
          calculate_model_parameter_elasticity(
            param_set = param_set,
            class_names = base_params$names_class,
            transition_matrix = transition_matrix,
            fecondity_matrix = fecondity_matrix,
            stoichiometry_array = mat_sto,
            element_names = base_params$element_names,
            L_max = base_params$L_max,
            delta_t = base_params$delta_t,
            theta = theta,
            class_lim = base_params$lim_class,
            sexratio = base_params$sexratio,
            gravid = base_params$gravid,
            fertil = base_params$fertil,
            growth_rate_coef = base_params$growth_rate_coef,
            growth_rate_intercept = base_params$growth_rate_intercept,
            molt_cycle_a = base_params$molt_cycle_a,
            molt_cycle_b = base_params$molt_cycle_b,
            molt_cycle_c = base_params$molt_cycle_c,
            molt_cycle_d = base_params$molt_cycle_d,
            parameter_name = param
          )
        })

        # Combine results for this parameter
        all_results[[param]] <- rbindlist(param_results)

        # Display progress
        cat("Completed elasticity analysis for parameter:", param, "\n")
      }

      # Combine all parameter results
      final_results <- rbindlist(all_results)

      return(final_results)
    },
    description = "Elasticity analysis for model parameters rather than matrix elements"
  ),

  # Calculate summary statistics for model parameter elasticity
  tar_target(
    model_parameter_elasticity_summary,
    model_parameter_elasticity_results[,
      .(
        mean_lambda_elasticity = mean(lambda_elasticity),
        sd_lambda_elasticity = sd(lambda_elasticity),
        mean_P_elasticity = mean(P_elasticity),
        sd_P_elasticity = sd(P_elasticity),
        n_samples = .N
      ),
      by = .(theta, parameter_name)
    ],
    description = "Summary statistics of model parameter elasticity analysis"
  ),

  # Save model parameter elasticity results
  tar_target(
    model_parameter_elasticity_save_path,
    {
      file_path = file.path(
        sim_output_dir,
        "model_parameter_elasticity_results.csv"
      )
      fwrite(
        model_parameter_elasticity_results,
        file = file_path,
        sep = ",",
        dec = "."
      )
      return(file_path)
    },
    description = "Save results of model parameter elasticity analysis"
  ),

  # ______________________________________________________________________________
  # Figure 1: GRH conceptual figure ----
  # ______________________________________________________________________________

  tar_target(
    figure_1,
    {
      load_fonts
      create_grh_conceptual_figure()
    },
    description = "Figure 1: Conceptual figure illustrating the GRH at individual and population levels"
  ),

  # ______________________________________________________________________________
  # Figure 2: Individual phosphorus rate ----
  # ______________________________________________________________________________

  # Create Figure 2
  tar_target(
    figure_2,
    {
      load_fonts
      create_phosphorus_figure(
        clean_processed_phosphorus_data,
        phosphorus_stats$info
      )
    },
    description = "Figure 2: Phosphorus content across different size classes"
  ),

  # ______________________________________________________________________________
  # Figure 3: Sensitivity analysis (Elasticity) ----
  # ______________________________________________________________________________

  # Create Figure 3
  tar_target(
    figure_3,
    {
      load_fonts
      create_elasticity_figure(
        elasticity_results,
        analysis_type = "survival"
      )
    },
    description = "Figure 3: Sensitivity of population growth rate and phosphorus content to survival rates"
  ),

  # ______________________________________________________________________________
  # Figure 4: J1 and A3 survival effect ----
  # ______________________________________________________________________________

  # Prepare data for Figure 4
  tar_target(
    figure_4_data,
    single_param_results[class_var %in% c("J1", "A3")],
    description = "Filtered data for Figure 4 showing the effect of J1 and A3 survival"
  ),

  # Create Figure 4
  tar_target(
    figure_4,
    {
      load_fonts
      create_j1_a3_survival_effect(figure_4_data)
    },
    description = "Figure 4: Effect of J1 and A3 survival on growth rate and phosphorus relationship"
  ),

  # ______________________________________________________________________________
  # Figure 5: Survival gradient effect ----
  # ______________________________________________________________________________

  # Create Figure 5
  tar_target(
    figure_5,
    {
      load_fonts
      create_survival_gradient_density_figure(
        multi_param_results,
        monthly_simulations
      )
    },
    description = "Figure 5: Density distribution of population growth rate and phosphorus content across survival rate categories and temperatures"
  ),

  # ______________________________________________________________________________
  # Supplementary figures ----
  # ______________________________________________________________________________

  # ______________________________________________________________________________
  # Figure S1: Phosphorus sex difference analysis ----
  # ______________________________________________________________________________

  # Load calibration data
  tar_target(
    calibration_sup_data,
    fread(
      here::here("data", "raw_data", "P_conc_range_2023_04.csv"),
      sep = ",",
      dec = ".",
      header = TRUE
    ),
    description = "Phosphorus calibration curve data from April 2023"
  ),

  # Calibration coefficient
  tar_target(
    calibration_coefficient_sup,
    cal_coef(calibration_sup_data),
    description = "Calibration coefficient derived from standard curve of April 2023"
  ),

  # Load raw phosphorus data
  tar_target(
    raw_phosphorus_sup_data,
    fread(
      here::here(
        "data",
        "raw_data",
        "phosphorus_measurements_individual_2023_04.csv"
      ),
      sep = ",",
      dec = ".",
      header = TRUE
    ),
    description = "Raw phosphorus measurement data from April 2023"
  ),

  # Process phosphorus data
  tar_target(
    processed_phosphorus_sup_data,
    calculate_phosphorus(
      raw_phosphorus_sup_data,
      calibration_coefficient_sup$coef
    ),
    description = "Processed phosphorus data with concentrations and percentages calculated"
  ),

  # Statistical analysis
  tar_target(
    phosphorus_stats_sup,
    auto_test_groups(
      processed_phosphorus_sup_data[sex != "J"],
      processed_phosphorus_sup_data[sex != "J"]$P_percent,
      processed_phosphorus_sup_data[sex != "J"]$sex,
      detailed = TRUE
    ),
    description = "Statistical analysis of individual phosphorus differences between adults"
  ),

  # Create Figure S1
  tar_target(
    figure_S1,
    {
      load_fonts
      create_phosphorus_sex_difference_figure(
        processed_phosphorus_sup_data[sex != "J"],
        phosphorus_stats_sup$info
      )
    },
    description = "Figure S1: Phosphorus content across sex"
  ),

  # ______________________________________________________________________________
  # Figure S2: Temperature-dependent transition rates visualization ----
  # ______________________________________________________________________________

  # Generate temperature range for transition rates
  tar_target(
    temp_range_transition,
    seq(2, 25, by = 0.1),
    description = "Temperature range for transition rates visualization"
  ),

  # Calculate transition rates for all temperatures
  tar_target(
    transition_rates_data,
    calculate_transition_rates(
      temp_range = temp_range_transition,
      L_max = base_params$L_max,
      delta_t = base_params$delta_t,
      class_lim = base_params$lim_class,
      class_names = base_params$names_class
    ),
    description = "Transition rates data for visualization"
  ),

  # Create transition rates plot
  tar_target(
    figure_S2,
    {
      load_fonts
      create_transition_rates_plot(
        transition_data = transition_rates_data,
        significance_threshold = 0.01,
        reference_temps = base_params$Theta_vec,
        L_max = base_params$L_max,
        delta_t = base_params$delta_t
      )
    },
    description = "Figure S2: Plot showing transition rates between size classes as a function of temperature"
  ),

  # ______________________________________________________________________________
  # Figures S3 and S4: Elasticities on fecundity and growth parameters ----
  # ______________________________________________________________________________
  # Create Figure S3
  tar_target(
    figure_S3,
    {
      load_fonts
      create_elasticity_figure(
        elasticity_results,
        analysis_type = "fecundity"
      )
    },
    description = "Figure S3: Sensitivity of population growth rate and phosphorus content to fecundity rates"
  ),

  # Create Figure S4
  tar_target(
    figure_S4,
    {
      load_fonts
      create_elasticity_figure(
        elasticity_results,
        analysis_type = "growth"
      )
    },
    description = "Figure S4: Sensitivity of population growth rate and phosphorus content to transition rates"
  ),

  # ______________________________________________________________________________
  # Figures S5: Model Parameter Elasticity Analysis ----
  # ______________________________________________________________________________

  # Create Figures S5
  tar_target(
    figure_S5,
    {
      load_fonts
      create_model_parameter_elasticity_figure(
        model_parameter_elasticity_results
      )
    },
    description = "Figure S5: Sensitivity analysis of underlying model parameters"
  ),

  # ______________________________________________________________________________
  # Table S1: Percentile distributions for Figure 4 ----
  # ______________________________________________________________________________

  # Compute Table S1
  tar_target(
    table_S1_data,
    compute_table_S1(multi_param_results),
    description = "Table S1: percentile distributions of lambda and population P content by survival rate category and temperature"
  ),

  # Save Table S1
  tar_target(
    save_table_S1,
    {
      file_path <- file.path(tab_output_dir, "table_S1_ecdf_percentiles.csv")
      data.table::fwrite(
        x = table_S1_data,
        bom = TRUE,
        encoding = "UTF-8",
        file = file_path,
        sep = ",",
        dec = ".",
        row.names = FALSE
      )
      file_path
    },
    description = "Saved Table S1 as CSV"
  ),

  # ______________________________________________________________________________
  # Save figures ----
  # ______________________________________________________________________________

  # Save Figure 1
  tar_target(
    save_figure_1,
    save_figure(
      plot = figure_1,
      basename = "figure_1_conceptual_figure",
      dir = fig_output_dir,
      width = 2800,
      height = 1400,
      units = "px",
      dpi = 300
    ),
    description = "Saved Figure 1 in multiple formats"
  ),

  # Save Figure 2
  tar_target(
    save_figure_2,
    save_figure(
      plot = figure_2,
      basename = "figure_2_phosphorus_by_size_class",
      dir = fig_output_dir,
      width = 720,
      height = 720,
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
      basename = "figure_3_elasticity_analysis_survival",
      dir = fig_output_dir,
      width = 2400,
      height = 1800,
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
      basename = "figure_4_j1_a3_survival_effect",
      dir = fig_output_dir,
      width = 1024,
      height = 720,
      units = "px",
      dpi = 200
    ),
    description = "Saved Figure 4 in multiple formats"
  ),

  # Save Figure 5
  tar_target(
    save_figure_5,
    save_figure(
      plot = figure_5,
      basename = "figure_5_survival_gradient_effect",
      dir = fig_output_dir,
      width = 2000,
      height = 1600,
      units = "px",
      dpi = 200
    ),
    description = "Saved Figure 5 in multiple formats"
  ),

  # Save Figure S1
  tar_target(
    save_figure_S1,
    save_figure(
      plot = figure_S1,
      basename = "figure_S1_phosphorus_by_sex",
      dir = fig_output_dir,
      width = 720,
      height = 720,
      units = "px",
      dpi = 200
    ),
    description = "Saved Figure S1 in multiple formats"
  ),

  # Save Figure S2
  tar_target(
    save_figure_S2,
    save_figure(
      plot = figure_S2,
      basename = "figure_S2_temperature_transition_rates",
      dir = fig_output_dir,
      width = 2048,
      height = 2048,
      units = "px",
      dpi = 300
    ),
    description = "Saved Figure S2 in multiple formats"
  ),

  # Save Figure S3
  tar_target(
    save_figure_S3,
    save_figure(
      plot = figure_S3,
      basename = "figure_S3_elasticity_analysis_fecundity",
      dir = fig_output_dir,
      width = 2400,
      height = 1800,
      units = "px",
      dpi = 300
    ),
    description = "Saved Figure S3 in multiple formats"
  ),

  # Save Figure S4
  tar_target(
    save_figure_S4,
    save_figure(
      plot = figure_S4,
      basename = "figure_S4_elasticity_analysis_growth",
      dir = fig_output_dir,
      width = 2400,
      height = 1800,
      units = "px",
      dpi = 300
    ),
    description = "Saved Figure S4 in multiple formats"
  ),

  # Save Figures S5
  tar_target(
    save_figure_S5,
    {
      names_figures = names(figure_S5)
      for (sub_fig in 1:length(names_figures)) {
        save_figure(
          plot = figure_S5[[sub_fig]],
          basename = paste0(
            "figure_S5_model_parameter_elasticity_",
            names(figure_S5[sub_fig])
          ),
          dir = fig_output_dir,
          width = 2400,
          height = 1800,
          units = "px",
          dpi = 300
        )
      }
    },
    description = "Saved Figures S5 in multiple formats"
  )
)
