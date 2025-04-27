###############################################################################
# figures.R
# Figure creation functions for the Gammarus-Phosphorum project
# Author: Titouan Dionet
# Date: April 2025
###############################################################################

#' Create phosphorus content figure
#'
#' @description 
#' This function creates Figure 1 for the manuscript showing 
#' phosphorus content by size class.
#'
#' @param phosphorus_data Processed phosphorus data
#' @param stats_info Statistical results with significance letters
#'
#' @return A ggplot object for Figure 1
#' 
#' @export
create_phosphorus_figure <- function(phosphorus_data, stats_info) {
  # Create figure
  p_plot <- ggplot(phosphorus_data) +
    # Formatting
    scale_y_continuous(breaks = seq(0, 100, by = 0.05), 
                       minor_breaks = seq(0, 100, by = 0.025), 
                       expand = c(0, 0)) +
    coord_cartesian(ylim = c(0.8, 1.35), clip = "on") +
    
    scale_color_manual(values = c(J1 = "#9ACD32", J2 = "#FFD700", A1 = "#8DEEEE", 
                                  A2 = "#AB82FF", A3 = "#EE6AA7", `neo-J1` = "#759837"),
                       name = "class", guide = "none") +
    
    # Data mapping
    aes(x = class, y = P_percent*100, col = class) +
    
    # Points with outlier highlighted
    geom_point(aes(shape = ifelse(class == "J1" & P_percent == min(phosphorus_data[class == "J1", P_percent]), 
                                  "outlier", "normal")), 
               size = 1.5) +
    scale_shape_manual(values = c(outlier = 1, normal = 16), guide = "none") +
    
    # Mean marker (star symbol)
    stat_summary(fun = "mean", col = "red", size = 2, shape = 13, geom = "point") +
    
    # Significance letters
    geom_text(data = stats_info, 
              aes(y = max*100+0.05, label = signif_letter, x = as.factor(trt)), 
              fontface = "bold", col = "black", family = "Roboto") +
    
    # Labels
    labs(x = "Size class", 
         y = "Individual P rate (%)") +
    
    # Theme
    theme_custom()
  
  return(p_plot)
}

#' Create comprehensive elasticity figure
#'
#' @description 
#' This function creates Figure 2, S2 and S3 for the manuscript showing 
#' the sensitivity of population growth rate and phosphorus content
#' to survival, fecundity, and growth rates.
#'
#' @param elasticity_results Data table with comprehensive elasticity analysis results
#'
#' @return A ggplot object for Figure 2, S2 or S3
#' @export
#' @importFrom scales trans_new label_percent
#' @importFrom scales trans_new label_percent
create_comprehensive_elasticity_figure <- function(elasticity_results, analysis_type) {
  # Select data
  elasticity_results = elasticity_results[parameter_type == analysis_type]
  
  # Define a common color palette for all parameter types
  class_colors <- c(J1 = "#9ACD32", J2 = "#FFD700", A1 = "#8DEEEE", A2 = "#AB82FF", A3 = "#EE6AA7")

  # Axis transformation
  squeeze_trans <- scales::trans_new(
    name = "squeeze",
    transform = function(x) sign(x) * log1p(abs(x)),
    inverse = function(x) sign(x) * (exp(abs(x)) - 1),
    domain = c(-Inf, Inf)
  )
  
  # Create temperature label
  temp_labels <- data.frame(
    theta = unique(elasticity_results$theta),
    label = paste0(unique(elasticity_results$theta), "\u00b0C")
  )
  
  # Sub-figure A: Sensitivity of asymptotic growth rate (\u03bb) by parameter type
  lambda_plot = ggplot(elasticity_results)  +
    # Scale color
    scale_color_manual(values = class_colors, name = "Size Class") +
    
    # Background
    geom_vline(xintercept = c(0.25, 0.50, 0.75), col = "grey75", linewidth = 0.5, linetype = 2) +
    geom_hline(yintercept = 0, col = "black", linewidth = 0.5) +
    
    # Plot lines
    stat_ecdf(aes(y = -lambda_elasticity*100, color = class_affected), 
              geom = "line", pad = FALSE, linewidth = 1) +
    
    # Temperature labels in bottom-right corner of each facet
    geom_label(data = temp_labels, aes(label = label),
               x = Inf, y = -Inf, hjust = 1, vjust = 0,
               label.padding = unit(0.15, "lines"),
               label.size = 0,
               label.r = unit(0, "lines"),
               fill = "white",
               alpha = 0.7,
               family = "Roboto-Bold", size = 3.5, color = "black") +
    
    # Facets
    facet_wrap(~ theta, nrow = 1) +
    
    # Labels
    labs(
      y = "Change in \u03bb (%)",
      x = "Cumulative proportion of simulations (%)"
    ) +
    
    # Theme
    theme_custom() +
    theme(
      aspect.ratio = 1,
      strip.background = element_blank(),
      strip.text = element_blank(),
      legend.position = "none",
      plot.margin = margin(t = 2, b = 0, l = 2, r = 2),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  
  # Sub-figure B: Sensitivity of phosphorus percentage (%P) by parameter type
  p_plot = ggplot(elasticity_results) +
    # Scale color
    scale_color_manual(values = class_colors, name = "Size Class") +
    
    # Mapping
    aes(y = -P_elasticity*100, color = class_affected) +
    
    # Background
    geom_vline(xintercept = c(0.25, 0.50, 0.75), col = "grey75", linewidth = 0.5, linetype = 2) +
    geom_hline(yintercept = 0, col = "black", linewidth = 0.5) +
    
    # Plot lines
    stat_ecdf(geom = "step", pad = FALSE, linewidth = 1) +
    
    # Temperature labels in bottom-right corner of each facet
    geom_label(data = temp_labels, aes(label = label),
               x = Inf, y = -Inf, hjust = 1, vjust = 0,
               label.padding = unit(0.15, "lines"),
               label.size = 0,
               label.r = unit(0, "lines"),
               fill = "white",
               alpha = 0.7,
               family = "Roboto-Bold", size = 3.5, color = "black") +
    
    # Facets
    facet_wrap(~ theta, nrow = 1) +
    
    # Labels
    labs(x = "Cumulative proportion of simulations (%)", 
         y = "Change in populational P rate (%)"
    ) +
    
    # Theme
    theme_custom() +
    theme(
      aspect.ratio = 1,
      strip.background = element_blank(),
      strip.text = element_blank(),
      legend.position = "bottom",
      plot.margin = margin(t = 0, b = 2, l = 2, r = 2)
    )
  
  
  # Scales
  if (analysis_type == "survival") {
    lambda_plot = lambda_plot +
      # Scale axis
      scale_x_continuous(breaks = seq(0, 1, by = 0.1), minor_breaks = seq(0, 1, by = 0.05), labels = scales::label_percent(suffix = ""), expand = c(0.01, 0.01)) +
      scale_y_continuous(breaks = seq(-100, 100, by = 1), minor_breaks = seq(0, 1, by = 0.1), expand = c(0.01, 0.01)) +
      coord_cartesian(xlim = c(0, 1), ylim = c(-10, 0), clip = "on")
    
    p_plot = p_plot +
      # Scale axis
      scale_x_continuous(breaks = seq(0, 1, by = 0.1), minor_breaks = seq(0, 1, by = 0.05), labels = scales::label_percent(suffix = ""), expand = c(0.01, 0.01)) +
      scale_y_continuous(breaks = seq(-100, 100, by = 1), minor_breaks = seq(-100, 100, by = 0.5), expand = c(0.01, 0.01), 
                         transform = squeeze_trans) +
      coord_cartesian(xlim = c(0, 1), ylim = c(-5, 6), clip = "on")
    
  } else if (analysis_type == "fecundity") {
    lambda_plot = lambda_plot +
      # Scale axis
      scale_x_continuous(breaks = seq(0, 1, by = 0.1), minor_breaks = seq(0, 1, by = 0.05), labels = scales::label_percent(suffix = ""), expand = c(0.01, 0.01)) +
      scale_y_continuous(breaks = seq(-100, 100, by = 0.25), minor_breaks = seq(-100, 100, by = 0.125), expand = c(0.01, 0.01)) +
      coord_cartesian(xlim = c(0, 1), ylim = c(-2.5, 0), clip = "on")
    
    p_plot = p_plot +
      # Scale axis
      scale_x_continuous(breaks = seq(0, 1, by = 0.1), minor_breaks = seq(0, 1, by = 0.05), labels = scales::label_percent(suffix = ""), expand = c(0.01, 0.01)) +
      scale_y_continuous(breaks = seq(-100, 100, by = 0.025), minor_breaks = seq(-100, 100, by = 0.0125), expand = c(0.01, 0.01), 
                         transform = squeeze_trans) +
      coord_cartesian(xlim = c(0, 1), ylim = c(-0.1, 0.15), clip = "on")
    
  } else if (analysis_type == "growth") {
    
    major_br = c(seq(-50, -10-1e-9, by = 10), seq(-10, -2-1e-9, by = 2), seq(-2, 2-1e-9, by = 0.5), seq(2, 10-1e-9, by = 2), seq(10, 50, by = 10))
    minor_br = c(seq(-50, -10-1e-9, by = 1), seq(-10, -2-1e-9, by = 0.5), seq(-2, 2-1e-9, by = 0.25), seq(2, 10-1e-9, by = 0.5), seq(2, 50, by = 1))
    
    lambda_plot = lambda_plot +
      # Scale axis
      scale_x_continuous(breaks = seq(0, 1, by = 0.1), minor_breaks = seq(0, 1, by = 0.05), labels = scales::label_percent(suffix = ""), expand = c(0.01, 0.01)) +
      scale_y_continuous(breaks = major_br, minor_breaks = minor_br, expand = c(0.01, 0.01), transform = squeeze_trans) +
      coord_cartesian(xlim = c(0, 1), ylim = c(-2, 50), clip = "on")
    
    p_plot = p_plot +
      # Scale axis
      scale_x_continuous(breaks = seq(0, 1, by = 0.1), minor_breaks = seq(0, 1, by = 0.05), labels = scales::label_percent(suffix = ""), expand = c(0.01, 0.01)) +
      scale_y_continuous(breaks = seq(-100, 100, by = 0.5), minor_breaks = seq(-100, 100, by = 0.25), expand = c(0.01, 0.01), 
                         transform = squeeze_trans) +
      coord_cartesian(xlim = c(0, 1), ylim = c(-4, 1), clip = "on")
    
  }
  
  # Use patchwork to assemble the plots
  library(patchwork)
  
  # Assemble the plots
  final_plot <- (lambda_plot / p_plot) +
    plot_annotation(tag_levels = 'A', tag_prefix = '(', tag_suffix = ')')  +
    # Adjust layout
    plot_layout(
      heights = c(1, 1),    # Equal height for both plots
      guides = "collect"
    ) &
    theme(legend.position = "bottom")
  
  return(final_plot)
}

#' Create J1 and A3 survival effect figure
#'
#' @description 
#' This function creates Figure 3 for the manuscript showing 
#' the effect of J1 and A3 survival on the relationship between
#' asymptotic growth rate and phosphorus content.
#'
#' @param figure_data Data filtered for J1 and A3 classes
#' @param ref_points Reference points from annual simulation
#'
#' @return A ggplot object for Figure 3
#' @export
#' @importFrom data.table as.data.table
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer
#' @importFrom data.table as.data.table
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer
create_j1_a3_survival_effect <- function(figure_data, ref_points) {
  # Create subdata
  figure_subdata = figure_data[, .(theta, class_var, J1 = surv_rate_J1, A3 = surv_rate_A3, lambda, mean_percentP)] |> 
    tidyr::pivot_longer(cols = c(J1, A3), names_to = "class", names_transform = as.factor, values_to = "surv_rate") |> 
    dplyr::mutate(class = factor(class, levels = c("J1", "A3")), theta = as.factor(theta)) |> 
    data.table::as.data.table()
  
  # Create temperature label
  temp_labels <- data.frame(
    theta = unique(figure_subdata$theta),
    label = paste0(unique(figure_subdata$theta), "\u00b0C")
  )
  
  # Create figure
  fig_3 <- ggplot() +
    # Scales
    scale_x_continuous(breaks = seq(0, 5, by = 0.2), minor_breaks = seq(0, 5, by = 0.1), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(0, 2, by = 0.010), minor_breaks = seq(0, 2, by = 0.005), expand = c(0, 0)) +
    coord_cartesian(xlim = c(0.3, 1.400001), ylim = c(0.98, 1.02000001), clip = "on") +
    
    # Style
    scale_color_manual(
      values = c(J1 = "#9ACD32", A3 = "#EE6AA7"),
      name = "Size Class"
    ) +
    
    # Background line for \u03bb = 1
    geom_vline(xintercept = 1, linewidth = 0.5, col = "darkgrey") +
    
    # Lines for survival variations
    geom_line(
      data = figure_subdata,
      aes(x = lambda, y = mean_percentP * 100, color = class_var),
      linewidth = 1, lineend = "round"
    ) +
    
    # Reference points (annual mean rates)
    geom_point(
      data = ref_points,
      aes(x = lambda, y = mean_percentP * 100),
      color = "black",
      size = 1
    ) +
    
    # Temperature labels in bottom-right corner of each facet
    geom_label(data = temp_labels, aes(label = label),
               x = Inf, y = -Inf, hjust = 1, vjust = 0,
               label.padding = unit(0.15, "lines"),
               label.size = 0,
               label.r = unit(0, "lines"),
               fill = "white",
               alpha = 0.7,
               family = "Roboto-Bold", size = 3.5, color = "black") +
    
    # Labels
    labs(
      x = "Asymptotic Growth Rate (\u03bb)",
      y = "Populational P rate (%)"
    ) +
    
    # Facets
    facet_wrap(~ theta, nrow = 1) +
    
    # Theme
    theme_custom() +
    theme(
      aspect.ratio = 1,
      strip.background = element_blank(),
      strip.text = element_blank(),
      panel.spacing.x = unit(0.7, "lines"),
      legend.position = "bottom",
      plot.margin = margin(t = 5, b = 5, l = 5, r = 8)
    )
  
  return(fig_3)
}

#' Create survival gradient effect figure
#'
#' @description 
#' This function creates Figure 4 for the manuscript showing 
#' the effect of survival rate gradients on the relationship between
#' asymptotic growth rate and phosphorus content.
#'
#' @param multi_param_results Data from multi-parameter simulations
#' @param monthly_results Monthly simulation results
#'
#' @return A ggplot object for Figure 4
#' 
#' @export
#' @importFrom data.table as.data.table
#' @importFrom dplyr mutate
#' @importFrom tagger tag_facets
#' @importFrom tidyr pivot_longer expand_grid
#' @importFrom data.table as.data.table
#' @importFrom dplyr mutate
#' @importFrom tagger tag_facets
#' @importFrom tidyr pivot_longer expand_grid
create_survival_gradient_effect <- function(multi_param_results, monthly_results) {
  # Create subdata
  simulations_data = multi_param_results[, .(theta, J1 = surv_rate_J1, A3 = surv_rate_A3, lambda, mean_percentP)] |> 
    tidyr::pivot_longer(cols = c(J1, A3), names_to = "class", names_transform = as.factor, values_to = "surv_rate") |> 
    dplyr::mutate(class = factor(class, levels = c("J1", "A3")), theta = as.factor(theta)) |> 
    data.table::as.data.table()
  
  # Create temperature label
  temp_labels = tidyr::expand_grid(class = unique(simulations_data$class), theta = unique(simulations_data$theta)) |> 
      as.data.frame() |> 
      dplyr::mutate(label = paste0(class, " - ", theta, "\u00b0C"))

  # Create figure
  fig_4 = ggplot() +
    # Scales
    scale_x_continuous(breaks = seq(0, 5, by = 0.2), minor_breaks = seq(0, 5, by = 0.1), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(0, 2, by = 0.010), minor_breaks = seq(0, 2, by = 0.005), expand = c(0, 0)) +
    coord_cartesian(xlim = c(0, 1.500001), ylim = c(0.95, 1.05000001), clip = "on") +
    
    # Style
    scale_color_viridis_c(name = "Class survival", option = "mako", limits = c(0, 1),
                          guide = guide_colorbar(
                            title.position = "left",        
                            title.vjust = 1,                 
                            barwidth = unit(100, "points"),        
                            barheight = unit(10, "points")      
                          )
    ) +
    
    # Background line for \u03bb = 1
    geom_vline(xintercept = 1, linewidth = 0.5, col = "darkgrey") +
    
    # Simulated points
    geom_point(
      data = simulations_data,
      aes(x = lambda, y = mean_percentP * 100, color = surv_rate),
      alpha = 0.6, size = 0.7, stroke = 0
    ) +
    
    # Monthly reference points
    geom_point(
      data = monthly_results,
      aes(x = lambda, y = mean_percentP * 100),
      color = "red", size = 1.2, shape = 18
    ) +
    
    # Temperature labels in bottom-right corner of each facet
    geom_label(data = temp_labels, aes(label = label),
               x = Inf, y = -Inf, hjust = 1, vjust = 0,
               label.padding = unit(0.15, "lines"),
               label.size = 0,
               label.r = unit(0, "lines"),
               fill = "white",
               alpha = 0.7,
               family = "Roboto-Bold", size = 3.5, color = "black") +
    
    # Labels
    labs(
      x = "Asymptotic Growth Rate (\u03bb)",
      y = "Populational P rate (%)"
    ) +
    
    # Facets
    facet_wrap(class ~ theta, nrow = 2) +
    tagger::tag_facets(tag = "rc", position = "tl", tag_levels = c("A", "1"), tag_prefix = "(", tag_suffix = ")", tag_sep = "") +
    
    # Theme
    theme_custom() +
    theme(
      aspect.ratio = 1,
      strip.background = element_blank(),
      strip.text = element_blank(),
      panel.spacing.y = unit(0.7, "lines"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      plot.margin = margin(t = 5, b = 5, l = 5, r = 8)
    )

  return(fig_4)
}

#' Create a plot of significant transition rates between size classes
#'
#' @description 
#' This function creates Figure S1 showing significant transition rates between size classes
#' as a function of temperature.
#'
#' @param transition_data Data frame containing transition rates (output from calculate_transition_rates)
#' @param significance_threshold Minimum transition rate to be considered significant (default: 0.01)
#' @param reference_temps Vector of reference temperatures to highlight with vertical lines
#' @param L_max Maximum size in mm
#' @param delta_t Time step in days
#'
#' @return A ggplot object with the transition rates visualization for Figure S1
#' 
#' @importFrom ggplot2 ggplot aes geom_vline geom_line scale_x_continuous scale_y_continuous coord_cartesian labs facet_grid
#' @importFrom dplyr group_by summarise filter inner_join
#' 
#' @export
#' @importFrom dplyr filter summarise group_by inner_join
create_transition_rates_plot <- function(transition_data, 
                                         significance_threshold = 0.01,
                                         reference_temps = c(8, 12, 16),
                                         L_max,
                                         delta_t) {
  # Identify significant transitions
  significant_transitions <- transition_data |> 
    dplyr::group_by(X, Y) |> 
    dplyr::summarise(max_value = max(Z), .groups = "drop") |> 
    dplyr::filter(max_value > significance_threshold)
  
  # Filter data for significant transitions only
  filtered_data <- transition_data |> 
    dplyr::inner_join(significant_transitions, by = c("X", "Y"))
  
  # Create the plot
  plot <- ggplot(filtered_data) +
    # Scales
    scale_x_continuous(breaks = seq(0, 25, by = 5), 
                       minor_breaks = seq(0, 25, by = 2.5), 
                       expand = c(0.01, 0.01)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.25), 
                       minor_breaks = seq(0, 1, by = 0.125), 
                       expand = c(0.01, 0.01)) +
    coord_cartesian(xlim = c(0, 25), ylim = c(0, 1), clip = "on") +
    
    # Elements
    aes(x = theta, y = Z) +
    geom_vline(xintercept = reference_temps, col = "#A0A0A0", linetype = "dashed") +
    geom_line(linewidth = 1, color = "#0070C0") +
    
    # Labels
    labs(x = "Temperature (°C)",
         y = "Transition rate",
         # title = "Significant transition rates between size classes",
         # subtitle = paste("Time step: ", delta_t, " days", sep = "")
    ) +
    
    # Theme and facets
    theme_custom() +
    facet_grid(vars(X), vars(Y)) +
    theme(aspect.ratio = 1,
          panel.spacing = unit(x = 0.5, units = "lines"))
  
  return(plot)
}

#' Create model parameter elasticity figure
#'
#' @description 
#' This function creates a figure showing the sensitivity of population growth rate
#' and phosphorus content to changes in underlying model parameters.
#'
#' @param elasticity_results Data table with model parameter elasticity results
#'
#' @return A ggplot object for the model parameter elasticity figure
#' @export
#' @importFrom scales trans_new label_percent
create_model_parameter_elasticity_figure <- function(elasticity_results) {
  # Define parameter categories for grouping and coloring
  param_categories <- data.frame(
    parameter_name = c(
      "growth_rate_coef", "growth_rate_intercept", "L_max",
      "molt_cycle_a", "molt_cycle_b", "molt_cycle_c", "molt_cycle_d",
      "sexratio", "gravid", "fertil_A1", "fertil_A2", "fertil_A3"
    ),
    category = c(
      rep("Growth", 3),
      rep("Molt Cycle", 4),
      rep("Reproduction", 2),
      rep("Fertility", 3)
    ),
    display_name = c(
      "Growth rate: Temperature coef.", "Growth rate: Intercept", "Maximum size (L_max)",
      "Molt cycle: Parameter a", "Molt cycle: Parameter b", "Molt cycle: Parameter c", "Molt cycle: Parameter d",
      "Sex ratio", "Proportion gravid", "Fertility: A1", "Fertility: A2", "Fertility: A3"
    )
  )
  
  # Add category and display name to results
  results_with_categories <- merge(
    elasticity_results,
    param_categories,
    by = "parameter_name",
    all.x = TRUE
  )
  
  # Set factor levels for proper ordering in plots
  results_with_categories$display_name <- factor(
    results_with_categories$display_name,
    levels = param_categories$display_name
  )
  
  results_with_categories$category <- factor(
    results_with_categories$category,
    levels = c("Growth", "Molt Cycle", "Reproduction", "Fertility")
  )
  
  # Define colors palette
  param_colors <- c(
    # Growth parameters
    "Growth rate: Temperature coef." = "#1a9850",
    "Growth rate: Intercept" = "#66bd63",
    "Maximum size (L_max)" = "#a6d96a",
    
    # Molt cycle parameters
    "Molt cycle: Parameter a" = "#d73027",
    "Molt cycle: Parameter b" = "#f46d43",
    "Molt cycle: Parameter c" = "#fdae61",
    "Molt cycle: Parameter d" = "#fee090",
    
    # Reproduction parameters
    "Sex ratio" = "#4575b4",
    "Proportion gravid" = "#74add1",
    
    # Fertility parameters
    "Fertility: A1" = "#762a83",
    "Fertility: A2" = "#9970ab",
    "Fertility: A3" = "#c2a5cf"
  )
  
  # Axis transformation
  squeeze_trans <- scales::trans_new(
    name = "squeeze",
    transform = function(x) sign(x) * log1p(abs(x)),
    inverse = function(x) sign(x) * (exp(abs(x)) - 1),
    domain = c(-Inf, Inf)
  )
  
  # Create temperature label
  temp_labels <- data.frame(
    theta = unique(results_with_categories$theta),
    label = paste0(unique(results_with_categories$theta), "\u00b0C")
  )
  
  # Categories
  categories_levels = levels(results_with_categories$category)
  
  # Figures list
  figures <- setNames(
    lapply(categories_levels, function(cat) {
      list()
    }),
    categories_levels
  )
  
  # Use patchwork to assemble the plots
  library(patchwork)
  
  for (i in 1:length(categories_levels)) {
    # Select sub-data
    data = results_with_categories[category == categories_levels[i]]
    
    # Sub-figure A: Sensitivity of asymptotic growth rate (\u03bb) by parameter type
    lambda_plot = ggplot(data)  +
      # Scales
      scale_x_continuous(breaks = seq(0, 1, by = 0.1), minor_breaks = seq(0, 1, by = 0.05), labels = scales::label_percent(suffix = ""), expand = c(0.01, 0.01)) +
      scale_color_manual(values = param_colors, name = "Parameter") +
      
      # Background
      geom_vline(xintercept = c(0.25, 0.50, 0.75), col = "grey75", linewidth = 0.5, linetype = 2) +
      geom_hline(yintercept = 0, col = "black", linewidth = 0.5) +
      
      # Plot lines
      stat_ecdf(aes(y = -lambda_elasticity*100, color = display_name), 
                geom = "line", pad = FALSE, linewidth = 1) +
      
      # Temperature labels in bottom-right corner of each facet
      geom_label(data = temp_labels, aes(label = label),
                 x = Inf, y = -Inf, hjust = 1, vjust = 0,
                 label.padding = unit(0.15, "lines"),
                 label.size = 0,
                 label.r = unit(0, "lines"),
                 fill = "white",
                 alpha = 0.7,
                 family = "Roboto-Bold", size = 3.5, color = "black") +
      
      # Facets
      facet_wrap( ~ theta, ncol = 3) +
      
      # Labels
      labs(
        y = "Change in \u03bb (%)",
        x = "Cumulative proportion of simulations (%)"
      ) +
      
      # Theme
      theme_custom() +
      theme(
        aspect.ratio = 1,
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "none",
        plot.margin = margin(t = 2, b = 0, l = 2, r = 2),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
    
    # Sub-figure B: Sensitivity of phosphorus percentage (%P) by parameter type
    p_plot = ggplot(data) +
      # Scales
      scale_x_continuous(breaks = seq(0, 1, by = 0.1), minor_breaks = seq(0, 1, by = 0.05), labels = scales::label_percent(suffix = ""), expand = c(0.01, 0.01)) +
      scale_color_manual(values = param_colors, name = "Parameter") +
      
      # Mapping
      aes(y = -P_elasticity*100, color = display_name) +
      
      # Background
      geom_vline(xintercept = c(0.25, 0.50, 0.75), col = "grey75", linewidth = 0.5, linetype = 2) +
      geom_hline(yintercept = 0, col = "black", linewidth = 0.5) +
      
      # Plot lines
      stat_ecdf(geom = "line", pad = FALSE, linewidth = 1) +
      
      # Temperature labels in bottom-right corner of each facet
      geom_label(data = temp_labels, aes(label = label),
                 x = Inf, y = -Inf, hjust = 1, vjust = 0,
                 label.padding = unit(0.15, "lines"),
                 label.size = 0,
                 label.r = unit(0, "lines"),
                 fill = "white",
                 alpha = 0.7,
                 family = "Roboto-Bold", size = 3.5, color = "black") +
      
      # Facets
      facet_wrap( ~ theta) +
      
      # Labels
      labs(x = "Cumulative proportion of simulations (%)", 
           y = "Change in populational P rate (%)"
      ) +
      
      # Theme
      theme_custom() +
      theme(
        aspect.ratio = 1,
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(t = 0, b = 2, l = 2, r = 2)
      )
    
    # Assemble the plots
    final_plot <- (lambda_plot / p_plot) +
      plot_annotation(tag_levels = 'A', tag_prefix = '(', tag_suffix = ')')  +
      # Adjust layout
      plot_layout(
        heights = c(1, 1),    # Equal height for both plots
        guides = "collect"
      ) &
      theme(legend.position = "bottom")
    
    # Figures saving
    figures[[categories_levels[i]]] = final_plot
    
  }
  
  return(figures)
}