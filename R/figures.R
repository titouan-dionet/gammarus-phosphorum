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
         y = "Individual phosphorus content (%)") +
    
    # Theme
    theme_custom()
  
  return(p_plot)
}

#' Create elasticity figure
#'
#' @description 
#' This function creates Figure 2 for the manuscript showing 
#' the sensitivity of population growth rate and phosphorus content
#' to survival rates.
#'
#' @param elasticity_results Data table with elasticity analysis results
#'
#' @return A ggplot object for Figure 2
#' @export
create_elasticity_figure <- function(elasticity_results) {
  # Sub-figure A: Sensitivity of asymptotic growth rate (\u03bb)
  fig_2a <- ggplot(elasticity_results) +
    stat_ecdf(aes(x = lambda_elasticity, color = class_affected), 
              geom = "line", pad = FALSE) +
    facet_wrap(~ theta, labeller = labeller(theta = function(x) paste("Temperature =", x, "\u00b0C"))) +
    scale_color_manual(
      values = c(J1 = "#9ACD32", J2 = "#FFD700", A1 = "#8DEEEE", A2 = "#AB82FF", A3 = "#EE6AA7"),
      name = "Size Class"
    ) +
    labs(
      x = "Reduction in \u03bb (%)",
      y = "Cumulative proportion of simulations",
      title = "Sensitivity of asymptotic population growth rate (\u03bb)",
      subtitle = "Effect of 10% reduction in survival rates"
    ) +
    theme_custom() +
    theme(legend.position = "bottom")
  
  # Sub-figure B: Sensitivity of phosphorus percentage (%P)
  fig_2b <- ggplot(elasticity_results) +
    stat_ecdf(aes(x = P_elasticity, color = class_affected), 
              geom = "line", pad = FALSE) +
    facet_wrap(~ theta, labeller = labeller(theta = function(x) paste("Temperature =", x, "\u00b0C"))) +
    scale_color_manual(
      values = c(J1 = "#9ACD32", J2 = "#FFD700", A1 = "#8DEEEE", A2 = "#AB82FF", A3 = "#EE6AA7"),
      name = "Size Class"
    ) +
    labs(
      x = "Change in %P",
      y = "Cumulative proportion of simulations",
      title = "Sensitivity of population-level phosphorus percentage (%P)",
      subtitle = "Effect of 10% reduction in survival rates"
    ) +
    theme_custom() +
    theme(legend.position = "bottom")
  
  # Combine the sub-figures
  fig_2 <- gridExtra::grid.arrange(fig_2a, fig_2b, ncol = 1)
  
  return(fig_2)
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
create_j1_a3_survival_effect <- function(figure_data, ref_points) {
  # Create figure
  fig_3 <- ggplot() +
    # Format graphique
    scale_x_continuous(breaks = seq(0, 5, by = 0.2), minor_breaks = seq(0, 5, by = 0.1), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(0, 2, by = 0.010), minor_breaks = seq(0, 2, by = 0.005), expand = c(0, 0)) +
    coord_cartesian(xlim = c(0.3, 1.400001), ylim = c(0.98, 1.02000001), clip = "on") +
    
    # Background
    geom_vline(xintercept = 1, linewidth = 0.5, col = "darkgrey") +
    
    # Lines for survival variations
    geom_line(
      data = figure_data,
      aes(x = lambda, y = mean_percentP * 100, color = class_var),
      linetype = 1, linewidth = 1, lineend = "round"
    ) +
    
    # Reference points (annual mean rates)
    geom_point(
      data = ref_points,
      aes(x = lambda, y = mean_percentP * 100),
      color = "black",
      size = 1
    ) +
    
    # Style
    scale_color_manual(
      values = c(J1 = "#9ACD32", A3 = "#EE6AA7"),
      name = "Size Class", guide = "none"
    ) +
    
    # Labels
    labs(
      x = "Asymptotic Growth Rate (\u03bb)",
      y = "Phosphorus Content (%)",
      title = "Relationship between Population Growth Rate and Phosphorus Content",
      subtitle = "Effect of varying survival rates for smallest (J1) and largest (A3) size classes"
    ) +
    
    facet_grid(vars(class_var), vars(theta))+
    theme_custom()
  
  return(fig_3)
}

#' Create a subplot for the survival gradient effect figure
#'
#' @description
#' Helper function to create a subplot for Figure 4
#'
#' @param data Data frame with simulation results
#' @param theta_val Current temperature value
#' @param class_name Size class name
#' @param monthly_data Monthly simulation results
#'
#' @return A ggplot object for the subplot
#' @keywords internal
create_survival_gradient_subplot <- function(data, theta_val, class_name, monthly_data) {
  ggplot(data[data$theta == theta_val, ]) +
    # Background with reference lines
    geom_vline(xintercept = 1, linewidth = 0.3, linetype = "dashed", color = "grey70") +
    
    # Simulated points
    geom_point(
      aes_string(x = "lambda", y = "mean_percentP * 100", color = paste0("surv_rate_", class_name)),
      alpha = 0.6,
      size = 0.7,
      stroke = 0
    ) +
    
    # Monthly reference points
    geom_point(
      data = monthly_data[monthly_data$theta == theta_val, ],
      aes(x = lambda, y = mean_percentP * 100),
      color = "red",
      size = 1.2,
      shape = 18
    ) +
    
    # Scales
    scale_color_viridis_c(
      name = paste(class_name, "Survival"),
      option = "viridis",
      limits = c(0, 1)
    ) +
    
    # Axis limits for consistency
    scale_x_continuous(
      limits = c(0.1, 1.4),
      breaks = seq(0.2, 1.4, by = 0.4)
    ) +
    
    scale_y_continuous(
      limits = c(0.96, 1.05), 
      breaks = seq(0.96, 1.05, by = 0.03)
    ) +
    
    # Labels
    labs(
      title = paste0(theta_val, "\u00b0C - ", class_name)
    ) +
    
    theme_custom() +
    theme(
      legend.position = "right",
      legend.key.size = unit(0.8, "lines"),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      plot.title = element_text(size = 10, hjust = 0.5),
      axis.title = element_text(size = 9)
    )
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
#' @export
create_survival_gradient_effect <- function(multi_param_results, monthly_results) {
  # Temperatures and focal classes
  temps <- unique(multi_param_results$theta)
  focal_classes <- c("J1", "A3")
  
  # Create all subplots
  plot_grid <- list()
  row_count <- 1
  
  for (t in temps) {
    for (cls in focal_classes) {
      subplot <- create_survival_gradient_subplot(multi_param_results, t, cls, monthly_results)
      
      # Add axis labels only where needed
      if (row_count == 3) {
        subplot <- subplot + labs(x = "Asymptotic Growth Rate (\u03bb)")
      } else {
        subplot <- subplot + labs(x = "")
      }
      
      if (cls == "J1") {
        subplot <- subplot + labs(y = "Phosphorus Content (%)")
      } else {
        subplot <- subplot + labs(y = "")
      }
      
      plot_grid[[length(plot_grid) + 1]] <- subplot
    }
    row_count <- row_count + 1
  }
  
  # Combine the subplots
  fig_4 <- gridExtra::grid.arrange(
    grobs = plot_grid,
    ncol = 2,
    nrow = 3,
    top = grid::textGrob(
      "Population-Level Phosphorus Content vs. Growth Rate",
      gp = grid::gpar(fontface = "bold", fontsize = 12)
    ),
    bottom = grid::textGrob(
      "Colored by survival rate. Red points show monthly empirical values.",
      gp = grid::gpar(fontsize = 9, fontface = "italic")
    )
  )
  
  return(fig_4)
}