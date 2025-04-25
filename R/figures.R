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
#' 
#' @import gglot2 patchwork
#' @importFrom scales percent trans_new
#' @importFrom showtext showtext_auto
#' 
#' @export
create_elasticity_figure <- function(elasticity_results) {
  # Create subdata
  lambda_data <- elasticity_results[, .(theta, class_affected, lambda_elasticity)]
  p_data <- elasticity_results[, .(theta, class_affected, P_elasticity)]
  
  # Create temperature label
  temp_labels <- data.frame(
    theta = unique(elasticity_results$theta),
    label = paste0(unique(elasticity_results$theta), "\u00b0C")
  )
  
  # Palette de couleurs pour les classes
  class_colors <- c(
    J1 = "#9ACD32", 
    J2 = "#FFD700", 
    A1 = "#8DEEEE", 
    A2 = "#AB82FF", 
    A3 = "#EE6AA7"
  )
  
  # Sub-figure A: Sensitivity of asymptotic growth rate (\u03bb)
  lambda_plot <- ggplot(lambda_data) +
    
    # Scale axis
    scale_x_continuous(breaks = seq(0, 1, by = 0.1), minor_breaks = seq(0, 1, by = 0.05), labels = scales::percent, expand = c(0.01, 0.01)) +
    scale_y_continuous(breaks = seq(-20, 20, by = 1), minor_breaks = seq(-20, 20, by = 0.5), expand = c(0.01, 0.01)) +
    coord_cartesian(xlim = c(0, 1), ylim = c(-10, 0), clip = "on") +
    
    # Scale color
    scale_color_manual(values = class_colors, name = "Size Class") +
    
    # Mapping
    aes(y = -lambda_elasticity*100, color = class_affected) +
    
    # Background
    geom_vline(xintercept = c(0.25, 0.50, 0.75), col = "grey75", linewidth = 0.5, linetype = 2) +
    
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
    labs(x = "Cumulative proportion of simulations", 
         y = "Change in \u03bb (%)") +
    
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

  # Sub-figure B: Sensitivity of phosphorus percentage (%P)
  squeeze_trans <- scales::trans_new(
    name = "squeeze",
    transform = function(x) sign(x) * log1p(abs(x)),
    inverse = function(x) sign(x) * (exp(abs(x)) - 1),
    domain = c(-Inf, Inf)
  )
  
  p_plot <- ggplot(p_data) +
    
    # Scale axis
    scale_x_continuous(breaks = seq(0, 1, by = 0.1), minor_breaks = seq(0, 1, by = 0.05), labels = scales::percent, expand = c(0.01, 0.01)) +
    scale_y_continuous(breaks = seq(-20, 20, by = 1), minor_breaks = seq(-20, 20, by = 0.5), expand = c(0.01, 0.01), 
                       transform = squeeze_trans) +
    coord_cartesian(xlim = c(0, 1), ylim = c(-4, 4), clip = "on") +
    
    # Scale color
    scale_color_manual(values = class_colors, name = "Size Class") +
    
    # Mapping
    aes(y = -P_elasticity*100, color = class_affected) +
    
    # Background
    geom_vline(xintercept = c(0.25, 0.50, 0.75), col = "grey75", linewidth = 0.5, linetype = 2) +

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
    labs(x = "Cumulative proportion of simulations", 
         y = "Change in populational P rate (%)"
    ) +
    
    # Theme
    theme_custom() +
    theme(
      aspect.ratio = 1,
      strip.background = element_blank(),
      strip.text = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      plot.margin = margin(t = 0, b = 2, l = 2, r = 2)
    )

  # Use patchwork to assemble the plots
  library(patchwork)
  
  # Assemble the plots
  final_plot <- lambda_plot / p_plot +
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
#' @importFrom data.table as.data.table
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer expand_grid
#' 
#' @export
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
