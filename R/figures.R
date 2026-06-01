###############################################################################
# figures.R
# Figure creation functions for the Gammarus-Phosphorum project
# Author: Titouan Dionet
# Date: April 2025
###############################################################################

#' Create conceptual figure for the Growth Rate Hypothesis at individual
#' and population levels
#'
#' @description
#' Creates Figure 1 for the manuscript introduction, illustrating the
#' Growth Rate Hypothesis (GRH) at the intra-specific (individual) level
#' and four alternative hypotheses (H1-H4) for its potential extension to
#' the population level. All curves are schematic and do not represent
#' empirical data. The figure is composed of three panels: (a) individual
#' phosphorus content and relative growth rate as a function of body size,
#' (b) the resulting positive individual-level GRH relationship, and (c)
#' four hypothetical relationships between mean population phosphorus
#' content and the asymptotic population growth rate (lambda).
#'
#' @return A patchwork ggplot object representing Figure 1
#' @export
create_grh_conceptual_figure <- function() {
  # ____________________________________________________________________________
  # Shared style elements ----
  # ____________________________________________________________________________

  arrow_style <- grid::arrow(length = unit(0.2, "cm"), type = "closed")

  base_theme <- theme_custom() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      axis.title = element_text(size = 9, family = "Roboto-Medium"),
      plot.tag = element_text(size = 10, face = "bold", family = "Roboto-Bold"),
      plot.title = element_text(
        size = 9,
        family = "Roboto-Medium",
        hjust = 0.5
      ),
      plot.title.position = "plot"
    )

  # ____________________________________________________________________________
  # Panel (a): Individual %P and RGR vs body size ----
  # ____________________________________________________________________________

  x_size <- seq(0, 10, length.out = 200)

  panel_a_data <- data.frame(
    x = rep(x_size, 2),
    y = c(
      exp(-0.35 * x_size) * 0.85 + 0.15, # %P: decreasing exponential
      exp(-0.5 * x_size) * 0.7 + 0.05 # RGR: steeper decrease
    ),
    curve = rep(c("Individual %P", "Relative Growth Rate"), each = 200)
  )

  panel_a <- ggplot(panel_a_data, aes(x = x, y = y, linetype = curve)) +
    geom_line(linewidth = 0.7, color = "black") +
    ggplot2::scale_linetype_manual(
      values = c("Individual %P" = "solid", "Relative Growth Rate" = "dashed"),
      guide = "none"
    ) +
    # Axis arrows
    ggplot2::annotate(
      "segment",
      x = 0,
      xend = 10.5,
      y = -Inf,
      yend = -Inf,
      arrow = arrow_style,
      linewidth = 0.5
    ) +
    ggplot2::annotate(
      "segment",
      x = -Inf,
      xend = -Inf,
      y = 0,
      yend = 1.08,
      arrow = arrow_style,
      linewidth = 0.5
    ) +
    # Curve labels
    ggplot2::annotate(
      "text",
      x = 7.5,
      y = 0.30,
      label = "Individual %P",
      size = 2.8,
      family = "Roboto-Medium",
      hjust = 0
    ) +
    ggplot2::annotate(
      "text",
      x = 7.5,
      y = 0.12,
      label = "RGR",
      size = 2.8,
      family = "Roboto-Medium",
      hjust = 0,
      fontface = "italic"
    ) +
    coord_cartesian(xlim = c(0, 10.5), ylim = c(0, 1.1), clip = "off") +
    labs(
      title = "Growth Rate Hypothesis (GRH)\napplied at the intra-specific level",
      x = "Individual size",
      y = "Individual %P",
      tag = "(A)"
    ) +
    base_theme +
    theme(
      plot.title = element_text(
        size = 9,
        hjust = 0.5,
        family = "Roboto-Medium",
        margin = margin(b = 4)
      ),
      plot.title.position = "plot",
      axis.title.y.right = element_text(size = 9)
    )

  # ____________________________________________________________________________
  # Panel (b): Insert -- Individual %P vs individual RGR ----
  # ____________________________________________________________________________

  x_rgr <- seq(0, 1, length.out = 100)

  panel_b_data <- data.frame(
    x = x_rgr,
    y = 0.2 + 0.75 * x_rgr
  )

  panel_b <- ggplot(panel_b_data, aes(x = x, y = y)) +
    geom_line(linewidth = 0.7, color = "black") +
    ggplot2::annotate(
      "segment",
      x = 0,
      xend = 1.08,
      y = -Inf,
      yend = -Inf,
      arrow = arrow_style,
      linewidth = 0.4
    ) +
    ggplot2::annotate(
      "segment",
      x = -Inf,
      xend = -Inf,
      y = 0.15,
      yend = 1.0,
      arrow = arrow_style,
      linewidth = 0.4
    ) +
    coord_cartesian(xlim = c(0, 1.1), ylim = c(0.15, 1.0), clip = "off") +
    labs(x = "Individual RGR", y = "Individual %P", tag = "(B)") +
    base_theme +
    theme(
      axis.title = element_text(size = 7.5, family = "Roboto-Medium"),
      plot.tag = element_text(size = 9, face = "bold")
    )

  # ____________________________________________________________________________
  # Panel (c): Hypotheses H1-H4 at population level ----
  # ____________________________________________________________________________

  x_lambda <- seq(0, 10, length.out = 300)

  # H1: positive linear (GRH holds)
  h1 <- data.frame(x = x_lambda, y = 0.3 + 0.065 * x_lambda, hyp = "H1")
  # H2: flat (no relationship)
  h2 <- data.frame(x = x_lambda, y = rep(0.55, 300), hyp = "H2")
  # H3: negative linear (reverse GRH)
  h3 <- data.frame(x = x_lambda, y = 0.85 - 0.065 * x_lambda, hyp = "H3")
  # H4: non-monotonic (complex)
  h4 <- data.frame(
    x = x_lambda,
    y = 0.55 + 0.18 * sin(x_lambda * 0.75 - 0.5),
    hyp = "H4"
  )

  panel_c_data <- rbind(h1, h2, h3, h4)

  # Label positions (right end of each curve)
  labels_c <- data.frame(
    x = c(10, 10, 10, 10),
    y = c(
      tail(h1$y, 1) + 0.03,
      tail(h2$y, 1) + 0.03,
      tail(h3$y, 1) - 0.03,
      tail(h4$y, 1) + 0.03
    ),
    hyp = c("H1", "H2", "H3", "H4"),
    label = c("H1", "H2", "H3", "H4"),
    hjust = c(0, 0, 0, 0)
  )

  hyp_colors <- c(
    H1 = "#2166ac",
    H2 = "#4dac26",
    H3 = "#d01c8b",
    H4 = "#f1a340"
  )

  hyp_linetypes <- c(
    H1 = "solid",
    H2 = "dashed",
    H3 = "dotted",
    H4 = "dotdash"
  )

  panel_c <- ggplot(
    panel_c_data,
    aes(x = x, y = y, color = hyp, linetype = hyp)
  ) +
    geom_line(linewidth = 0.75) +
    geom_text(
      data = labels_c,
      aes(x = x, y = y, label = label, color = hyp),
      size = 3,
      fontface = "bold",
      hjust = -0.1,
      family = "Roboto-Bold",
      inherit.aes = FALSE
    ) +
    ggplot2::annotate(
      "text",
      x = 5,
      y = 1.06,
      label = "?",
      size = 8,
      color = "grey50",
      family = "Roboto-Bold"
    ) +
    scale_color_manual(values = hyp_colors, guide = "none") +
    ggplot2::scale_linetype_manual(values = hyp_linetypes, guide = "none") +
    ggplot2::annotate(
      "segment",
      x = 0,
      xend = 11,
      y = -Inf,
      yend = -Inf,
      arrow = arrow_style,
      linewidth = 0.5
    ) +
    ggplot2::annotate(
      "segment",
      x = -Inf,
      xend = -Inf,
      y = 0.15,
      yend = 1.1,
      arrow = arrow_style,
      linewidth = 0.5
    ) +
    coord_cartesian(xlim = c(0, 11.5), ylim = c(0.15, 1.15), clip = "off") +
    labs(
      x = expression("Population asymptotic growth rate (" * lambda * ")"),
      y = "Mean population %P",
      tag = "(C)",
      title = "Relationship between mean population %P\nand population-level growth rate?"
    ) +
    base_theme

  # ____________________________________________________________________________
  # Assemble with patchwork ----
  # ____________________________________________________________________________

  left_panel <- (panel_a / panel_b) +
    patchwork::plot_layout(heights = c(2, 1))

  final_figure <- (left_panel | panel_c) +
    patchwork::plot_layout(widths = c(1, 1.6))

  return(final_figure)
}

#' Create phosphorus content figure
#'
#' @description
#' This function creates Figure 2 for the manuscript showing
#' phosphorus content by size class.
#'
#' @param phosphorus_data Processed phosphorus data
#' @param stats_info Statistical results with significance letters
#'
#' @return A ggplot object for Figure 2
#'
#' @export
create_phosphorus_figure <- function(phosphorus_data, stats_info) {
  # Create figure
  p_plot <- ggplot(phosphorus_data) +
    # Formatting
    scale_y_continuous(
      breaks = seq(0, 100, by = 0.05),
      minor_breaks = seq(0, 100, by = 0.025),
      expand = c(0, 0)
    ) +
    coord_cartesian(ylim = c(0.8, 1.35), clip = "on") +

    ggplot2::scale_color_manual(
      values = c(
        J1 = "#9ACD32",
        J2 = "#FFD700",
        A1 = "#8DEEEE",
        A2 = "#AB82FF",
        A3 = "#EE6AA7",
        `neo-J1` = "#759837"
      ),
      name = "class",
      guide = "none"
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        J1 = "#9ACD32",
        J2 = "#FFD700",
        A1 = "#8DEEEE",
        A2 = "#AB82FF",
        A3 = "#EE6AA7",
        `neo-J1` = "#759837"
      ),
      name = "class",
      guide = "none"
    ) +

    # Data mapping
    aes(x = class, y = P_percent * 100, col = class) +

    # Points with outlier highlighted
    ggplot2::geom_jitter(
      aes(fill = class),
      shape = 21,
      size = 1,
      alpha = 0.4,
      width = 0.05
    ) +

    # Mean marker (star symbol)
    # stat_summary(fun = "mean", col = "red", size = 2, shape = 13, geom = "point") +
    geom_point(
      data = stats_info,
      aes(x = as.factor(trt), y = mean * 100, col = trt),
      shape = 13,
      size = 2
    ) +
    ggplot2::geom_errorbar(
      data = stats_info,
      aes(
        x = as.factor(trt),
        y = NULL,
        ymin = (mean - se) * 100,
        ymax = (mean + se) * 100,
        col = trt
      ),
      linewidth = 0.5,
      width = 0.2
    ) +

    # Significance letters
    geom_text(
      data = stats_info,
      aes(y = max * 100 + 0.05, label = signif_letter, x = as.factor(trt)),
      fontface = "bold",
      col = "black",
      family = "Roboto"
    ) +

    # Labels
    labs(x = "Size class", y = "Individual P content (%)") +

    # Theme
    theme_custom()

  return(p_plot)
}

#' Create comprehensive elasticity figure
#'
#' @description
#' This function creates Figure 3, S3 and S4 for the manuscript showing
#' the sensitivity of population growth rate and phosphorus content
#' to survival, fecundity, and growth rates.
#'
#' @param elasticity_results Data table with comprehensive elasticity analysis results.
#' @param analysis_type Type of analysis to coduct: \code{"survival"}, \code{"fecundity"} or \code{"growth"}.
#'
#' @return A ggplot object for Figure 3, S3 or S4
#' @export
#' @importFrom scales trans_new label_percent
#' @importFrom scales trans_new label_percent
create_elasticity_figure <- function(elasticity_results, analysis_type) {
  # Select data
  elasticity_results = elasticity_results[parameter_type == analysis_type]

  # Define a common color palette for all parameter types
  class_colors <- c(
    J1 = "#9ACD32",
    J2 = "#FFD700",
    A1 = "#8DEEEE",
    A2 = "#AB82FF",
    A3 = "#EE6AA7"
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
    theta = unique(elasticity_results$theta),
    label = paste0(unique(elasticity_results$theta), "\u00b0C")
  )

  # Sub-figure A: Sensitivity of asymptotic growth rate (\u03bb) by parameter type
  lambda_plot = ggplot(elasticity_results) +
    # Scale color
    scale_color_manual(values = class_colors, name = "Size Class") +

    # Background
    geom_vline(
      xintercept = c(0.25, 0.50, 0.75),
      col = "grey75",
      linewidth = 0.5,
      linetype = 2
    ) +
    geom_hline(yintercept = 0, col = "black", linewidth = 0.5) +

    # Plot lines
    stat_ecdf(
      aes(y = -lambda_elasticity * 100, color = class_affected),
      geom = "line",
      pad = FALSE,
      linewidth = 1
    ) +

    # Temperature labels in bottom-right corner of each facet
    geom_label(
      data = temp_labels,
      aes(label = label),
      x = Inf,
      y = -Inf,
      hjust = 1,
      vjust = 0,
      label.padding = unit(0.15, "lines"),
      label.size = 0,
      label.r = unit(0, "lines"),
      fill = "white",
      alpha = 0.7,
      family = "Roboto-Bold",
      size = 3.5,
      color = "black"
    ) +

    # Facets
    facet_wrap(~theta, nrow = 1) +

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
      axis.ticks.x = element_blank(),
      axis.ticks.length.x = unit(0, "cm")
    )

  # Sub-figure B: Sensitivity of phosphorus percentage (%P) by parameter type
  p_plot = ggplot(elasticity_results) +
    # Scale color
    scale_color_manual(values = class_colors, name = "Size Class") +

    # Mapping
    aes(y = -P_elasticity * 100, color = class_affected) +

    # Background
    geom_vline(
      xintercept = c(0.25, 0.50, 0.75),
      col = "grey75",
      linewidth = 0.5,
      linetype = 2
    ) +
    geom_hline(yintercept = 0, col = "black", linewidth = 0.5) +

    # Plot lines
    stat_ecdf(geom = "step", pad = FALSE, linewidth = 1) +

    # Temperature labels in bottom-right corner of each facet
    geom_label(
      data = temp_labels,
      aes(label = label),
      x = Inf,
      y = -Inf,
      hjust = 1,
      vjust = 0,
      label.padding = unit(0.15, "lines"),
      label.size = 0,
      label.r = unit(0, "lines"),
      fill = "white",
      alpha = 0.7,
      family = "Roboto-Bold",
      size = 3.5,
      color = "black"
    ) +

    # Facets
    facet_wrap(~theta, nrow = 1) +

    # Labels
    labs(
      x = "Cumulative proportion of simulations (%)",
      y = "Change in populational P content (%)"
    ) +

    # Theme
    theme_custom() +
    theme(
      aspect.ratio = 1,
      strip.background = element_blank(),
      strip.text = element_blank(),
      legend.position = "bottom",
      plot.margin = margin(t = 0, b = 2, l = 2, r = 2),
      axis.ticks.length.x.top = unit(0, "cm")
    )

  # Scales
  if (analysis_type == "survival") {
    lambda_plot = lambda_plot +
      # Scale axis
      scale_x_continuous(
        breaks = seq(0, 1, by = 0.1),
        minor_breaks = seq(0, 1, by = 0.05),
        labels = scales::label_percent(suffix = ""),
        expand = c(0.01, 0.01)
      ) +
      scale_y_continuous(
        breaks = seq(-100, 100, by = 1),
        minor_breaks = seq(0, 1, by = 0.1),
        expand = c(0.01, 0.01)
      ) +
      coord_cartesian(xlim = c(0, 1), ylim = c(-10, 0), clip = "on")

    p_plot = p_plot +
      # Scale axis
      scale_x_continuous(
        breaks = seq(0, 1, by = 0.1),
        minor_breaks = seq(0, 1, by = 0.05),
        labels = scales::label_percent(suffix = ""),
        expand = c(0.01, 0.01)
      ) +
      scale_y_continuous(
        breaks = seq(-100, 100, by = 1),
        minor_breaks = seq(-100, 100, by = 0.5),
        expand = c(0.01, 0.01),
        transform = squeeze_trans
      ) +
      coord_cartesian(xlim = c(0, 1), ylim = c(-5, 6), clip = "on")
  } else if (analysis_type == "fecundity") {
    lambda_plot = lambda_plot +
      # Scale axis
      scale_x_continuous(
        breaks = seq(0, 1, by = 0.1),
        minor_breaks = seq(0, 1, by = 0.05),
        labels = scales::label_percent(suffix = ""),
        expand = c(0.01, 0.01)
      ) +
      scale_y_continuous(
        breaks = seq(-100, 100, by = 0.25),
        minor_breaks = seq(-100, 100, by = 0.125),
        expand = c(0.01, 0.01)
      ) +
      coord_cartesian(xlim = c(0, 1), ylim = c(-2.5, 0), clip = "on")

    p_plot = p_plot +
      # Scale axis
      scale_x_continuous(
        breaks = seq(0, 1, by = 0.1),
        minor_breaks = seq(0, 1, by = 0.05),
        labels = scales::label_percent(suffix = ""),
        expand = c(0.01, 0.01)
      ) +
      scale_y_continuous(
        breaks = seq(-100, 100, by = 0.025),
        minor_breaks = seq(-100, 100, by = 0.0125),
        expand = c(0.01, 0.01),
        transform = squeeze_trans
      ) +
      coord_cartesian(xlim = c(0, 1), ylim = c(-0.1, 0.15), clip = "on")
  } else if (analysis_type == "growth") {
    major_br = c(
      seq(-50, -10 - 1e-9, by = 10),
      seq(-10, -2 - 1e-9, by = 2),
      seq(-2, 2 - 1e-9, by = 0.5),
      seq(2, 10 - 1e-9, by = 2),
      seq(10, 50, by = 10)
    )
    minor_br = c(
      seq(-50, -10 - 1e-9, by = 1),
      seq(-10, -2 - 1e-9, by = 0.5),
      seq(-2, 2 - 1e-9, by = 0.25),
      seq(2, 10 - 1e-9, by = 0.5),
      seq(2, 50, by = 1)
    )

    lambda_plot = lambda_plot +
      # Scale axis
      scale_x_continuous(
        breaks = seq(0, 1, by = 0.1),
        minor_breaks = seq(0, 1, by = 0.05),
        labels = scales::label_percent(suffix = ""),
        expand = c(0.01, 0.01)
      ) +
      scale_y_continuous(
        breaks = major_br,
        minor_breaks = minor_br,
        expand = c(0.01, 0.01),
        transform = squeeze_trans
      ) +
      coord_cartesian(xlim = c(0, 1), ylim = c(-2, 50), clip = "on")

    p_plot = p_plot +
      # Scale axis
      scale_x_continuous(
        breaks = seq(0, 1, by = 0.1),
        minor_breaks = seq(0, 1, by = 0.05),
        labels = scales::label_percent(suffix = ""),
        expand = c(0.01, 0.01)
      ) +
      scale_y_continuous(
        breaks = seq(-100, 100, by = 0.5),
        minor_breaks = seq(-100, 100, by = 0.25),
        expand = c(0.01, 0.01),
        transform = squeeze_trans
      ) +
      coord_cartesian(xlim = c(0, 1), ylim = c(-4, 1), clip = "on")
  }

  # Assemble the plots
  layout = c(
    patchwork::area(t = 1, l = 1, b = 5, r = 2),
    patchwork::area(t = 6, l = 1, b = 10, r = 2)
  )
  final_plot <- (lambda_plot / p_plot) +
    patchwork::plot_annotation(
      tag_levels = 'A',
      tag_prefix = '(',
      tag_suffix = ')'
    ) +
    # Adjust layout
    patchwork::plot_layout(
      # heights = c(1, 1),    # Equal height for both plots
      design = layout,
      guides = "collect"
    ) &
    theme(legend.position = "bottom")

  return(final_plot)
}

#' Create J1 and A3 survival effect figure
#'
#' @description
#' Creates Figure 4 for the manuscript, displaying the relationship between
#' the asymptotic growth rate (lambda) and mean population phosphorus content
#' (%P) when varying the survival rate of the J1 (top row) or A3 (bottom row)
#' size class across its full range, at three temperatures (8, 12, and 16 C).
#' All other class-specific survival rates are held constant at their empirical
#' monthly average values. These two classes were selected based on their
#' dominant contribution to lambda sensitivity (Fig. 3).
#'
#' @param figure_data Data table containing single-parameter simulation results
#'   for J1 and A3 survival variations, with columns \code{theta},
#'   \code{class_var}, \code{surv_rate_J1}, \code{surv_rate_A3},
#'   \code{lambda}, and \code{mean_percentP}
#'
#' @return A ggplot object representing Figure 4
#' @export
#' @importFrom data.table as.data.table
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer
create_j1_a3_survival_effect <- function(figure_data) {
  # Prepare data
  figure_subdata <- figure_data[, .(
    theta,
    class_var,
    J1 = surv_rate_J1,
    A3 = surv_rate_A3,
    lambda,
    mean_percentP
  )] |>
    tidyr::pivot_longer(
      cols = c(J1, A3),
      names_to = "class",
      names_transform = as.factor,
      values_to = "surv_rate"
    ) |>
    dplyr::mutate(
      class = factor(class, levels = c("J1", "A3")),
      theta = as.factor(theta)
    ) |>
    data.table::as.data.table()

  # Temperature labels per facet (class_var x theta combination)
  temp_labels <- tidyr::expand_grid(
    class_var = unique(figure_subdata$class_var),
    theta = unique(figure_subdata$theta)
  ) |>
    dplyr::mutate(label = paste0(theta, "\u00b0C"))

  # Class labels for row annotation (left side of first column)
  class_labels <- data.frame(
    class_var = unique(figure_subdata$class_var),
    theta = min(as.numeric(as.character(unique(figure_subdata$theta)))),
    label = paste0(unique(figure_subdata$class_var), " class")
  ) |>
    dplyr::mutate(theta = as.factor(theta))

  # Create figure
  fig_4 <- ggplot() +

    # Scales
    scale_x_continuous(
      breaks = seq(0, 5, by = 0.2),
      minor_breaks = seq(0, 5, by = 0.1),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = seq(0, 2, by = 0.010),
      minor_breaks = seq(0, 2, by = 0.005),
      expand = c(0, 0)
    ) +
    coord_cartesian(
      xlim = c(0.3, 1.400001),
      ylim = c(0.98, 1.02000001),
      clip = "on"
    ) +

    # Color per class
    scale_color_manual(
      values = c(J1 = "#9ACD32", A3 = "#EE6AA7"),
      name = "Size class",
      guide = "none"
    ) +

    # Reference line at lambda = 1
    geom_vline(
      xintercept = 1,
      linewidth = 0.5,
      col = "darkgrey",
      linetype = "dashed"
    ) +

    # Survival variation curves
    geom_line(
      data = figure_subdata,
      aes(x = lambda, y = mean_percentP * 100, color = class_var),
      linewidth = 1,
      lineend = "round"
    ) +

    # Temperature label (bottom-right of each facet)
    geom_label(
      data = temp_labels,
      aes(label = label),
      x = Inf,
      y = -Inf,
      hjust = 1,
      vjust = 0,
      label.padding = unit(0.15, "lines"),
      label.size = 0,
      label.r = unit(0, "lines"),
      fill = "white",
      alpha = 0.7,
      family = "Roboto-Bold",
      size = 3.5,
      color = "black"
    ) +

    # Axis labels
    labs(
      x = "Asymptotic growth rate (\u03bb)",
      y = "Mean population P content (%)"
    ) +

    # Two rows: J1 (top) and A3 (bottom), three columns: temperatures
    facet_wrap(class_var ~ theta, nrow = 2) +

    # Tag facets (A1-A3 top row, B1-B3 bottom row)
    tagger::tag_facets(
      tag = "rc",
      position = "tl",
      tag_levels = c("A", "1"),
      tag_prefix = "(",
      tag_suffix = ")",
      tag_sep = ""
    ) +

    # Theme
    theme_custom() +
    theme(
      aspect.ratio = 1,
      strip.background = element_blank(),
      strip.text = element_blank(),
      panel.spacing.x = unit(0.7, "lines"),
      panel.spacing.y = unit(1.2, "lines"),
      legend.position = "none",
      plot.margin = margin(t = 5, b = 5, l = 5, r = 8)
    )

  return(fig_4)
}

#' Create survival gradient density figure
#'
#' @description
#' This function creates Figure 5 for the manuscript showing the density
#' distribution of the asymptotic population growth rate (lambda) and
#' population phosphorus content (%P) across survival rate categories for
#' the J1 and A3 size classes, at each simulated temperature. Survival rates
#' are binned into four quartile categories. The joint distribution of lambda
#' and %P is represented by overlapping 2D kernel density estimates for J1
#' (green) and A3 (pink), with isodensity contour lines, and marginal
#' density curves projected onto each axis. Facets are arranged by temperature
#' (rows) and survival category (columns).
#'
#' @param multi_param_results A data.table produced by the multi-parameter
#'   simulation, containing at minimum the columns \code{theta},
#'   \code{surv_rate_J1}, \code{surv_rate_A3}, \code{lambda}, and
#'   \code{mean_percentP}.
#' @param monthly_results A data.table of monthly simulation results,
#'   containing at minimum the columns \code{lambda} and \code{mean_percentP}.
#'   Currently unused in this version of the figure but retained for
#'   interface consistency with the pipeline.
#'
#' @return A ggplot object for Figure 5.
#'
#' @export
#' @importFrom data.table as.data.table
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer expand_grid
#' @importFrom ggnewscale new_scale_fill new_scale_colour
create_survival_gradient_density_figure <- function(
  multi_param_results,
  monthly_results
) {
  # Create subdata
  simulations_data = multi_param_results[, .(
    theta,
    J1 = surv_rate_J1,
    A3 = surv_rate_A3,
    lambda,
    mean_percentP
  )] |>
    tidyr::pivot_longer(
      cols = c(J1, A3),
      names_to = "class",
      names_transform = as.factor,
      values_to = "surv_rate"
    ) |>
    dplyr::mutate(
      class = factor(class, levels = c("J1", "A3")),
      theta = as.factor(theta)
    ) |>
    data.table::as.data.table()

  # Categorize survival rates into quartile bins
  plot_data = simulations_data
  plot_data[,
    category := factor(
      ifelse(
        surv_rate <= 0.25,
        "s \u2264 0.25",
        ifelse(
          surv_rate <= 0.50,
          "0.25 < s \u2264 0.50",
          ifelse(
            surv_rate <= 0.75,
            "0.50 < s \u2264 0.75",
            "0.75 < s \u2264 1.00"
          )
        )
      ),
      levels = c(
        "s \u2264 0.25",
        "0.25 < s \u2264 0.50",
        "0.50 < s \u2264 0.75",
        "0.75 < s \u2264 1.00"
      )
    )
  ]

  # Facet labels combining survival category and temperature
  temp_labels = tidyr::expand_grid(
    category = unique(plot_data$category),
    theta = unique(plot_data$theta)
  ) |>
    as.data.frame() |>
    dplyr::mutate(label = paste0(category, " - ", theta, "\u00b0C"))

  # Create figure
  fig_4 = ggplot() +
    # Scales
    scale_x_continuous(
      breaks = seq(0, 5, 0.2),
      minor_breaks = seq(0, 5, 0.1),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = seq(0, 2, 0.01),
      minor_breaks = seq(0, 2, 0.005),
      expand = c(0, 0)
    ) +
    coord_cartesian(xlim = c(0, 1.5), ylim = c(0.95, 1.05)) +

    # Background line for lambda = 1
    geom_vline(xintercept = 1, linewidth = 0.5, col = "darkgrey") +

    # A3 density raster layer
    ggplot2::stat_density_2d(
      data = simulations_data[class == "A3"],
      aes(
        x = lambda,
        y = mean_percentP * 100,
        fill = after_stat(ndensity)
      ),
      geom = "raster",
      contour = FALSE
    ) +
    ggplot2::scale_fill_gradientn(
      colours = c("#ff007700", "#ff0077b0"),
      name = "A3",
      limits = c(0, 1),
      guide = guide_colorbar(
        title.position = "left",
        title.vjust = 0.9,
        barwidth = unit(100, "points"),
        barheight = unit(10, "points")
      )
    ) +

    # J1 density raster layer (new fill scale)
    ggnewscale::new_scale_fill() +
    ggplot2::stat_density_2d(
      data = simulations_data[class == "J1"],
      aes(
        x = lambda,
        y = mean_percentP * 100,
        fill = after_stat(ndensity)
      ),
      geom = "raster",
      contour = FALSE
    ) +
    ggplot2::scale_fill_gradientn(
      colours = c("#86c90000", "#86c900b0"),
      name = "Density for: J1",
      limits = c(0, 1),
      guide = guide_colorbar(
        title.position = "left",
        title.vjust = 0.9,
        barwidth = unit(100, "points"),
        barheight = unit(10, "points")
      )
    ) +

    # A3 isodensity contour lines (new fill and colour scales)
    ggnewscale::new_scale_fill() +
    ggnewscale::new_scale_colour() +
    ggplot2::geom_density_2d(
      data = simulations_data[class == "A3"],
      aes(
        x = lambda,
        y = mean_percentP * 100,
        color = after_stat(level)
      ),
      contour_var = "ndensity",
      breaks = c(0.1, 0.25, 0.5, 0.75, 0.9),
      linewidth = 0.5
    ) +
    ggplot2::scale_color_gradientn(
      colours = c("#ff007720", "#ff0077e0"),
      name = "A3",
      limits = c(0, 1),
      guide = "none"
    ) +

    # J1 isodensity contour lines (new colour scale)
    ggnewscale::new_scale_colour() +
    ggplot2::scale_color_gradientn(
      colours = c("#86c90020", "#86c900e0"),
      name = "J1",
      limits = c(0, 1),
      guide = "none"
    ) +
    ggplot2::geom_density_2d(
      data = simulations_data[class == "J1"],
      aes(
        x = lambda,
        y = mean_percentP * 100,
        color = after_stat(level)
      ),
      contour_var = "ndensity",
      breaks = c(0.1, 0.25, 0.5, 0.75, 0.9),
      linewidth = 0.5
    ) +

    # Facet labels in upper-right corner
    geom_label(
      data = temp_labels,
      aes(label = label),
      x = Inf,
      y = Inf,
      hjust = "inward",
      vjust = "inward",
      label.padding = unit(0.15, "lines"),
      label.size = 0,
      label.r = unit(0, "lines"),
      fill = "white",
      alpha = 0.7,
      family = "Roboto-Bold",
      size = 3.5,
      color = "black"
    ) +

    # Marginal density curves: lambda (x-axis projection)
    ggnewscale::new_scale_colour() +
    scale_color_manual(
      values = c(J1 = "#86c900e0", A3 = "#ff0077e0"),
      guide = "none"
    ) +
    ggplot2::geom_density(
      data = simulations_data,
      aes(
        x = lambda,
        y = after_stat(ndensity) * 0.01 + 0.95,
        color = class
      ),
      fill = NA,
      adjust = 1
    ) +

    # Marginal density curves: %P (y-axis projection)
    ggplot2::geom_density(
      data = simulations_data,
      aes(
        x = after_stat(ndensity) * 0.1,
        y = mean_percentP * 100,
        color = class
      ),
      fill = NA,
      adjust = 1
    ) +

    # Facets: rows = temperature, columns = survival category
    facet_wrap(theta ~ category, nrow = 3) +

    # Labels
    labs(
      x = "Asymptotic Growth Rate (\u03bb)",
      y = "Populational P content (%)"
    ) +

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

#' Create supplementary figure S1 for sex differences in phosphorus content
#'
#' @description
#' Creates Figure S1 for the manuscript, displaying individual phosphorus
#' content (% of dry mass) as a function of sex (male, female, ovigerous
#' female). Individual measurements are shown as jittered points, with
#' mean +/- standard error and significance letters from post-hoc tests
#' overlaid.
#'
#' @param phosphorus_data A data table of processed phosphorus data containing
#'   columns \code{sex} and \code{P_percent}
#' @param stats_info A data table of statistical results containing columns
#'   \code{trt} (sex group), \code{mean}, \code{se}, \code{max}, and
#'   \code{signif_letter} (compact letter display from post-hoc comparison)
#'
#' @return A ggplot object representing Figure S1
#'
#' @export
create_phosphorus_sex_difference_figure <- function(
  phosphorus_data,
  stats_info
) {
  # Create figure
  p_plot <- ggplot(phosphorus_data) +
    # Formatting
    scale_y_continuous(
      breaks = seq(0, 100, by = 0.10),
      minor_breaks = seq(0, 100, by = 0.05),
      expand = c(0, 0)
    ) +
    coord_cartesian(ylim = c(0, 1.20001), clip = "on") +

    ggplot2::scale_color_manual(
      values = c(
        M = "#5697ec",
        F = "#f1536d",
        F.ovi = "#ca8dee"
      ),
      name = "sex",
      guide = "none"
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        M = "#5697ec",
        F = "#f1536d",
        F.ovi = "#ca8dee"
      ),
      name = "sex",
      guide = "none"
    ) +

    # Data mapping
    aes(x = sex, y = P_percent * 100, col = sex) +

    # Points with outlier highlighted
    ggplot2::geom_jitter(
      aes(fill = sex),
      shape = 21,
      size = 1,
      alpha = 0.4,
      width = 0.05
    ) +

    # Mean marker (star symbol)
    # stat_summary(fun = "mean", col = "red", size = 2, shape = 13, geom = "point") +
    geom_point(
      data = stats_info,
      aes(x = as.factor(trt), y = mean * 100, col = trt),
      shape = 13,
      size = 2
    ) +
    ggplot2::geom_errorbar(
      data = stats_info,
      aes(
        x = as.factor(trt),
        y = NULL,
        ymin = (mean - se) * 100,
        ymax = (mean + se) * 100,
        col = trt
      ),
      linewidth = 0.5,
      width = 0.2
    ) +

    # Significance letters
    geom_text(
      data = stats_info,
      aes(y = max * 100 + 0.05, label = signif_letter, x = as.factor(trt)),
      fontface = "bold",
      col = "black",
      family = "Roboto"
    ) +

    # Labels
    labs(x = "Sex", y = "Individual P content (%)") +

    # Theme
    theme_custom()

  return(p_plot)
}

#' Create a plot of significant transition rates between size classes
#'
#' @description
#' This function creates Figure S2 showing significant transition rates between size classes
#' as a function of temperature.
#'
#' @param transition_data Data frame containing transition rates (output from calculate_transition_rates)
#' @param significance_threshold Minimum transition rate to be considered significant (default: 0.01)
#' @param reference_temps Vector of reference temperatures to highlight with vertical lines
#' @param L_max Maximum size in mm
#' @param delta_t Time step in days
#'
#' @return A ggplot object with the transition rates visualization for Figure S2
#'
#' @importFrom ggplot2 ggplot aes geom_vline geom_line scale_x_continuous scale_y_continuous coord_cartesian labs facet_grid
#' @importFrom dplyr group_by summarise filter inner_join
#'
#' @export
#' @importFrom dplyr filter summarise group_by inner_join
create_transition_rates_plot <- function(
  transition_data,
  significance_threshold = 0.01,
  reference_temps = c(8, 12, 16),
  L_max,
  delta_t
) {
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
    scale_x_continuous(
      breaks = seq(0, 25, by = 5),
      minor_breaks = seq(0, 25, by = 2.5),
      expand = c(0.01, 0.01)
    ) +
    scale_y_continuous(
      breaks = seq(0, 1, by = 0.25),
      minor_breaks = seq(0, 1, by = 0.125),
      expand = c(0.01, 0.01)
    ) +
    coord_cartesian(xlim = c(0, 25), ylim = c(0, 1), clip = "on") +

    # Elements
    aes(x = theta, y = Z) +
    geom_vline(
      xintercept = reference_temps,
      col = "#A0A0A0",
      linetype = "dashed"
    ) +
    geom_line(linewidth = 1, color = "#0070C0") +

    # Labels
    labs(
      x = "Temperature (\u00b0C)",
      y = "Transition rate",
      # title = "Significant transition rates between size classes",
      # subtitle = paste("Time step: ", delta_t, " days", sep = "")
    ) +

    # Theme and facets
    theme_custom() +
    facet_grid(vars(X), vars(Y)) +
    theme(aspect.ratio = 1, panel.spacing = unit(x = 0.5, units = "lines"))

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
      "growth_rate_coef",
      "growth_rate_intercept",
      "L_max",
      "molt_cycle_a",
      "molt_cycle_b",
      "molt_cycle_c",
      "molt_cycle_d",
      "sexratio",
      "gravid",
      "fertil_A1",
      "fertil_A2",
      "fertil_A3"
    ),
    category = c(
      rep("Growth", 3),
      rep("Molt Cycle", 4),
      rep("Reproduction", 2),
      rep("Fertility", 3)
    ),
    display_name = c(
      "Growth rate: Temperature coef.",
      "Growth rate: Intercept",
      "Maximum size (L_max)",
      "Molt cycle: Parameter a",
      "Molt cycle: Parameter b",
      "Molt cycle: Parameter c",
      "Molt cycle: Parameter d",
      "Sex ratio",
      "Proportion gravid",
      "Fertility: A1",
      "Fertility: A2",
      "Fertility: A3"
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

  for (i in 1:length(categories_levels)) {
    # Select sub-data
    data = results_with_categories[category == categories_levels[i]]

    # Sub-figure A: Sensitivity of asymptotic growth rate (\u03bb) by parameter type
    lambda_plot = ggplot(data) +
      # Scales
      scale_x_continuous(
        breaks = seq(0, 1, by = 0.1),
        minor_breaks = seq(0, 1, by = 0.05),
        labels = scales::label_percent(suffix = ""),
        expand = c(0.01, 0.01)
      ) +
      scale_color_manual(values = param_colors, name = "Parameter") +

      # Background
      geom_vline(
        xintercept = c(0.25, 0.50, 0.75),
        col = "grey75",
        linewidth = 0.5,
        linetype = 2
      ) +
      geom_hline(yintercept = 0, col = "black", linewidth = 0.5) +

      # Plot lines
      stat_ecdf(
        aes(y = -lambda_elasticity * 100, color = display_name),
        geom = "line",
        pad = FALSE,
        linewidth = 1
      ) +

      # Temperature labels in bottom-right corner of each facet
      geom_label(
        data = temp_labels,
        aes(label = label),
        x = Inf,
        y = -Inf,
        hjust = 1,
        vjust = 0,
        label.padding = unit(0.15, "lines"),
        label.size = 0,
        label.r = unit(0, "lines"),
        fill = "white",
        alpha = 0.7,
        family = "Roboto-Bold",
        size = 3.5,
        color = "black"
      ) +

      # Facets
      facet_wrap(~theta, ncol = 3) +

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
      scale_x_continuous(
        breaks = seq(0, 1, by = 0.1),
        minor_breaks = seq(0, 1, by = 0.05),
        labels = scales::label_percent(suffix = ""),
        expand = c(0.01, 0.01)
      ) +
      scale_color_manual(values = param_colors, name = "Parameter") +

      # Mapping
      aes(y = -P_elasticity * 100, color = display_name) +

      # Background
      geom_vline(
        xintercept = c(0.25, 0.50, 0.75),
        col = "grey75",
        linewidth = 0.5,
        linetype = 2
      ) +
      geom_hline(yintercept = 0, col = "black", linewidth = 0.5) +

      # Plot lines
      stat_ecdf(geom = "line", pad = FALSE, linewidth = 1) +

      # Temperature labels in bottom-right corner of each facet
      geom_label(
        data = temp_labels,
        aes(label = label),
        x = Inf,
        y = -Inf,
        hjust = 1,
        vjust = 0,
        label.padding = unit(0.15, "lines"),
        label.size = 0,
        label.r = unit(0, "lines"),
        fill = "white",
        alpha = 0.7,
        family = "Roboto-Bold",
        size = 3.5,
        color = "black"
      ) +

      # Facets
      facet_wrap(~theta) +

      # Labels
      labs(
        x = "Cumulative proportion of simulations (%)",
        y = "Change in populational P content (%)"
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
      patchwork::plot_annotation(
        tag_levels = 'A',
        tag_prefix = '(',
        tag_suffix = ')'
      ) +
      # Adjust layout
      patchwork::plot_layout(
        heights = c(1, 1), # Equal height for both plots
        guides = "collect"
      ) &
      theme(legend.position = "bottom")

    # Figures saving
    figures[[categories_levels[i]]] = final_plot
  }

  return(figures)
}
