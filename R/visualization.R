###############################################################################
# visualization.R
# Visualization utilities for the Gammarus-Phosphorum project
# Author: Titouan Dionet
# Date: April 2025
###############################################################################

#' Set up fonts for the project
#' @description Set up fonts for the project
#'
#' @return TRUE if fonts are correctly loaded
#' 
#' @import here tools sysfonts showtext
#' @export
setup_fonts <- function() {
  # Fonts directory
  font_dir <- here::here("fonts", "roboto")
  
  # List .ttf files
  ttf_files <- list.files(font_dir, pattern = "\\.ttf$", full.names = TRUE)
  
  # Add each font
  for (ttf_file in ttf_files) {
    font_name <- tools::file_path_sans_ext(basename(ttf_file))
    sysfonts::font_add(family = font_name, regular = ttf_file)
  }
  
  # Activate showtext
  showtext::showtext_auto(enable = TRUE)
  
  return(TRUE)
}

#' Custom theme for ggplot2
#' 
#' @description 
#' This function creates a custom theme for ggplot2 based on theme_bw() with pre-defined 
#' styling for various plot elements.
#' 
#' @return A ggplot2 theme object with custom styling.
#'
#' @details
#' The function customizes multiple visual aspects of a ggplot2 plot, including:
#' - Text font family (Roboto)
#' - Colors and font faces for titles, axis labels, and legends
#' - Spacing between panels and margins
#' - Background colors and styling for strips in faceted plots
#'
#' This creates a consistent visual style across all plots in your project.
#' 
#' @examples
#' \dontrun{
#' library(ggplot2)
#' 
#' # Create a plot with custom theme
#' ggplot(data = gammarus_data, aes(x = Classe, y = percentP*100)) +
#'   geom_boxplot() +
#'   labs(x = "Size class", y = "Phosphorus (%)") +
#'   theme_custom()
#' }
#' 
#' @importFrom ggplot2 theme_bw theme element_text element_rect unit
#' 
#' @export
theme_custom = function() {
  base_theme = ggplot2::theme_bw()
  
  # Global
  base_theme$text$family = "Roboto-Medium"
  
  # Panel
  base_theme$panel.spacing = unit(0.5, "lines")
  base_theme$panel.background$fill = "white"
  
  # Plot
  # base_theme$plot.title$colour = "#0070C0"
  base_theme$plot.title$face = "bold"
  # base_theme$plot.subtitle$colour = "#0070C0"
  base_theme$plot.subtitle$face = "italic"
  base_theme$plot.margin = unit(c(1,1,1,1), "lines")
  base_theme$plot.background$fill = "transparent"
  base_theme$plot.background$colour = NA
  
  # Axis
  # base_theme$axis.title.x$colour = "#0070C0"
  base_theme$axis.title.x$face = "bold"
  # base_theme$axis.title.y$colour = "#0070C0"
  base_theme$axis.title.y$face = "bold"
  
  # Legend
  # base_theme$legend.title$colour = "#0070C0"
  base_theme$legend.title$face = "bold"
  base_theme$legend.background$fill = "transparent"
  base_theme$legend.background$colour = NA
  # base_theme$legend.text$colour = "#0070C0"
  base_theme$legend.text$face = "plain"
  
  # Strip
  # base_theme$strip.text$colour = "#0070C0"
  base_theme$strip.text$face = "bold"
  base_theme$strip.text$size = 10
  base_theme$strip.background$colour = "black"
  base_theme$strip.background$fill = "white"
  base_theme$strip.background$linewidth = 0.5
  
  return(base_theme)
}

#' Save a figure in multiple formats
#'
#' @description 
#' Saves a ggplot object as both PNG and SVG files with the specified dimensions
#'
#' @param plot The ggplot object to save
#' @param basename Base filename without extension
#' @param dir Directory where the files should be saved
#' @param width Width of the figure (in the specified units)
#' @param height Height of the figure (in the specified units)
#' @param units Units for width and height ('in', 'cm', 'mm', or 'px')
#' @param dpi Resolution in dots per inch
#' @param formats Vector of formats to save (default: c("png", "svg"))
#'
#' @return Path to the last saved file
#' @export
save_figure <- function(plot, basename, dir, width, height, units = "in", dpi = 300, 
                        formats = c("png", "svg")) {
  # Ensure the directory exists
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  
  # Save in each format
  last_path <- NULL
  
  for (fmt in formats) {
    filename <- paste0(basename, ".", fmt)
    file_path <- file.path(dir, filename)
    
    ggsave(
      filename = file_path,
      plot = plot,
      width = width,
      height = height,
      dpi = dpi,
      units = units
    )
    
    last_path <- file_path
    message(sprintf("Saved figure to %s (size: %s x %s %s, dpi: %s)", 
                    file_path, width, height, units, dpi))
  }
  
  return(last_path)
}