#' @keywords internal
"_PACKAGE"

# Imports: start ---- 

#' @importFrom stats TukeyHSD aov bartlett.test coef fligner.test kruskal.test 
#'   lm median oneway.test quantile residuals runif sd shapiro.test
#' @importFrom ggplot2 ggplot aes aes_string geom_point geom_text geom_line 
#'   geom_vline stat_ecdf stat_summary facet_wrap facet_grid scale_color_manual 
#'   scale_shape_manual scale_x_continuous scale_y_continuous coord_cartesian 
#'   scale_color_viridis_c labs labeller vars theme element_text element_rect 
#'   unit ggsave geom_hline geom_label element_blank margin guide_colorbar
#' @importFrom data.table data.table copy as.data.table setnames .SD .N .I := 
#' @importFrom gridExtra grid.arrange
#' @importFrom grid textGrob gpar
#' @importFrom utils head tail
#' @importFrom patchwork plot_annotation plot_layout
#' @importFrom stats setNames 

# Imports: end ----

utils::globalVariables(c(
  "P_percent", "lambda", "lambda_elasticity", "P_elasticity", 
  "class_affected", "mean_percentP", "class_var", "theta", 
  "signif_letter", "trt", "id", "absorbance", "dilution_factor",
  "P_conc_microg_ml", "P_mass_microg", "sample_volume_ml", 
  "dry_sample_mass_microg", "label", "surv_rate_J1", "surv_rate_A3", 
  "surv_rate", "J1", "A3", "category", "display_name", "X", "Y", "Z", 
  "max_value", "parameter_type", "."
))

NULL
