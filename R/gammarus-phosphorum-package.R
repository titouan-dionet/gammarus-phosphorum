#' @keywords internal
"_PACKAGE"

# Imports: start ---- 

#' @importFrom stats TukeyHSD aov bartlett.test coef fligner.test kruskal.test 
#'   lm median oneway.test quantile residuals runif sd shapiro.test
#' @importFrom ggplot2 ggplot aes aes_string geom_point geom_text geom_line 
#'   geom_vline stat_ecdf stat_summary facet_wrap facet_grid scale_color_manual 
#'   scale_shape_manual scale_x_continuous scale_y_continuous coord_cartesian 
#'   scale_color_viridis_c labs labeller vars theme element_text element_rect 
#'   unit ggsave
#' @importFrom data.table data.table copy as.data.table setnames .SD .N .I :=
#' @importFrom gridExtra grid.arrange
#' @importFrom grid textGrob gpar
#' @importFrom utils head tail

# Imports: end ----

utils::globalVariables(c(
  "P_percent", "lambda", "lambda_elasticity", "P_elasticity", 
  "class_affected", "mean_percentP", "class_var", "theta", 
  "signif_letter", "trt", "id", "absorbance", "dilution_factor",
  "P_conc_microg_ml", "P_mass_microg", "sample_volume_ml", 
  "dry_sample_mass_microg"
))

NULL
