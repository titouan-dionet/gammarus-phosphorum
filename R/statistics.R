###############################################################################
# STATISTICS FUNCTIONS
# Description: Functions for statistical analysis and group comparison
# Author: Titouan Dionet
# Date: April 2025
###############################################################################

###############################################################################
# STATISTICS FUNCTIONS
###############################################################################

#' Automatic Statistical Group Comparison
#'
#' @description Performs a comprehensive set of statistical tests on input data,
#' automatically selecting appropriate tests based on data characteristics.
#'
#' @param data A data frame containing the data to analyze
#' @param val Values to test (numeric vector)
#' @param trt Treatments/groups to compare (factor or vector to be converted to factor)
#' @param paired Logical indicating if data are paired (default: FALSE)
#' @param alpha Significance level (default: 0.05)
#' @param detailed Logical - if TRUE, returns additional test details (default: FALSE)
#'
#' @return A list containing:
#'   \item{p}{List of p-values from different tests}
#'   \item{info}{Data frame with summary statistics and significance letters}
#'   \item{tests}{Details about which tests were performed and why (if detailed=TRUE)}
#'   \item{diagnostics}{Data frame with diagnostic test results (if detailed=TRUE)}
#'   \item{model}{The final model used for post-hoc comparisons (if detailed=TRUE)}
#'
#' @examples
#' # Create example data
#' set.seed(123)
#' example_data <- data.frame(
#'   value = c(rnorm(10, 1, 0.2), rnorm(10, 1.5, 0.3), rnorm(10, 1.3, 0.25)),
#'   group = rep(c("A", "B", "C"), each = 10)
#' )
#' # Perform automatic test
#' result <- auto_test_groups(example_data, example_data$value, example_data$group)
#'
#' @importFrom car leveneTest
#'
#' @export
auto_test_groups <- function(
  data,
  val,
  trt,
  paired = FALSE,
  alpha = 0.05,
  detailed = FALSE
) {
  # Convert trt to factor if it's not already
  if (!is.factor(trt)) {
    trt <- as.factor(trt)
  }

  # Initialize results list
  results <- list()

  # Number of groups and observations
  n_groups <- length(unique(trt))
  n_obs <- length(val)

  # Check if we have enough data points for analysis
  if (n_groups < 2) {
    stop("At least two groups are required for comparison")
  }

  if (n_obs < 3) {
    stop("At least three observations are required for analysis")
  }

  # Run descriptive statistics
  group_stats <- data.frame(
    trt = levels(trt),
    n = as.numeric(table(trt)),
    mean = tapply(val, trt, mean, na.rm = TRUE),
    median = tapply(val, trt, median, na.rm = TRUE),
    sd = tapply(val, trt, sd, na.rm = TRUE),
    se = tapply(val, trt, function(x) {
      sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
    }),
    min = tapply(val, trt, min, na.rm = TRUE),
    max = tapply(val, trt, max, na.rm = TRUE),
    cv = tapply(val, trt, function(x) {
      sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
    }),
    q25 = tapply(val, trt, function(x) quantile(x, 0.25, na.rm = TRUE)),
    q75 = tapply(val, trt, function(x) quantile(x, 0.75, na.rm = TRUE))
  )

  # Initialize tests and diagnostics tracking
  performed_tests <- list()
  diagnostics <- data.frame(
    test = character(),
    statistic = numeric(),
    p_value = numeric(),
    conclusion = character(),
    stringsAsFactors = FALSE
  )

  # -------------------------------------------------------
  # 1. Normality Tests
  # -------------------------------------------------------

  # Initial ANOVA as a starting point
  model_anova <- aov(val ~ trt)

  # Shapiro-Wilk test on residuals (global normality)
  sw_test <- shapiro.test(residuals(model_anova))
  sw_p <- sw_test$p.value
  sw_conclusion <- ifelse(sw_p > alpha, "Normal", "Non-normal")

  diagnostics <- rbind(
    diagnostics,
    data.frame(
      test = "Shapiro-Wilk (residuals)",
      statistic = sw_test$statistic,
      p_value = sw_p,
      conclusion = sw_conclusion
    )
  )

  # Per-group normality tests - FIXED VERSION
  group_norm_stats <- list()
  group_norm_pvals <- list()
  all_groups_normal <- TRUE

  # For each unique group
  for (g in levels(trt)) {
    group_data <- val[trt == g]
    if (length(group_data) >= 3 && length(group_data) <= 5000) {
      # Run Shapiro-Wilk test
      norm_test <- shapiro.test(group_data)
      group_norm_stats[[g]] <- norm_test$statistic
      group_norm_pvals[[g]] <- norm_test$p.value

      # Check if this group is normal
      if (norm_test$p.value <= alpha) {
        all_groups_normal <- FALSE
      }
    }
  }

  # Create normality dataframe
  group_normality_df <- data.frame(
    group = names(group_norm_stats),
    statistic = unlist(group_norm_stats),
    p_value = unlist(group_norm_pvals),
    conclusion = ifelse(
      unlist(group_norm_pvals) > alpha,
      "Normal",
      "Non-normal"
    )
  )

  # Add to diagnostics
  diagnostics <- rbind(
    diagnostics,
    data.frame(
      test = "Group-wise Shapiro-Wilk",
      statistic = NA,
      p_value = NA,
      conclusion = ifelse(
        all_groups_normal,
        "All groups normal",
        "Some groups non-normal"
      )
    )
  )

  # -------------------------------------------------------
  # 2. Homogeneity of Variance Tests
  # -------------------------------------------------------

  # Bartlett test (assumes normality)
  bart_test <- bartlett.test(val ~ trt)
  bart_p <- bart_test$p.value
  bart_conclusion <- ifelse(
    bart_p > alpha,
    "Equal variances",
    "Unequal variances"
  )

  diagnostics <- rbind(
    diagnostics,
    data.frame(
      test = "Bartlett",
      statistic = bart_test$statistic,
      p_value = bart_p,
      conclusion = bart_conclusion
    )
  )

  # Levene test (more robust to non-normality)
  levene_test <- car::leveneTest(val ~ trt)
  levene_p <- levene_test[1, "Pr(>F)"]
  levene_conclusion <- ifelse(
    levene_p > alpha,
    "Equal variances",
    "Unequal variances"
  )

  diagnostics <- rbind(
    diagnostics,
    data.frame(
      test = "Levene",
      statistic = levene_test[1, "F value"],
      p_value = levene_p,
      conclusion = levene_conclusion
    )
  )

  # Fligner-Killeen (non-parametric, very robust)
  fligner_test <- fligner.test(val ~ trt)
  fligner_p <- fligner_test$p.value
  fligner_conclusion <- ifelse(
    fligner_p > alpha,
    "Equal variances",
    "Unequal variances"
  )

  diagnostics <- rbind(
    diagnostics,
    data.frame(
      test = "Fligner-Killeen",
      statistic = fligner_test$statistic,
      p_value = fligner_p,
      conclusion = fligner_conclusion
    )
  )

  # Are variances homogeneous?
  # Consider both Levene and Fligner as they're more robust
  equal_variance <- (levene_p > alpha) && (fligner_p > alpha)

  # -------------------------------------------------------
  # 3. Determine the appropriate analysis path based on diagnostics
  # -------------------------------------------------------

  p_values <- list()
  model_used <- NULL
  test_type <- ""

  # If data are normal and have equal variances: parametric tests
  if ((sw_p > alpha || all_groups_normal) && equal_variance) {
    test_type <- "parametric"
    performed_tests[["main"]] <- "ANOVA"

    # One-way ANOVA
    anova_result <- summary(model_anova)
    anova_p <- anova_result[[1]][["Pr(>F)"]][1]
    p_values[["anova"]] <- anova_p

    diagnostics <- rbind(
      diagnostics,
      data.frame(
        test = "One-way ANOVA",
        statistic = anova_result[[1]][["F value"]][1],
        p_value = anova_p,
        conclusion = ifelse(
          anova_p < alpha,
          "At least one group differs",
          "No significant differences"
        )
      )
    )

    # If significant, do post-hoc test
    if (anova_p < alpha) {
      tukey_test <- TukeyHSD(model_anova)
      p_values[["tukey"]] <- tukey_test$trt[, "p adj"]
      performed_tests[["post_hoc"]] <- "Tukey HSD"

      # Get significance letters
      hsd_result <- agricolae::HSD.test(model_anova, "trt", group = TRUE)
      letters_df <- data.frame(
        trt = rownames(hsd_result$groups),
        signif_letter = hsd_result$groups$groups
      )

      model_used <- model_anova
    }
  } else if ((sw_p > alpha || all_groups_normal) && !equal_variance) {
    # If data are normal but variances unequal: Welch's ANOVA
    test_type <- "welch"
    performed_tests[["main"]] <- "Welch's ANOVA"

    # Welch's ANOVA
    welch_result <- oneway.test(val ~ trt, var.equal = FALSE)
    welch_p <- welch_result$p.value
    p_values[["welch"]] <- welch_p

    diagnostics <- rbind(
      diagnostics,
      data.frame(
        test = "Welch's ANOVA",
        statistic = welch_result$statistic,
        p_value = welch_p,
        conclusion = ifelse(
          welch_p < alpha,
          "At least one group differs",
          "No significant differences"
        )
      )
    )

    # If significant, do Games-Howell post-hoc test
    if (welch_p < alpha) {
      # Using Box-Cox transformation to stabilize variances for post-hoc
      bc_transform <- MASS::boxcox(lm(val + abs(min(val)) + 1 ~ trt))
      lambda <- bc_transform$x[which.max(bc_transform$y)]

      if (abs(lambda) < 0.001) {
        # Log transformation if lambda near zero
        transformed_val <- log(val + abs(min(val)) + 1)
      } else {
        # Box-Cox transformation
        transformed_val <- ((val + abs(min(val)) + 1)^lambda - 1) / lambda
      }

      model_bc <- aov(transformed_val ~ trt)
      performed_tests[["transform"]] <- paste(
        "Box-Cox (lambda =",
        round(lambda, 3),
        ")"
      )

      # Now perform post-hoc on transformed data
      hsd_result <- agricolae::HSD.test(model_bc, "trt", group = TRUE)
      letters_df <- data.frame(
        trt = rownames(hsd_result$groups),
        signif_letter = hsd_result$groups$groups
      )

      model_used <- model_bc
    }
  } else {
    # If data are non-normal: non-parametric tests
    test_type <- "nonparametric"
    performed_tests[["main"]] <- "Kruskal-Wallis"

    # Kruskal-Wallis test
    kw_result <- kruskal.test(val ~ trt)
    kw_p <- kw_result$p.value
    p_values[["kruskal"]] <- kw_p

    diagnostics <- rbind(
      diagnostics,
      data.frame(
        test = "Kruskal-Wallis",
        statistic = kw_result$statistic,
        p_value = kw_p,
        conclusion = ifelse(
          kw_p < alpha,
          "At least one group differs",
          "No significant differences"
        )
      )
    )

    # If significant, do non-parametric post-hoc
    if (kw_p < alpha) {
      # Using Dunn's test with Bonferroni correction
      dunn_test <- agricolae::kruskal(
        y = val,
        trt = trt,
        p.adj = "bonferroni",
        group = TRUE
      )
      performed_tests[["post_hoc"]] <- "Dunn's test with Bonferroni correction"

      letters_df <- data.frame(
        trt = rownames(dunn_test$groups),
        signif_letter = dunn_test$groups$groups
      )

      model_used <- NA # No actual model for Kruskal-Wallis
      p_values[["dunn"]] <- dunn_test$comparison$pvalue
    }
  }

  # If no significant differences, set all to same letter
  if (!exists("letters_df")) {
    letters_df <- data.frame(
      trt = levels(as.factor(trt)),
      signif_letter = rep("a", length(levels(as.factor(trt))))
    )
  }

  # -------------------------------------------------------
  # 4. Create final output
  # -------------------------------------------------------

  # Merge descriptive stats with letters
  info <- merge(group_stats, letters_df, by = "trt")

  # Add test type information
  results$p <- p_values
  results$test_type <- test_type
  results$performed_tests <- performed_tests
  results$diagnostics <- diagnostics
  results$info <- info
  results$model <- model_used

  # Return only what's required based on detailed flag
  if (!detailed) {
    return(list(p = p_values, info = info))
  } else {
    return(results)
  }
}

#' Compute percentile distributions of lambda and population P content for Table S1
#'
#' @description
#' Computes summary statistics (min, P10, P25, P50, P75, P90, max) of the
#' asymptotic population growth rate (lambda) and population phosphorus content
#' (%P) across survival rate categories (quartile bins of J1 and A3 survival
#' rates) at each simulated temperature. Results are returned in a wide format
#' suitable for direct export as supplementary Table S1.
#'
#' @param multi_param_results A data.table produced by the multi-parameter
#'   simulation, containing at minimum the columns \code{theta},
#'   \code{surv_rate_J1}, \code{surv_rate_A3}, \code{lambda}, and
#'   \code{mean_percentP}.
#'
#' @return A data.table in wide format with columns: Temp (°C), Class,
#'   Survival category, Parameter, Min, P10, P25, P50, P75, P90, Max.
#'   Lambda values are rounded to 3 decimal places; P content values are
#'   expressed as percentages and rounded to 3 decimal places.
#'
#' @export
compute_table_S1 <- function(multi_param_results) {
  data <- multi_param_results[, .(
    theta,
    surv_rate_J1,
    surv_rate_A3,
    lambda,
    mean_percentP
  )]

  # Categorize J1 survival rate into quartile bins
  data[,
    J1_category := factor(
      ifelse(
        surv_rate_J1 <= 0.25,
        "\u2264 0.25",
        ifelse(
          surv_rate_J1 <= 0.50,
          "\u2264 0.50",
          ifelse(surv_rate_J1 <= 0.75, "\u2264 0.75", "\u2264 1.00")
        )
      ),
      levels = c("\u2264 0.25", "\u2264 0.50", "\u2264 0.75", "\u2264 1.00")
    )
  ]

  # Categorize A3 survival rate into quartile bins
  data[,
    A3_category := factor(
      ifelse(
        surv_rate_A3 <= 0.25,
        "\u2264 0.25",
        ifelse(
          surv_rate_A3 <= 0.50,
          "\u2264 0.50",
          ifelse(surv_rate_A3 <= 0.75, "\u2264 0.75", "\u2264 1.00")
        )
      ),
      levels = c("\u2264 0.25", "\u2264 0.50", "\u2264 0.75", "\u2264 1.00")
    )
  ]

  # Compute percentiles for J1
  stats_J1 <- data |>
    dplyr::group_by(theta, J1_category) |>
    dplyr::summarise(
      lambda_Min = min(lambda),
      lambda_P10 = quantile(lambda, 0.10),
      lambda_P25 = quantile(lambda, 0.25),
      lambda_P50 = quantile(lambda, 0.50),
      lambda_P75 = quantile(lambda, 0.75),
      lambda_P90 = quantile(lambda, 0.90),
      lambda_Max = max(lambda),
      P_Min = min(mean_percentP),
      P_P10 = quantile(mean_percentP, 0.10),
      P_P25 = quantile(mean_percentP, 0.25),
      P_P50 = quantile(mean_percentP, 0.50),
      P_P75 = quantile(mean_percentP, 0.75),
      P_P90 = quantile(mean_percentP, 0.90),
      P_Max = max(mean_percentP),
      .groups = "drop"
    ) |>
    dplyr::mutate(Class = "J1")

  # Compute percentiles for A3
  stats_A3 <- data |>
    dplyr::group_by(theta, A3_category) |>
    dplyr::summarise(
      lambda_Min = min(lambda),
      lambda_P10 = quantile(lambda, 0.10),
      lambda_P25 = quantile(lambda, 0.25),
      lambda_P50 = quantile(lambda, 0.50),
      lambda_P75 = quantile(lambda, 0.75),
      lambda_P90 = quantile(lambda, 0.90),
      lambda_Max = max(lambda),
      P_Min = min(mean_percentP),
      P_P10 = quantile(mean_percentP, 0.10),
      P_P25 = quantile(mean_percentP, 0.25),
      P_P50 = quantile(mean_percentP, 0.50),
      P_P75 = quantile(mean_percentP, 0.75),
      P_P90 = quantile(mean_percentP, 0.90),
      P_Max = max(mean_percentP),
      .groups = "drop"
    ) |>
    dplyr::mutate(Class = "A3") |>
    dplyr::rename(J1_category = A3_category)

  # Combine, convert %P to percentage, round, and reshape
  table_long <- dplyr::bind_rows(stats_J1, stats_A3) |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("P_"), ~ . * 100),
      dplyr::across(dplyr::starts_with("lambda_"), ~ round(., 3)),
      dplyr::across(dplyr::starts_with("P_"), ~ round(., 3)),
      Class = factor(Class, levels = c("J1", "A3")),
      theta = factor(theta, levels = c(8, 12, 16))
    ) |>
    tidyr::pivot_longer(
      cols = c(dplyr::starts_with("lambda_"), dplyr::starts_with("P_")),
      names_to = "temp_col",
      values_to = "value"
    ) |>
    dplyr::mutate(
      Parameter = ifelse(grepl("lambda", temp_col), "\u03bb", "%P"),
      Percentile = gsub(".*_", "", temp_col)
    ) |>
    dplyr::select(-temp_col) |>
    tidyr::pivot_wider(names_from = Percentile, values_from = value) |>
    dplyr::select(
      "Temp (\u00b0C)" = theta,
      Class,
      Survival = J1_category,
      Parameter,
      Min,
      P10,
      P25,
      P50,
      P75,
      P90,
      Max
    ) |>
    dplyr::arrange('Temp (\u00b0C)', Class, Survival, dplyr::desc(Parameter)) |>
    data.table::as.data.table()

  return(table_long)
}
