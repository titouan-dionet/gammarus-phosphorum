#' Gammarus-Phosphorum Pipeline Execution
#'
#' @title Execute the Gammarus-Phosphorum analysis pipeline
#'
#' @description
#' This script serves as the main entry point for running the Gammarus-Phosphorum
#' project pipeline. It executes the following steps:
#' 1. Restores dependencies using renv
#' 2. Sets the targets configuration
#' 3. Visualizes the pipeline structure (pre-execution) and saves it
#' 4. Executes the pipeline using targets
#'
#' The pipeline analyzes phosphorus stoichiometry and population dynamics in
#' Gammarus fossarum to test the Growth Rate Hypothesis at the population level.
#'
#' @author Titouan Dionet \email{titouan.dionet@univ-lorraine.fr}
#'
#' @date 2025/04/15
#'
#' @usage
#' source("make.R")  # Run the entire pipeline
#'

# ---- Setup ----
# Print header
cat("========================================================\n")
cat(" GAMMARUS-PHOSPHORUM PIPELINE\n")
cat("========================================================\n")
cat("Starting execution at", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Check for renv and initialize if not already done
if (!requireNamespace("renv", quietly = TRUE)) {
  cat("Installing renv package...\n")
  install.packages("renv")
}

# ---- Restore Dependencies ----
cat("Restoring dependencies from renv.lock...\n")
renv_result <- try(renv::restore(), silent = TRUE)

if (inherits(renv_result, "try-error")) {
  cat("Error restoring dependencies. Please run renv::restore() manually.\n")
  cat("Error details:", attr(renv_result, "condition")$message, "\n")
} else {
  cat("Dependencies restored successfully.\n")
}

# ---- Configure Targets ----
cat("\nConfiguring targets...\n")

if (!requireNamespace("targets", quietly = TRUE)) {
  cat("Installing targets package...\n")
  install.packages("targets")
}

if (!requireNamespace("here", quietly = TRUE)) {
  cat("Installing here package...\n")
  install.packages("here")
}

# Set targets configuration
targets::tar_config_set(
  store = here::here("outputs", "_targets"),
  script = here::here("analyses", "pipeline", "_targets.R")
)

# ---- Create data directory ----
data_dir <- here::here("data", "raw_data")
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  cat("Created data/raw_data/ directory.\n")
  cat("Please place the raw data files in:", data_dir, "\n")
  cat("Data available at: https://doi.org/10.57745/KTUAUG\n")
  stop(
    "Pipeline stopped: raw data files are missing. Download them and rerun make.R."
  )
}

# ---- Pre-Execution Visualization ----
cat("Generating and saving pipeline visualization (pre-execution)...\n")
pre_viz <- try(
  targets::tar_visnetwork(targets_only = TRUE, callr_function = NULL),
  silent = TRUE
)

if (inherits(pre_viz, "try-error")) {
  cat("Error generating pre-execution visualization.\n")
  cat("Error details:", attr(pre_viz, "condition")$message, "\n")
} else {
  # Create documentation directory if it doesn't exist
  doc_dir <- here::here("outputs", "pipeline")
  if (!dir.exists(doc_dir)) {
    dir.create(doc_dir, recursive = TRUE)
  }

  # Save as HTML
  html_file <- file.path(doc_dir, "pipeline_pre_execution.html")
  htmlwidgets::saveWidget(pre_viz, html_file, selfcontained = TRUE)
  cat("Pre-execution visualization saved to:", html_file, "\n")

  # If in an interactive session, display the visualization
  if (interactive()) {
    show(pre_viz)
    cat("Pipeline visualization displayed in RStudio Viewer.\n")
  }
}

# ---- Execute Pipeline ----
cat("\nExecuting pipeline...\n")
start_time <- Sys.time()

# Run the pipeline
result <- try(targets::tar_make())

end_time <- Sys.time()
execution_time <- difftime(end_time, start_time, units = "mins")

# Check for errors
if (inherits(result, "try-error")) {
  cat("\nPipeline execution FAILED.\n")
  cat("Error details:", attr(result, "condition")$message, "\n")
} else {
  cat("\nPipeline execution COMPLETED successfully.\n")
}

cat("Execution time:", round(as.numeric(execution_time), 2), "minutes\n")

# ---- Post-Execution Visualization ----
cat("\nGenerating and saving pipeline visualization (post-execution)...\n")
post_viz <- try(
  targets::tar_visnetwork(targets_only = TRUE, callr_function = NULL),
  silent = TRUE
)
if (inherits(post_viz, "try-error")) {
  cat("Error generating post-execution visualization.\n")
  cat("Error details:", attr(post_viz, "condition")$message, "\n")
} else {
  # Save as HTML
  html_file <- file.path(doc_dir, "pipeline_post_execution.html")
  htmlwidgets::saveWidget(post_viz, html_file, selfcontained = TRUE)
  cat("Post-execution visualization saved to:", html_file, "\n")

  # If in an interactive session, display the visualization
  if (interactive()) {
    print(post_viz)
    cat("Pipeline visualization displayed in RStudio Viewer.\n")
  }
}

# ---- Summary ----
cat("\n========================================================\n")
cat(" PIPELINE SUMMARY\n")
cat("========================================================\n")

# Get summary information
completed_targets <- length(targets::tar_completed())
failed_targets <- length(targets::tar_errored())

cat("Targets executed:", completed_targets + failed_targets, "\n")
cat("Successful targets:", completed_targets, "\n")
cat("Failed targets:", failed_targets, "\n")

if (failed_targets > 0) {
  cat("\nFailed targets:\n")
  for (target in targets::tar_errored()) {
    cat("  -", target, "\n")
  }
}

cat("\nExecution finished at", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("All documentation saved to:", doc_dir, "\n")
cat("========================================================\n")

# ---- Interactive Mode (if running in an interactive session) ----
if (interactive()) {
  cat("\nInteractive Mode:\n")
  cat(
    "1. To view the pipeline manifest interactively: targets::tar_manifest()\n"
  )
  cat("2. To visualize the pipeline graph: targets::tar_visnetwork()\n")
  cat("3. To view target metadata: targets::tar_meta()\n")
  cat("4. To load a specific target: targets::tar_load(target_name)\n")
  cat("5. To view all outputs: browse to", doc_dir, "\n")
}
