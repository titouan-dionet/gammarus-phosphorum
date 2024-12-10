#' gammarus-phosphorum: A Research Compendium
#' 
#' @description 
#' A paragraph providing a full description of the project and describing each 
#' step of the workflow.
#' 
#' @author Titouan Dionet \email{titouan.dionet@univ-lorraine.fr}
#' 
#' @date 2024/12/10



## Install Dependencies (listed in DESCRIPTION) ----

renv::restore()


## Load Project Addins (R Functions and Packages) ----

devtools::load_all(here::here())


## Global Variables ----

# You can list global variables here (or in a separate R script)


## Run Project ----

# List all R scripts in a sequential order and using the following form:
# source(here::here("analyses", "script_X.R"))
