#' gammarus-phosphorum: A Research Compendium
#' 
#' @description 
#' A paragraph providing a full description of the project and describing each 
#' step of the workflow.
#' 
#' @author Titouan Dionet \email{titouan.dionet@univ-lorraine.fr}
#' 
#' @date 2024/12/10



## Install Dependencies (listed in DESCRIPTION) renv ----

renv::restore()

## Load Project Addins (R Functions) ----
targets::tar_config_set(store = here::here("outputs", "pipeline"),
                        script = here::here("analyses", "pipeline.R"))

## Pre visualisation ----
targets::tar_visnetwork()

## Launch pipeline ----
targets::tar_make()

## Post visualisation ----
targets::tar_visnetwork()
