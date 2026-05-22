
library(targets)
library(tarchetypes)
library(tidyverse)
library(janitor)
library(pointblank)


load_data <- function(file) {
  readr::read_csv(file) |>
    janitor::remove_empty() |>
    janitor::remove_empty()
}

  
    

    
  