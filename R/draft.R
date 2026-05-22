
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

##Expectation/Validation

validate_patients <- function(patients) {
  patients |> 
    col_vals_between(
      birthdate,
      as.Date("1900-01-01"),
      Sys.Date(),
      label = "Birthdate",
      na_pass = TRUE
    ) |>
    #Birth before death
    col_vars_gte(
      deathdate,
      vars(birthdate),
      label = "Death data after birth date",
      na_pass = TRUE
    ) |>
    #SSN check
    col_vals_regenx(
      ssn, 
    "[0-9]{3}-[0-9]{2}-[0-9]{4}$",
      label = "SSN must follow XXX-XX-XXXX format"
    ) |>
  }
  
    

    
  