library(targets)
library(tarchetypes)
library(duckplyr)

# Declare packages used in the pipeline
tar_option_set(packages = c("tidyverse", "janitor", "pointblank"))  


# Check if file exists, if not, it will download
if (!fs::file_exists("data.zip")) {
  message("Downloading data.zip from GitHub")
  curl::curl_download(
    "https://github.com/STA220/cs/raw/refs/heads/main/data.zip",
    "data.zip",
    quiet = FALSE
  )
}

# List steps of your pipeline
list(
  # Step 1: Register the data
  tar_target(zipdata, "data.zip", format = "file"),

  # Step 2: unzip the data
  tar_target(csv_files, zip::unzip(zipdata)),

  # Step 3: Load patients file raw data
  tar_target(patients, {
    read_csv(unz(zipdata, "data-fixed/patients.csv")) |>
    janitor::remove_empty(patients, quiet = FALSE) |>
    janitor::remove_constant(patients, quiet = FALSE)}
  ),

  #Step 4: validate 
  tar_target(checks,
     patients |> 
  create_agent(label = "A very simple example") |>
  col_vals_between(
    birthdate,
    as.Date("1900-01-01"),
    Sys.Date(),
    label = "Birthdate",
    na_pass = TRUE
  ) |>
  # Birth before death
  col_vals_gte(
    deathdate,
    vars(birthdate),
    label = "Death date after birth date",
    na_pass = TRUE
  ) |>
  # SSN check
  col_vals_regex(
    ssn, 
    "[0-9]{3}-[0-9]{2}-[0-9]{4}$",
    label = "SSN must follow XXX-XX-XXXX format"
  ) |>
  col_is_integer(
    id,
    label = "patient id"
  ) |>
  interrogate()),

tar_target(report, export_report(checks, "patient_validation.html")
),
#step 5; unzip payer file
tar_target(payer_file, unzip("data.zip", files = "data-fixed/payer_transitions.csv")
  ), 
#step 6 use duckdb to perform calculations on disk 
tar_target(lastdata, duckplyr::read_csv_duckdb("data-fixed/payer_transitions.csv") |>
 summarise(lastdate = max(start_date)) |>
 collect() |>
 pluck("lastdate") |>
 as.Date()
 ),

 #step 7: calc extraction date for living patients
 tar_target(patients_li, patients |>
   filter(is.na(deathdate)) |>
   mutate(
    age_extract =
      as.integer(as.Date(lastdate)- as.Date(birthdate)) %/% 365
   )),

 #step 8: age histogram
 tar_target(age_histogram, hist(patients_li$age_extract)),

 #step 9: how to address patients?
 tar_target(names, patients|> 
   mutate(
    full_name = paste(prefix, first, middle, last, suffix),
    full_name = trimws(full_name)) |> 
   mutate(across(where(is.character), trimws))
),

 #step 10: license or not? 
 tar_target(license, patients|>
   mutate(driver = !is.na(drivers))
 )

)

