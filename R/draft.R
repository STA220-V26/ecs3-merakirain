
library(targets)
library(tarchetypes)

# Declare packages used in the pipeline
tar_option_set(packages = c("tidyverse", "janitor", "pointblank"), format = "qs")  # Default storage format. qs (which is actually qs2) is fast.


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
  tar_target(patients, data.table::fread("data-fixed/patients.csv")) |>
    janitor::remove_empty(patients, quiet = FALSE) |>
    janitor::remove_constant(patients, quiet = FALSE)


)