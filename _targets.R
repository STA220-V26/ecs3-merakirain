library(targets)
library(tarchetypes)
library(duckplyr)
library(leaflet)
library(decoder)
library(stringr)

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
    janitor::remove_empty(quiet = FALSE) |>
    janitor::remove_constant(quiet = FALSE)}
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
tar_target(lastdate, duckplyr::read_csv_duckdb("data-fixed/payer_transitions.csv") |>
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
 ),
 #step 11: interactive map visualization (cool!)
 tar_target(cool_map, leaflet::leaflet(data = names) |>
 leaflet::addTiles() |>
 leaflet::addMarkers(~lon, ~lat, label = ~full_name)),
#this probably would not make the best statistical inference for the whole of USA, nor the whole world...


#step 12: convert all data to parquet files
tar_target(
  parquet_files,
  {
    zip::unzip("data.zip")
    fs::dir_create("data-parquet")

    csv2parquet <- function(file) {
      new_file <- file |>
        stringr::str_replace("-fixed", "-parquet") |>
        stringr::str_replace(".csv", ".parquet")

      duckplyr::read_csv_duckdb(file) |>
        duckplyr::compute_parquet(new_file)
    }

    fs::dir_ls("data-fixed", glob = "*.csv") |>
      purrr::walk(csv2parquet)
    fs::dir_delete("data-fixed")

    "done"
  }
),

#step 13: load the procedure data
tar_target(procedures, duckplyr::read_parquet_duckdb("data-parquet/procedures.parquet") |>
  collect()
),

#step 14: select observations with reasoncode_icd10
tar_target(reasoncode, procedures |>
 select(patient, reasoncode_icd10, start) |>
 filter(!is.na(reasoncode_icd10)) |>
 collect()
),

#step:15 create procedures data table
tar_target(
  procedures_dt,
    procedures |>
      mutate(year = lubridate::year(start))
),

tar_target(
  proc_n_adults,
  {
    procedures_dt |>
      inner_join(
        patients |>
          mutate(birthdate = as.Date(birthdate)) |>
          select(id, birthdate),
        by = c("patient" = "id")
      ) |>
      filter(year - lubridate::year(birthdate) >= 18) |>
      count(reasoncode_icd10, year)
  }
),

#step 16: code to description
tar_target(
  cond_by_year,
  proc_n_adults |>
    mutate(reasoncode_icd10 = as.character(reasoncode_icd10)) |>
    left_join(
      decoder::icd10se |>
        as.data.frame() |>
        mutate(key = as.character(key)),
      by = c("reasoncode_icd10" = "key")
    )

),


#step 17:  5 most common conditions
tar_target(
  top5,
  cond_by_year |>
    group_by(value) |>
    summarise(N = sum(n), .groups = "drop") |>
    arrange(desc(N)) |>
    slice_head(n = 5) |>
    pull(value)
),

  #step 18: visualization 
tar_target(
  plot_cond_by_year,
  cond_by_year |>
    filter(value %in% top5) |>
    count(year, value) |>
    ggplot(aes(year, n, color = value)) +
    geom_line() +
    theme(legend.position = "bottom") +
    guides(color = guide_legend(ncol = 1)) +
    scale_color_discrete(
      labels = function(x) str_wrap(x, width = 40)
    )
)
 #we should focus on later time where we know that data is properly reported. 
 #it shows the changes of documented conditions over time;however it should not be relied upon because of the lengthy time in between (which could include different coding and populations etc. ) 
 #i think the focus here is about the reporting rather than the disease/treatment 
)


