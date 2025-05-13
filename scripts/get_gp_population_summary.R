#' -----------------------------------------------------------------------------
#' GP REGISTERED POPULATIONS
#' 
#' Processes to extract key measures from GP 
#' 
#' NHS Digital produce monthly summaries of the number of people registered at
#' each practice by age and gender. These files are available here:
#' https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice
#' 
#' I've downloaded and processed csv files for each month over a period of years
#' that I'm interested in and and combined to a single .Rds file, called 
#' 'gp_pop_by_age_gender.Rds'
#' -----------------------------------------------------------------------------

# libraries
library(tidyverse)
library(here)

# read the data
df <- readRDS(file = here::here('data', 'gp_pop', 'gp_pop_by_age_gender.Rds'))

# summarise counts of patients useful for the evaluation
df_summary <- 
  df |> 
  dplyr::filter(!age == 'ALL') |> # exclude totals and keep just single year age bands
  dplyr::summarise(
    # total people
    people = sum(number_of_patients, na.rm = TRUE),
    
    # people aged 18+
    adults = sum(number_of_patients[!age %in% c(0:18)], na.rm = TRUE),
    adults_male = sum(number_of_patients[!age %in% c(0:18) & sex == 'MALE'], na.rm = TRUE),
    adults_male_prop = adults_male / adults,
    
    # people aged 65+
    adults_65_plus = sum(number_of_patients[!age %in% c(0:64)], na.rm = TRUE),
    adults_65_plus_prop = adults_65_plus / adults,
    
    # people aged 80+ (old metric, keeping for records)
    adults_80_plus = sum(number_of_patients[!age %in% c(0:79)], na.rm = TRUE),
    adults_80_plus_prop = adults_80_plus / adults,
    
    .by = c(extract_date, org_code)
  ) |> 
  dplyr::arrange(extract_date, org_code)

# save the data for future use
saveRDS(
  object = df_summary,
  file = here::here('data', 'gp_pop', 'gp_pop_summary.Rds')
)
