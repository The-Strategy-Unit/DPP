#' -----------------------------------------------------------------------------
#' Get registered population per GP WTE
#' 
#' Using data from here:
#' https://digital.nhs.uk/data-and-information/publications/statistical/general-and-personal-medical-services/30-april-2023
#' -----------------------------------------------------------------------------



# I've downloaded CSV files from the above site for April 2023 to use in the 
# matching process.
# This script is to read in the file and output
# a) GP WTE (including trainees)
# b) Patient headcount
# c) Work out the number of patients per WTE

# read the csv
df_gp_wf_load <- vroom::vroom(
  file = here::here('data', 'gp_wf', 'csv', '1 General Practice – April 2023 Practice Level - Detailed.csv'),
  delim = ',',
  show_col_types = FALSE
)

df_gp_wf <- 
  df_gp_wf_load |> 
  janitor::clean_names() |> 
  # select the fields we are interested in
  dplyr::select(
    prac_code,
    prac_name,
    total_patients,
    total_gp_fte,
    total_nurses_fte, # additional field selected by Emma Wells
    total_dpc_fte, # additional field selected by Emma Wells
    gp_source,
    dpc_source,
  ) |> 
  dplyr::rowwise() |> 
  # work out the number of patients per clinician
  dplyr::mutate(
    total_clinical_fte = sum(total_gp_fte, total_nurses_fte, total_dpc_fte, na.rm = TRUE),
    month = zoo::as.yearmon('2023-04-01'),
    patients_per_clinician = total_patients / total_clinical_fte
  ) |> 
  dplyr::ungroup()

df_missing <- 
  df_gp_wf |> 
  filter(is.na(total_clinical_fte))

# save the data for later use
saveRDS(
  object = df_gp_wf,
  file = here::here('data', 'gp_wf', 'gp_workforce.Rds')
)
