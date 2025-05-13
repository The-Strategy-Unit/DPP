#' -----------------------------------------------------------------------------
#' GET MATCHING VARIABLES DATA
#' 
#' Combine the separate data sources for the matching variables into a single
#' data frame in preparation for matching.
#' -----------------------------------------------------------------------------

# udf --------------------------------------------------------------------------

#' Set invalid values in a column to zero
#' 
#' Changes any non-valid values in a supplied column to zero
#'
#' @param col vector - dplyr::column - a column in a df to which this function should be applied
#'
#' @returns vector
set_invalid_to_zero <- function(col) {
  
  col_return <-
    col |> 
    dplyr::tibble() |> 
    dplyr::mutate(
      col = dplyr::case_when(
        !is.finite(col) ~ 0,
        .default = col
      )
    ) |> 
    dplyr::pull(col)
  
  return(col_return)
}


# read the data ----------------------------------------------------------------
# gp registered population, proportion male, proportion 65+ (needs filtering to Apr 2023)
df_gp_pop <- readRDS(
  file = here::here('data', 'gp_pop', 'gp_pop_summary.Rds')
)

# gp population per clinical fte
df_gp_wf <- readRDS(
  file = here::here('data', 'gp_wf', 'gp_workforce.Rds')
)

# practice weighted deprivation (needs filtering to 2019)
df_imd <- readRDS(
  file = here::here('data', 'fingertips', 'deprivation.Rds')
)

# practice diabetes QOF (needs filtering to 2022/23)
df_diabetes <- readRDS(
  file = here::here('data', 'fingertips', 'diabetes_qof.Rds')
)

# practice hypertension QOF (needs filtering to 2022/23)
df_hypertension <- readRDS(
  file = here::here('data', 'fingertips', 'hypertension_qof.Rds')
)

# practice obesity QOF (needs filtering to 2022/23)
df_obesity <- readRDS(
  file = here::here('data', 'fingertips', 'obesity_qof.Rds')
)

# practice rurality
df_rurality <- readRDS(
  file = here::here('data', 'open_geo', 'gp_rurality.Rds')
)

# practice details
# df_prac <- readRDS(
#   file = here::here('data', 'ods', 'gp_practice_details.Rds')
# )
# NB, above file's practice codes don't seem to match with the codes used elsewhere

# using this file instead:
# https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/april-2023
# this is an extract from the Apr 2023 data for registered populations and provides
# a map between practice to higher geographies.
df_prac <- readr::read_csv(
  file = here::here('data', 'gp_pop', '2023_04_gp-reg-pat-prac-map.csv'),
  show_col_types = FALSE
)

df_intervention <- readRDS(
  file = here::here('data', 'general', 'intervention_practices.Rds')
)

# cvd_prevent outcomes - using Mar 2023 (pre) and Jun 2024 (post)
df_cvd <- readRDS(
  file = here::here('data', 'cvd_prevent', 'cvd_prevent.Rds')
) 

# list time periods
df_cvd |> 
  dplyr::count(time_period_id, year_month)

# create flags for each practice
df_cvd_summary <-
  df_cvd |> 
  dplyr::filter(time_period_id %in% c(8, 20)) |> 
  dplyr::summarise(
    flag_outcome = dplyr::case_when(
      !is.na(numerator) ~ TRUE,
      .default = FALSE
    ),
    .by = c(area_code, area_name, indicator_code, time_period_id)
  )

# pre-intervention period
df_cvd_summary_pre <-
  df_cvd_summary |> 
  dplyr::filter(
    indicator_code == 'CVDP002AF',
    time_period_id == 8,
    flag_outcome == TRUE
  )

# post-intervention period
df_cvd_summary_post <-
  df_cvd_summary |> 
  dplyr::filter(
    indicator_code == 'CVDP002AF',
    time_period_id == 20,
    flag_outcome == TRUE
  )


# data checking ----------------------------------------------------------------
test_data <- TRUE
if (test_data) {
  print('df_imd')
  df_imd |> dplyr::count(Timeperiod) |> print()
  
  print('df_diabetes')
  df_diabetes |> dplyr::count(Timeperiod) |> print()
  
  print('df_hypertension')
  df_hypertension |> dplyr::count(Timeperiod) |> print()
  
  print('df_obesity')
  df_obesity |> dplyr::count(Timeperiod) |> print()
  
  # print('df_rurality')
  # df_rurality |> dplyr::count()
}

# combine the data -------------------------------------------------------------
df_matching <-
  df_gp_pop |> 
  dplyr::filter(extract_date == as.Date('2023-04-01')) |> 
  dplyr::select(
    prac_code = org_code,
    adults_headcount = adults,
    adults_male_prop,
    adults_65_plus_prop
  ) |> 
  # add in gp workforce
  dplyr::left_join(
    y = df_gp_wf |> 
      dplyr::select(
        prac_code,
        patients_per_clinician
      ),
    by = dplyr::join_by(prac_code == prac_code)
  ) |> 
  # add in deprivation
  dplyr::left_join(
    y = df_imd |> 
      dplyr::filter(Timeperiod == 2019) |> 
      dplyr::select(
        AreaCode,
        deprivation_score = Value
      ) |> 
      dplyr::distinct(),
    by = dplyr::join_by(prac_code == AreaCode)
  ) |> 
  # add in diabetes prevalence
  dplyr::left_join(
    y = df_diabetes |> 
      dplyr::filter(Timeperiod == '2022/23') |> 
      dplyr::select(
        AreaCode,
        diabetes_prevalence = Value
      ) |> 
      dplyr::distinct(),
    by = dplyr::join_by(prac_code == AreaCode)
  ) |> 
  # add in hypertension prevalence
  dplyr::left_join(
    y = df_hypertension |> 
      dplyr::filter(Timeperiod == '2022/23') |> 
      dplyr::select(
        AreaCode,
        hypertension_prevalence = Value
      ) |> 
      dplyr::distinct(),
    by = dplyr::join_by(prac_code == AreaCode)
  ) |> 
  # add in obesity prevalence
  dplyr::left_join(
    y = df_obesity |> 
      dplyr::filter(Timeperiod == '2022/23') |> 
      dplyr::select(
        AreaCode,
        obesity_prevalence = Value
      ) |> 
      dplyr::distinct(),
    by = dplyr::join_by(prac_code == AreaCode)
  ) |> 
  # add in rurality
  dplyr::left_join(
    y = df_rurality |> 
      dplyr::select(
        prac_code,
        rural_code
      ) |> 
      dplyr::distinct(),
    by = dplyr::join_by(prac_code == prac_code)
  ) |> 
  # add in gp practice details
  dplyr::left_join(
    y = df_prac |> 
      janitor::clean_names() |> 
      dplyr::select(
        practice_code,
        practice_name,
        practice_postcode
      ) |> 
      dplyr::distinct(),
    by = dplyr::join_by(prac_code == practice_code)
  ) |> 
  # # tidy up values
  # dplyr::mutate(
  #   # convert NA and Inf values to zeroes
  #   patients_per_clinician = patients_per_clinician |> set_invalid_to_zero(),
  #   deprivation_score = deprivation_score |> set_invalid_to_zero()
  #   )
  # )
  # add in the flag for intervention / control
  dplyr::left_join(
    y = df_intervention |> 
      dplyr::select(
        practice_code,
        flag_intervention
      ) |> 
      dplyr::distinct(),
    by = dplyr::join_by(prac_code == practice_code)
  ) |> 
  # assume all non-TRUE flags are controls
  tidyr::replace_na(list(flag_intervention = FALSE)) |> 
  # put practice details first
  dplyr::relocate(
    dplyr::any_of(c('prac_code', 'practice_name', 'practice_postcode')), 
    .before = 1
  ) |> 
  # add in the flags for outcomes (pre and post intervention)
  dplyr::left_join(
    y = df_cvd_summary_pre |> 
      dplyr::select(area_code, flag_outcome_pre = flag_outcome),
    by = dplyr::join_by(prac_code == area_code)
  ) |> 
  dplyr::left_join(
    y = df_cvd_summary_post |> 
      dplyr::select(area_code, flag_outcome_post = flag_outcome),
    by = dplyr::join_by(prac_code == area_code)
  )

# save file (with missings) for future use --------------------------------
saveRDS(
  object = df_matching,
  file = here::here('data', 'matching', 'df_matching_missing.Rds')
)

# dq checks ----------------------------------------------------------------
# count flags
df_matching |> dplyr::count(flag_intervention)
  
# Summarise missing values
df_matching |> 
  finalfit::missing_plot()

# summarise missing for intervention / control site
df_matching |> summary()
purrr::map(
  .x = c(TRUE, FALSE),
  function(.x) {
    df_matching |> 
      dplyr::filter(flag_intervention == .x) |> 
      finalfit::missing_plot()
  }
)

# ... looks like more missing values in control sites

df_matching |> 
  finalfit::missing_glimpse()

purrr::map(
  .x = c(TRUE, FALSE),
  function(.x) {
    df_matching |> 
      dplyr::filter(flag_intervention == .x) |> 
      finalfit::missing_glimpse()
  }
)
# ... yup, max 1.2% missing for deprivation in intervention group (deprivation),
# whereas max of 4.8% missing for deprivation, diabetes, hypertension, obesity

df_matching |> 
  finalfit::missing_pattern()

purrr::map(
  .x = c(TRUE, FALSE),
  function(.x) {
    df_matching |> 
      dplyr::filter(flag_intervention == .x) |> 
      finalfit::missing_pattern()
  }
)

# can I summarise this in a tibble?
match_missing_summary <- 
  df_matching |> 
  dplyr::select(-c(prac_code, practice_name, practice_postcode, adults_headcount)) |> 
  dplyr::mutate(rows = dplyr::n(), .by = flag_intervention) |> 
  dplyr::summarise(
    dplyr::across(
      .cols = dplyr::everything(),
      .fns = function(.x) {
        (sum(!is.na(.x)) / length(.x)) |> 
          scales::percent(accuracy = 0.1)
      }
    ),
    .by = c(flag_intervention, rows)
  )

str(match_missing_summary)
match_missing_summary
