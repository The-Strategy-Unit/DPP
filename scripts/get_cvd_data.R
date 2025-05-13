#' -----------------------------------------------------------------------------
#' GET DATA FROM CVD PREVENT API
#' 
#' Use the API to get the data we are interested in
#' 
#' INDICATORS ---
#' ID = 1 | CVDP001AF | Prevalence of GP recorded atrial fibrillation
#' ID = 7 | CVDP002AF | Patients with GP recorded atrial fibrillation and with a CHADS2 or CHA2DS2-VASc score of 2 or more, who are currently treated with any oral anticoagulant.
#' 
#' .. however, ID1 is NOT available at GP practice level.
#' 
#' Will attempt to recreate ID1 by using the denominator from ID7 as the numerator for ID1.
#' Then obtain a denominator for ID1 from another indicator, such as the following:
#' 
#' ID = 20 | CVDP005HYP | Patients with single bp reading of 140/90 mmHg (at risk of hypertension) who do not have a record of GP recorded hypertension.
#' 
#' -----------------------------------------------------------------------------

# libraries --------------------------------------------------------------------
library(tidyverse)
library(cvdprevent)

# functions --------------------------------------------------------------------
#' Get GP indicators for a single time period
#' 
#' Returns indicator data for each participating GP practice for the specified
#' time period.
#'
#' @param .time_period_id Integer - a valid time period ID
#' @param .year_month Zoo::yearmon - a yearmon object indicating the year and month of the time period
#' @param .gp_list Tibble - reference data containing the GP registered adult population per month
#'
#' @return Tibble of data showing indicator data for each practice in the time period
get_gp_indicator_for_time_period <- function(.time_period_id, .year_month, .gp_list) {
  
  # download the indicator data for CVDP002AF
  df_1 <- cvd_indicator_raw_data(
    indicator_id = 7,
    time_period_id = .time_period_id,
    system_level_id = 5
  ) |> 
    janitor::clean_names() |> 
    # add in additional information
    mutate(
      time_period_id = .time_period_id,
      year_month = .year_month,
      indicator_id = 7
    )
  
  # duplicate the data
  df_2 <- df_1 |> 
    dplyr::select(
      dplyr::any_of(
        c(
          'area_code',
          'area_name',
          'category_attribute',
          'numerator' = 'denominator',
          'metric_category_name',
          'metric_category_type_name',
          'value_note',
          'time_period_id',
          'year_month'
        )
      )
    ) |> 
    # add in the denominator from the list size data
    dplyr::left_join(
      y = .gp_list |> 
        dplyr::select(
          year_month,
          org_code,
          denominator = adults # this is what we want joining to the data
        ),
      by = dplyr::join_by(
        year_month == year_month,
        area_code == org_code
      )
    ) |> 
    # final updates
    dplyr::mutate(
      # add in indicator description
      indicator_id = 1,
      indicator_code = 'CVDP001AF_custom',
      indicator_name = 'Prevalence of GP recorded atrial fibrillation (custom)',
      indicator_short_name = 'AF: Prevalence (CVDP001AF) (custom)',
      
      # calculate the value
      value = round(((numerator / denominator) * 100), digits = 3)
    )
  
  # combine both sets of data together
  df_return <- bind_rows(
    df_1,
    df_2
  ) |>
    # order the variables
    select(
      area_code, area_name,
      time_period_id, year_month,
      indicator_id, indicator_code, indicator_name, indicator_short_name,
      metric_category_name, metric_category_type_name, category_attribute,
      numerator, denominator, value,
      value_note
    )
  
  return(df_return)
  
}

# main -------------------------------------------------------------------------

# get a list of time periods for practice data
gp_time_periods <- cvd_area_system_level_time_periods() |> 
  janitor::clean_names() |> 
  dplyr::filter(
    system_level_name == 'Practice', # only want time periods which contain practice-level data
    !time_period_id == 10 # the indicator we are interested in doesn't appear in time period 10 (to sept 2023)
  ) |> 
  dplyr::mutate(
    year_month = end_date |> 
      lubridate::parse_date_time(orders = '%a, %d %b %Y %H:%M:%S') |> 
      zoo::as.yearmon()
  )

# get gp list size data for each month
gp_list <- readRDS(file = here::here('data', 'gp_pop', 'gp_pop_summary.Rds')) |> 
  dplyr::mutate(
    year_month = zoo::as.yearmon(extract_date)
  )

# get the indicator data
df_indicators <-
  purrr::map2_dfr(
    .x = gp_time_periods$time_period_id,
    .y = gp_time_periods$year_month,
    .f = \(.x, .y) get_gp_indicator_for_time_period(
      .time_period_id = .x,
      .year_month = .y,
      .gp_list = gp_list # pass in the pre-loaded df for use
    )
  )

# problem starts with time period 10 - missing 'area code'
# test <- cvd_indicator_raw_data(
#   indicator_id = 7,
#   time_period_id = 6,
#   system_level_id = 5
# )


# save for later use
saveRDS(
  object = df_indicators,
  file = here::here('data', 'cvd_prevent', 'cvd_prevent.Rds')
)

# experiments --------------------

# time periods
time_periods <- cvd_time_period_list()

# what system levels for the latest time period
system_levels_time_period <- cvd_time_period_system_levels()

# latest data is id 17 (to March 2024)
system_levels <- cvd_time_period_system_levels()

# list practices for time period 17
practices <- cvd_area_list(time_period_id = 17, system_level_id = 5)

# list time periods with data at GP practice level
gp_time_periods <- 
  cvd_time_period_system_levels() |> 
  filter(SystemLevelName == 'Practice')

# what practices have data in the latest time period
#gp_list_time <- cvd_area_list(time_period_id = 18, system_level_id = 5)

# what indicators do we have
indicator_list <- cvd_indicator_list(time_period_id = 18, system_level_id = 5)
indicator_list_2 <- cvd_indicator_list(time_period_id = 17, system_level_id = 1)


df_1 <- cvd_indicator_raw_data(
  indicator_id = 7,
  time_period_id = 18,
  system_level_id = 5
) |> 
  janitor::clean_names()

# list indicator data for a given area
test <- cvd_indicator(time_period_id = 18, area_id = 1)
test$indicators |> view()

cvd_indicator_person_timeseries(indicator_id = 1, area_id = 1)

metric_list <- cvd_indicator_metric_list(time_period_id = 18, system_level_id = 5)

test <- cvd_indicator_raw_data(
  indicator_id = 1, 
  time_period_id = 18, 
  system_level_id = 8
) |> 
  janitor::clean_names()

meta <- cvd_indicator_details(indicator_id = 20)
