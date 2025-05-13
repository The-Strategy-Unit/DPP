#' -----------------------------------------------------------------------------
#' DATA FROM FINGERTIPS
#' 
#' Using the FingertipsR package to source relevant data
#' -----------------------------------------------------------------------------

# libraries
library(tidyverse)
library(here)
library(fingertipsR)
#library(qs)
library(qs2)

# main -------------------------------------------------------------------------

# get area types
f_areas <- fingertipsR::area_types()

# get all profiles
f_profiles <- fingertipsR::profiles()

# select the id of interest
# 20 = national GP profiles
# 21 = national GP profiles - supporting indicators
# 135 = cardiovascular disease
f_indicators <- indicators(ProfileID = c(20, 21, 135, 20))
f_indicators <- indicators(ProfileID = c(20))

# list all indicators
f_indicators_all <- indicators_unique()

# compile a list of likely indicators
interesting_ind_id <- f_indicators |> 
  filter(str_detect(IndicatorName, 'atrial|Atrial|AF|coag|fibril')) |> 
  pull(IndicatorID) |> 
  unique()

# or, just list all indicators
interesting_ind_id <- f_indicators |> 
  filter(str_de)
  pull(IndicatorID) |> 
  unique()

# data can be accessed using the indicator id, NB area type 7 = practice
df <- fingertips_data(IndicatorID = interesting_ind_id, AreaTypeID = 7)

# save this locally for later use
#qsave(x = df, file = here('data', 'fingertips', 'df.qs'))
qs2::qd_save(
  object = df,
  file = here('data', 'fingertips', 'df.qs2')
)

# load from local file
#df <- qread(file = here('data', 'fingertips', 'df.qs'))
df <- qs2::qd_read(file = here('data', 'fingertips', 'df.qs2'))

# what indicators do we have
df_ind <- df |> 
  filter(AreaType == 'GPs') |> 
  count(IndicatorID, IndicatorName)

# summarise time points by indicator  
df_ind_time <- df |> 
  filter(AreaType == 'GPs') |> 
  summarise(
    records = n(), .by = c(IndicatorID, IndicatorName, Timeperiodrange, Timeperiod, TimeperiodSortable)
  )

# age -----
df_age <- fingertips_data(IndicatorID = 93468, AreaTypeID = 7)

# hypertension ----
df_hyp <- fingertips_data(IndicatorID = 219, AreaTypeID = 7)
saveRDS(
  object = df_hyp,
  file = here::here('data', 'fingertips', 'hypertension_qof.Rds')
)

# deprivation -----
df_dep <- fingertips_data(IndicatorID = 93553, AreaTypeID = 7)
# 2019 IMD
saveRDS(
  object = df_dep,
  file = here::here('data', 'fingertips', 'deprivation.Rds')
)

# rurality ----
# get practice by postcode
df_prac_pc <- vroom::vroom(
  file = here::here('data', 'open_geo', 'epraccur.csv'),
  delim = ',',
  show_col_types = FALSE,
  col_select = c(1, 2, 10),
  col_names = FALSE
) |> 
  dplyr::rename(
    prac_code = X1,
    prac_name = X2,
    postcode = X10
  ) |> 
  # remove spaces in postcode
  dplyr::mutate(
    postcode = postcode |> stringr::str_remove_all(pattern = ' ')
  )

# get postcode -> lsoa lookup
lu_pc_lsoa <- vroom::vroom(
  file = here::here('data', 'open_geo', 'PCD_OA_LSOA_MSOA_LAD_NOV19_UK_LU.csv'),
  delim = ',',
  show_col_types = FALSE
) |> 
  # select fields
  dplyr::select(
    postcode = pcd7,
    lsoa_code = lsoa11cd
  ) |> 
  # remove spaces in the postcode
  dplyr::mutate(
    postcode = postcode |> stringr::str_remove_all(pattern = ' ')
  )

# get lsoa -> rurality lookup
lu_lsoa_rural <- vroom::vroom(
  file = here::here('data', 'open_geo', 'rural_urban_lsoa_2011.csv'),
  delim = ',',
  show_col_types = FALSE,
) |> 
  # select fields
  dplyr::select(
    lsoa_code = LSOA11CD,
    rural_code = RUC11CD,
    rural_desc = RUC11
  )

# combine together
df_prac_pc_rural <-
  df_prac_pc |> 
  # add lsoa to data
  dplyr::left_join(
    y = lu_pc_lsoa,
    by = dplyr::join_by(
      postcode == postcode
    )
  ) |> 
  # add rurality 
  dplyr::left_join(
    y = lu_lsoa_rural,
    by = dplyr::join_by(
      lsoa_code == lsoa_code
    )
  ) |> 
  # convert rural code to ordered factor (? better for matching)
  dplyr::mutate(
    rural_code = rural_code |> 
      # ordering from urban > rural
      forcats::fct(
        levels = c('A1', 'B1', 'C1', 'C2', 'D1', 'D2', 'E1', 'E2')
      ) |> 
      forcats::fct_na_value_to_level(level = 'NA')
  )

# save for later use
saveRDS(
  object = df_prac_pc_rural,
  file = here::here('data', 'open_geo', 'gp_rurality.Rds')
)

# obesity ------
df_obes <- fingertips_data(IndicatorID = 92588, AreaTypeID = 7)
# save for later use
saveRDS(
  object = df_obes,
  file = here::here('data', 'fingertips', 'obesity_qof.Rds')
)

# diabetes ------------
df_diab <- fingertips_data(IndicatorID = 241, AreaTypeID = 7)
# save for later use
saveRDS(
  object = df_diab,
  file = here::here('data', 'fingertips', 'diabetes_qof.Rds')
)
