#' -----------------------------------------------------------------------------
#' DOWNLOAD GP REGISTERED POPULATION DATA
#' 
#' Functions to download details about GP practice registered population from 
#' NHS Digital
#' -----------------------------------------------------------------------------

# set up -----------------------------------------------------------------------
url_base <- 'https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice'

# functions --------------------------------------------------------------------

#' List URLs to search for downloadable files
#' 
#' List out likely URLs for the 'Patients Registered at a GP Practice' publication
#' portal, by month.
#'
#' @param month_from String - the starting month in the format 'YYYY-MM-DD' 
#' @param month_to String - the end month in the format 'YYYY-MM-DD'
#'
#' @return Tibble of likely URLs
#' @examples
#' urls <- list_out_urls_to_search(month_from = '2020-03-01', month_to = '2024-06-01')
list_out_urls_to_search <- function(month_from, month_to) {
  month_from <- as.Date(month_from)
  month_to <- as.Date(month_to)
  sequence <- seq(from = month_from, to = month_to, by = 'month')
  urls <- sequence |> 
    dplyr::as_tibble() |> 
    dplyr::mutate(
      url = glue::glue('{url_base}/{format.Date(value, "%B-%Y")}') |> tolower()
    ) |> 
    dplyr::rename(month = value)
}


#' Get hyperlinks from a html page
#' 
#' Scrapes urls embedded within a html file.
#'
#' @param url String - the url to scape from
#'
#' @return Tibble - a list of urls embedded within the html page
get_links_from_url <- function(url, month) {
  
  # get the html file
  pg <- xml2::read_html(url)
  
  # extract hyperlinks
  links <- rvest::html_attr(rvest::html_nodes(pg, "a"), "href") |> 
    dplyr::as_tibble() |> 
    dplyr::mutate(month = month)
  
  return(links)
}


#' Identify links pointing to .zip or .csv files
#' 
#' Shortlist links from a long-list to those where they appear to point to a
#' .zip or a .csv file.
#'
#' @param links Tibble containing urls, as an output from `get_links_from_url` 
#'
#' @return Tibble - a shortlist of links pointing to .zip files and a column listing the filename
identify_zip_links <- function(links) {
  
  links <-
    links |> 
    dplyr::filter(
      stringr::str_ends(string = value, pattern = '.zip|.csv')
    ) |> 
    dplyr::mutate(
      file = basename(value)
    )
  
  return(links)
}

#' Download and read a .zip file
#' 
#' Downloads .zip files from a given url then reads this into a Tibble using
#' vroom.
#'
#' @param url String - the url pointing to the file of interest
#'
#' @return Tibble of data as contained within the .zip file
download_and_read_zip_file <- function(url) {
  
  temp_zip <- tempfile()
  download.file(url = url, destfile = temp_zip)
  df <- vroom::vroom(file = temp_zip, show_col_types = FALSE, col_types = 'cccccccc', delim = ',')
  
  return(df)
}

download_and_summarise <- function(url) {


  df <-
    # download the file
    download_and_read_zip_file(url = url) |>
    # summarise the data
    janitor::clean_names() |>
    dplyr::filter(age %in% c(as.character(18:94), '95+')) |>
    dplyr::mutate(number_of_patients = as.integer(number_of_patients)) |>
    dplyr::summarise(
      number_of_patients = sum(number_of_patients, na.rm = T),
      .by = c(org_code, extract_date)
    ) |>
    # prepare for output
    dplyr::mutate(
      extract_date = lubridate::parse_date_time2(
        x = extract_date,
        orders = c('%y%b%Y', '%Y-%m-%d')
      )
    ) |>
    dplyr::arrange(org_code, extract_date)

  return(df)

}

download_and_stash <- function(url, month) {
  
  # prepare some details
  file_ext <- tools::file_ext(url)
  file_name <- url |> basename() |> tools::file_path_sans_ext()
  
  # download
  temp_file <- tempfile()
  download.file(url = url, destfile = temp_file)
  
  # extract if a zip file
  if (file_ext == 'zip') {
    temp_file <- unzip(zipfile = temp_file)
  }
  
  # move this file to the 'raw' folder
  file.copy(
    from = temp_file,
    to = here::here('data', 'gp_pop', 'raw', glue::glue('{month}_{file_name}.csv')),
    overwrite = TRUE
  )
  
  # clear the temp file
  result <- file.remove(temp_file)
  
}

convert_csv_to_rds <- function(file_path) {
  
  # get the filename - excluding path and extension
  file_name <- file_path |> basename() |> tools::file_path_sans_ext()
  
  # load the file and process
  df <- vroom::vroom(
    file = file_path,
    delim = ',',
    col_types = 'cccccccc',
    skip_empty_rows = TRUE,
  ) |> 
    # prepare the data
    janitor::clean_names()
  
  # save the rds file
  saveRDS(
    object = df,
    file = here::here('data', 'gp_pop', 'rds', glue::glue('{file_name}.RDS'))
  )
  
  gc()
    
}

convert_csv_to_rds_v2 <- function(file_path) {
  
  # get the filename - excluding path and extension
  file_name <- file_path |> basename() |> tools::file_path_sans_ext()
  
  # load the file and process
  df <- read.csv(
    file = file_path,
    blank.lines.skip = TRUE,
    colClasses = 'character'
  ) |> 
    # prepare the data
    janitor::clean_names()
  
  # save the rds file
  saveRDS(
    object = df,
    file = here::here('data', 'gp_pop', 'rds', glue::glue('{file_name}.RDS'))
  )
  
}

#test <- convert_csv_to_rds('C:/Users/craig.parylo/OneDrive - NHS/Documents/Projects/1234 DPP/data/gp_pop/raw/2023-04-01_gp-reg-pat-prac-sing-age-female.csv')

#' Get GP registered populations for a given period
#' 
#' Coordinates the download of GP registered populations by practice and month
#' from the GP stat site.
#'
#' @param month_from String - the starting month in the format 'YYYY-MM-DD' 
#' @param month_to String - the end month in the format 'YYYY-MM-DD' 
#' @param files Character vector - a list of .zip files to download (if present) from the NHSD site
#'
#' @return Tibble of data showing 18+ years registered population by GP practice by month
get_gp_populations <- function(
    month_from, month_to, 
    files = c('gp-reg-pat-prac-sing-age-female.zip', 'gp-reg-pat-prac-sing-age-male.zip')
  ) {
  
  cli::cli_alert_info('Finding URLs...')
  
  # list urls to search
  urls <- list_out_urls_to_search(month_from = month_from, month_to = month_to)
  
  # get links from these urls
  urls_tofollow <-
    purrr::map2_dfr(
      .x = urls$url,
      .y = urls$month,
      .f = \(.x, .y) get_links_from_url(url = .x, month = .y)
    )
  
  # limit urls to .zip files
  urls_tofollow <- 
    urls_tofollow |> 
    identify_zip_links() |> 
    # limit further to files of interest
    dplyr::filter(
      file %in% files
    ) |> 
    dplyr::distinct()
  
  cli::cli_alert_info('Downloading files ...')

  # download these files and store in the raw folder
  purrr::map2(
    .x = urls_tofollow$value,
    .y = urls_tofollow$month,
    .f = \(.x, .y) download_and_stash(url = .x, month = .y)
  )
  
  cli::cli_alert_info('Converting to rds ...')
  # load these files, convert to RDS type and save in rds folder
  files <- list.files(
    path = here::here('data', 'gp_pop', 'raw'),
    full.names = TRUE
  ) |>
    dplyr::as_tibble() |>
    dplyr::distinct()

  purrr::walk(
    .x = files$value,
    .f = \(.x) convert_csv_to_rds_v2(file_path = .x)
  )

  # # read from rds and summarise
  # files <- list.files(
  #   path = here::here('data', 'gp_pop', 'rds'),
  #   full.names = TRUE
  # )
  # 
  # gp_pop_by_age <- purrr::map_dfr(
  #   .x = files$value,
  #   .f = \(readRDS(file = .x))
  # )
  # 
  # return(gp_pop_by_age)
}

read_rds_files <- function(folder_to_search) {
  
  # list out the RDS files
  files_rds <- list.files(
    #path = here::here('data', 'gp_pop', 'rds'),
    path = folder_to_search,
    full.names = TRUE
  )
  
  # read all to a single df
  df_return <- purrr::map_dfr(
    .x = files_rds,
    .f = \(.x) readRDS(file = .x)
  )
  
  # process the data
  df_return_v2 <-
    df_return |> 
    #dplyr::slice_sample(prop = 0.01) |> 
    dplyr::mutate(
      # parse count
      number_of_patients = number_of_patients |> as.integer(),
      
      # parse dates
      extract_date = lubridate::parse_date_time2(
        x = extract_date,
        orders = c(
          '%d-%b-%y', # 01-Apr-20
          '%y%b%Y',   # 01APR2021
          '%Y-%m-%d'  # 2024-06-01
        )
      ),
      
      # coalesce duplicate fields
      ccg_code = dplyr::coalesce(ccg_code, sub_icb_loc_code, sub_icb_location_code),
      ons_ccg_code = dplyr::coalesce(ons_ccg_code, ons_sub_icb_loc_code, ons_sub_icb_location_code)
    ) |> 
    # drop the duplicate fields
    dplyr::select(-c(sub_icb_loc_code, sub_icb_location_code, ons_sub_icb_loc_code, ons_sub_icb_location_code))
  
  # save to file
  saveRDS(
    object = df_return_v2,
    file = here::here('data', 'gp_pop', 'gp_pop_by_age_gender.Rds')
  )
  
  # return the result
  return(df_return_v2)
}

# main -------------------------------------------------------------------------

# test <- vroom::vroom(
#   file = here::here('data', 'gp_pop', 'raw', '2020-08-01_gp-reg-pat-prac-sing-age-female.csv'),
#   delim = ','
# )
# 
# df <- vroom::vroom(
#   file = here::here('data', 'gp_pop', 'raw', '2020-08-01_gp-reg-pat-prac-sing-age-female.csv'),
#   delim = ',',
#   col_types = 'cccccccc',
#   skip_empty_rows = TRUE,
# ) |> 
#   # prepare the data
#   janitor::clean_names()
# 
# df1 <- vroom::vroom(
#   file = files$value[1:30],
#   delim = ',',
#   col_types = 'cccccccc',
#   skip_empty_rows = TRUE,
# ) |> 
#   # prepare the data
#   janitor::clean_names()
# 
# df2 <- vroom::vroom(
#   file = files$value[31:40],
#   delim = ',',
#   col_types = 'cccccccc',
#   skip_empty_rows = TRUE,
# ) |> 
#   # prepare the data
#   janitor::clean_names()
# 
# df3 <- vroom::vroom(
#   file = files$value[41:50],
#   delim = ',',
#   col_types = 'cccccccc',
#   skip_empty_rows = TRUE,
# ) |> 
#   # prepare the data
#   janitor::clean_names()
# 
# df4 <- vroom::vroom(
#   file = files$value[51:54],
#   delim = ',',
#   col_types = 'cccccccc',
#   skip_empty_rows = TRUE,
# ) |> 
#   # prepare the data
#   janitor::clean_names()
# 
# df5 <- vroom::vroom(
#   file = files$value[55:56],
#   delim = ',',
#   col_types = 'cccccccc',
#   skip_empty_rows = TRUE,
# ) |> 
#   # prepare the data
#   janitor::clean_names()
# 
# df5 <- vroom::vroom(
#   file = files$value[56:60],
#   delim = ',',
#   col_types = 'cccccccc',
#   skip_empty_rows = TRUE,
# ) |> 
#   # prepare the data
#   janitor::clean_names()
# 
# test <- vroom::vroom(
#   file = files$value[4],
#   delim = ',',
#   col_types = vroom::cols(.default = "c"),
#   skip_empty_rows = TRUE
# )
# 
# test
# 
# purrr::walk(
#   .x = files$value,
#   .f = \(.x) convert_csv_to_rds_v2(file_path = .x)
# )
# 
# PolyPatEx::fixCSV(
#   file = files$value[4]
# )
# 
# test <- read.csv(
#   file = files$value[4],
#   blank.lines.skip = TRUE,
#   colClasses = 'character'
# )

## using function --------------------------------------------------------------
gp_pop_by_month <- get_gp_populations(
  month_from = '2024-07-01',
  month_to = '2025-01-01',
  files = c(
    'gp-reg-pat-prac-sing-age-female.zip', 'gp-reg-pat-prac-sing-age-male.zip',
    'gp-reg-pat-prac-sing-age-female.csv', 'gp-reg-pat-prac-sing-age-male.csv'
  )
)

gp_pop_by_month <- read_rds_files(folder_to_search = here::here('data', 'gp_pop', 'rds'))

# check months
gp_pop_by_month |> 
  count(extract_date)

# save this to file
saveRDS(
  object = gp_pop_by_month,
  file = here::here('data', 'gp_pop', 'gp_pop_by_month.RDS')
)

## step-by-step ----------------------------------------------------------------
# list urls to search
urls <- list_out_urls_to_search(month_from = '2020-03-01', month_to = '2024-06-01')

# get links from these urls
urls_tofollow <-  
  purrr::map2_dfr(
    .x = urls$url,
    .y = urls$month,
    .f = \(.x, .y) get_links_from_url(url = .x, month = .y)
  )

# limit urls to .zip files
urls_tofollow <- 
  urls_tofollow |> 
  identify_zip_links() |> 
  # limit further to files of interest
  dplyr::filter(
    file %in% c('gp-reg-pat-prac-sing-age-female.zip', 'gp-reg-pat-prac-sing-age-male.zip')
  )

# 

# download these files
gp_pop_by_age <- urls_tofollow$value |> 
  purrr::map_dfr(
    .f = \(.x) download_and_read_zip_file(url = .x)
  )

# summarise the 18+ years population by practice and extract date
gp_pop_by_age <-
  gp_pop_by_age |>
  janitor::clean_names() |> 
  dplyr::filter(
    age %in% c(as.character(18:94), '95+')
  ) |> 
  dplyr::summarise(
    number_of_patients = sum(number_of_patients, na.rm = T),
    .by = c(org_code, extract_date)
  ) |> 
  dplyr::arrange(org_code, extract_date)



# summarise the data
df <- readRDS(file = here::here('data', 'gp_pop', 'gp_pop_by_age_gender.Rds'))

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
    
    # people aged ? 55+ plus
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
