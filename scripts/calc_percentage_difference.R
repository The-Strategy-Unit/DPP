#' -----------------------------------------------------------------------------
#' CALCULATE PERCENTAGE DIFFERENCES
#' 
#' For the case studies David asked if I could calculate the percentage changes
#' for intervention and control groups.
#' 
#' These will be used in a sentence like 'In the control group, the prevalence 
#' of GP recorded high-risk AF increased from X% to X% and in the intervention 
#' group it went from X% to X%'
#' 
#' Thoughts:
#' Average pre and post?
#'  - on all data points
#'  - equal number pre and post data points
#' Earliest pre and latest post?
#' Lowest pre and highest post?
#' -----------------------------------------------------------------------------

# libraries
library(zoo)

# grab the data and consolidate to a single df

# list the Rds exports and identify the project from the filename
files <- list.files(
  path = here::here('data', 'project', 'did_data'), 
  full.names = TRUE
) |> 
  tibble::as_tibble() |> 
  dplyr::rename(path = value) |> 
  dplyr::mutate(
    project = basename(path) |> 
      stringr::str_extract(pattern = "p\\d+")
  )

# read the data to a single df
df <- purrr::map2_dfr(
  .x = files$path,
  .y = files$project,
  .f = function(.x, .y) {
    readr::read_rds(file = .x) |> 
      dplyr::mutate(project = .y)
  }
)

# calculate pre- and post- measures for each group
df_summary <-
  df |> 
  dplyr::filter(!is.na(value)) |> 
  dplyr::mutate(
    project = project |> 
      factor(levels = paste0('p', 1:20)) |> 
      forcats::fct_drop()
  ) |> 
  dplyr::mutate(
    treated_time_period_id_project = max(treated_time_period_id, na.rm = TRUE),
    .by = project
  ) |> 
  dplyr::mutate(
    flag_post = dplyr::if_else(
      condition = time_period_id < treated_time_period_id_project,
      true = 0,
      false = 1
    ) |> 
      factor(levels = c(0,1), labels = c("Pre", "Post")),
    flag_intervention = factor(
      x = flag_intervention,
      levels = c(0,1),
      labels = c("Control", "Intervention")
    )
  ) |> 
  dplyr::summarise(
    mean = mean(value, na.rm = TRUE) / 100,
    .by = c(project, indicator_code, indicator_short_name, flag_intervention, flag_post)
  ) |> 
  dplyr::arrange(
    project, indicator_code, flag_intervention
  ) |> 
  tidyr::pivot_wider(
    names_from = flag_post,
    values_from = mean
  ) |> 
  dplyr::mutate(
    change = Post - Pre
  )

# present as {gt} table
tab <- 
  df_summary |> 
  dplyr::group_by(project) |> 
  gt::gt() |> 
  gt::cols_hide(indicator_code) |> 
  gt::fmt_percent(columns = c(Pre, Post, change)) |> 
  gt::cols_label(
    indicator_short_name = "Indicator",
    flag_intervention = "Group",
    change = "Change"
  ) |> 
  gt::cols_align(align = 'left', columns = flag_intervention) |> 
  gt::tab_header(
    title = "Percentage changes",
    subtitle = "Average changes in performance before and after the DPP interventions (March 2022 to September 2024)"
  ) |> 
  gt::tab_footnote(
    locations = gt::cells_column_labels(columns = Pre),
    footnote = "Average (mean) value of all reporting periods prior to the intervention."
  ) |> 
  gt::tab_footnote(
    locations = gt::cells_column_labels(columns = Post),
    footnote = "Average (mean) value of all reporting periods after the intervention."
  ) |> 
  gt::tab_footnote(
    locations = gt::cells_column_labels(columns = change),
    footnote = "Difference in value (Post - Pre)"
  )

# export the tab
tab |> 
  gt::gtsave(
    filename = "percent_change.html", 
    path = here::here('outputs', 'project_reports')
  )
