#' -----------------------------------------------------------------------------
#' GET THE FINAL LIST OF DPP PROJECTS
#' 
#' Reads a copy of the `DPP interview tracker.xlsx`, processes and saves a
#' copy as an .rds file for later use.
#' 
#' Note:
#' The original copy of the interview tracker is found at:
#' [Teams] > 1234 DPP evaluation > 2. Project delivery > Data collection tools
#' -----------------------------------------------------------------------------

# load the copy of the interview tracker
dpp <- readxl::read_xlsx(
  path = here::here('data', 'project', 'DPP interview tracker.xlsx'),
  sheet = 'DPP evaluation projects final'
)

# process to remove extra rows (for additional interviewees)
dpp <-
  dpp |> 
  janitor::clean_names() |> 
  dplyr::filter(!is.na(project_id)) |> 
  dplyr::select(-dplyr::starts_with('x')) |> 
  dplyr::mutate(
    started_month = zoo::as.yearmon(started_month),
    ended_month = zoo::as.yearmon(ended_month)
  )

# save for future use
saveRDS(
  object = dpp,
  file = here::here('data', 'project', 'ddp_projects.Rds')
)
