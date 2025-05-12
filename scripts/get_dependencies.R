#' -----------------------------------------------------------------------------
#' LIST DEPENDENCIES
#' 
#' Create a list of packages used in this project.
#' -----------------------------------------------------------------------------

# gather a list of dependencies in the project and where they were used
df_pak_used_details <-
  renv::dependencies() |>
  janitor::clean_names() |>
  dplyr::mutate(
    file = basename(source) |> factor()
  )

# summarise to a list of packages (alphabetical)
df_pak_summary <-
  df_pak_used_details |> 
  dplyr::select(package) |> 
  dplyr::distinct() |> 
  dplyr::arrange(package)