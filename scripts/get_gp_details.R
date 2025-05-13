#' -----------------------------------------------------------------------------
#' GET GP PRACTICE DETAILS
#' 
#' Gather some information about GP practices, such as name, postcode
#' and whether active/inactive
#' -----------------------------------------------------------------------------

library(tidyverse)
library(NHSRtools) # provides a wrapper for the ODS API

# get the 'roles' for each organisation in the ODS api
df_roles <- ods_get_roles()

# 'BRANCH SURGERY' appears to the correct term for GP practices
gp_prac_role <-
  df_roles |> 
  dplyr::filter(display_name == 'BRANCH SURGERY') |> 
  pull(id)

# find all organisations with this primary role id
gp_practices <-
  ods_get_organisations(primary_role_id = gp_prac_role)

# save for future use
saveRDS(
  object = gp_practices,
  file = here::here('data', 'ods', 'gp_practice_details.RDS')
)
