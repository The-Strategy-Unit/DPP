#' -----------------------------------------------------------------------------
#' LINK GP WITH HOSPITALS
#' 
#' There are some DPP projects which are hospital focussed. These are currently
#' handled in `link_grants_with_practices.R` by assuming hospital catchment
#' areas cover their ICB.
#' 
#' This assumption may not be accurate, or, at least mis-represents the number
#' of GP practices affected by a hospital-based project.
#' 
#' This script attempts to link GP practices with hospitals to allow for a more
#' rational and realistic view of the scope of DPP projects.
#' 
#' It is based on the work of Office for Health Improvement and Disparities 
#' (OHID) who compiled a catchment area of hospitals based on attendances, split
#' by type - e.g. Outpatient, Inpatient, Emergency.
#' 
#' Site where I found the link:
#' https://www.eastsussexjsna.org.uk/resources/nhs-acute-hospital-trust-catchment-populations/
#' 
#' PowerBI dashboard:
#' https://app.powerbi.com/view?r=eyJrIjoiODZmNGQ0YzItZDAwZi00MzFiLWE4NzAtMzVmNTUwMThmMTVlIiwidCI6ImVlNGUxNDk5LTRhMzUtNGIyZS1hZDQ3LTVmM2NmOWRlODY2NiIsImMiOjh9
#' 
#' Source to download data
#' https://app.box.com/s/qh8gzpzeo1firv1ezfxx2e6c4tgtrudl
#' 
#' Postcode to MSOA lookup:
#' https://open-geography-portalx-ons.hub.arcgis.com/datasets/ons::postcode-to-oa-2021-to-lsoa-to-msoa-to-lad-november-2024-best-fit-lookup-in-the-uk/about
#' 
#' 
#' I used the project typology document to do the original geo-assignment of
#' practices to projects. The result of this is the file:
#' /data/project/project_practice_lu.Rds
#' 
#' I did a search 
#' -----------------------------------------------------------------------------

# find hospital-based projects
df_dpp_prac_temp <- readRDS(
  file = here::here('data', 'project', 'project_practice_lu.Rds')
) |> 
  dplyr::filter(method == 'Hospital') |> 
  dplyr::select(grant_id, method) |> 
  dplyr::distinct() |> 
  dplyr::arrange(grant_id)

# I searched for these grant_ids in the SharePoint folder for this project.

# I then manually reviewed any documents, ideally grant applications to identify
# whether the project could have their GP practices assigned in a 'better' way,
# e.g. whether the hospital project focussed on:
# - emergency admissions (e.g. post-stroke / tia care),
# - elective admissions (e.g. AF detection clinics),
# - or require a manual identification of participating GP practices.

# here's my result (2025-03-31):
#'    grant_id     n	Trust
#'    <glue>   <int>	
#'    1 2023-016   197	Exclude - national	Royal College of Physicians - education programme with no defined geography.
#'    2 2023-045   178	Exclude - not AF focussed. PCN or GP practice based - emailed Dr De (pde@hotmail.co.uk) for guidance. Dr De called back - was unwilling to give details of specific PCNs or practices - all of Sandwell and Birmingham
#'    3 2023-048   178	Leave as is - University Hospitals Birmingham / Heartlands Hospital / "3 hospital sites", GP referral base.
#'    4 2023-068   113	Leave as is - expected benefits at ICS level. University of Derby and Burton NHS FT, establishing a lipid clinic, post stroke.
#'    5 2023-070   265	Leave as is. Leeds Teaching Hospitals - AF screening in HF clinics.
#'    6 2023-088   408	Leave as is. Manchester Univeristy NHS FT. Lipid clinic
#'    7 2023-090   178*	University Hospital Birmingham. Queen Elizabeth Hospital. A&E based ? emergency admissions
#'    8 2023-112   120	Leave as is - expected benefits at ICS level. Coventry and Warwickshire - AF clinic
#'    9 2023-146    75	Leave as is - expected benefits at ICS level. North Bristol NHS Trust.
#'    10 2023-162   345*	Chelsea and Westminster NHS FT. Borough of Houslow. Hounslow Health PCN
#'    11 2023-189    69*	Dorset County Hospital NHS FT. TIA clinic (?inpatient admission route)
#'    12 2023-226   120*	University Hospitals Coventry and Warwickshire. Pharmacist-led clinic in outpatient and primary care. Looking at groups post ACS (acute coronary syndrome)
#'    13 2023-232   340*	Halton CCG. They mentioned 7 of 14 practices participated. emailed the leads for additional information. They replied back - Fir Park Medical Centre, Murdishaw Health Centre, Oaks Place Surgery, Grove House Practice, The Beeches Medical Centre, Bevan Group Practice, Castlefields Health Centre.
#'    14 2024-003    69	Dorset County Hospital NHS FT - improving AF clinic. ? Elective
#'    15 2024-011   142	Leave as is. North Staffordshire Combined Healthcare NHS Trust, Very specialised - AF in people with severe mental illness. This is a mental health Trust so not part of the hospital catchment area, so can't use the catchment data. There are educational aspects which are offered to clinicians in the Trust as well as 'neighbouring Midlands Partnership Foundation Trust who provide mental healthcare across Staffordshire'.

# The starred ones are those where I think we should change the practice identification
# methodology


# prepare the data as RDS files ------------------------------------------------
# NB, don't need to run this section each time.
run_again <- FALSE
if (run_again == TRUE) {
  
  # Hospital catchment data -------------------------
  # load the Excel file
  df_trust_catchment_emergency <- readxl::read_excel(
    path = here::here('data', 'hospital_geo', '2022 Trust Catchment Populations_Supplementary MSOA Analysis.xlsx'),
    sheet = 'Emergency'
  )
  
  df_trust_catchment_elective <- readxl::read_excel(
    path = here::here('data', 'hospital_geo', '2022 Trust Catchment Populations_Supplementary MSOA Analysis.xlsx'),
    sheet = 'Elective'
  )
  
  df_trust_catchment_alladmissions <- readxl::read_excel(
    path = here::here('data', 'hospital_geo', '2022 Trust Catchment Populations_Supplementary MSOA Analysis.xlsx'),
    sheet = 'All Admissions'
  )
  
  # save for future use
  saveRDS(
    object = df_trust_catchment_emergency,
    file = here::here('data', 'hospital_geo', 'emergency_admissions.Rds')
  )
  
  saveRDS(
    object = df_trust_catchment_elective,
    file = here::here('data', 'hospital_geo', 'elective_admissions.Rds')
  )
  
  saveRDS(
    object = df_trust_catchment_alladmissions,
    file = here::here('data', 'hospital_geo', 'all_admissions.Rds')
  )
  
  # Postcode to MSOA lookup -----------------------------
  # load the CSV
  df_postcode_lu <- vroom::vroom(
    file = here::here('data', 'hospital_geo', 'PCD_OA21_LSOA21_MSOA21_LAD_NOV24_UK_LU.csv'),
    delim = ',',
    col_types = 'c'
  )
  saveRDS(
    object = df_postcode_lu,
    file = here::here('data', 'hospital_geo', 'postcode_lu.Rds')
  )
}

# read the hospital catchment data ---------------------------------------------
df_trust_emergency <- readRDS(
  file = here::here('data', 'hospital_geo', 'emergency_admissions.Rds')
) |> janitor::clean_names()
df_trust_elective <- readRDS(
  file = here::here('data', 'hospital_geo', 'elective_admissions.Rds')
) |> janitor::clean_names()
df_trust_alladmissions <- readRDS(
  file = here::here('data', 'hospital_geo', 'all_admissions.Rds')
) |> janitor::clean_names()

# read the postcode lookup data (includes MSOA code)
df_postcode_lu <- readRDS(
  file = here::here('data', 'hospital_geo', 'postcode_lu.Rds')
) |> janitor::clean_names()

# read the practice matching data
df_matching <- readRDS(file = here::here('data', 'matching', 'df_matching_imputed.Rds'))

# read the practice geography lookup file
df_geo <- vroom::vroom(
  file = here::here('data', 'general', 'intervention_counterfac_lu.csv')
) |> 
  janitor::clean_names()

# main -------------------------------------------------------------------------

# get a list of GP practices and postcodes then link with msoa data
df_gp_msoa <-
  df_matching |> 
  dplyr::select(prac_code, practice_name, practice_postcode) |> 
  dplyr::distinct() |> 
  dplyr::mutate(
    practice_postcode = practice_postcode |> 
      stringr::str_remove(" ")
  ) |> 
  # add in msoa via postcode
  dplyr::left_join(
    y = df_postcode_lu |> 
      dplyr::select(pcds, msoa21cd) |> 
      dplyr::mutate(
        postcode = pcds |> 
          stringr::str_trim() |> 
          stringr::str_remove(" ")
      ) |> 
      dplyr::select(-pcds) |> 
      dplyr::distinct(),
    by = dplyr::join_by(practice_postcode == postcode)
  )

## P9 (2023-052) ---------------------------------------------------------------
# North Middlesex following post TIA (so treating as emergency)
df_p9 <-
  df_trust_emergency |> 
  dplyr::mutate(
    project_id = 'P9',
    project_code = '2023-052',
    method = 'Hospital - emergency admissions MSOA-to-GP practice'
  ) |> 
  dplyr::filter(
    # limit to North Middlesex Trust
    trust_code == 'RAP',
    # limit to first past the post MSOAS, i.e. where the hospital is the dominant provider
    fptp == TRUE,
    # limit to latest 3 years
    catchment_year >= df_trust_emergency |>
      dplyr::select(catchment_year) |> 
      dplyr::distinct() |> 
      dplyr::slice_max(order_by = catchment_year, n = 3) |> 
      dplyr::pull(catchment_year) |> 
      min()
  ) |> 
  # get the msoa codes
  dplyr::select(
    project_id, 
    project_code, 
    trust_code, 
    trust_name, 
    msoa, 
    method) |> 
  dplyr::distinct() |> 
  dplyr::inner_join(
    y = df_gp_msoa,
    by = dplyr::join_by(msoa == msoa21cd)
  ) 

# save this lookup for later use
saveRDS(
  object = df_p9,
  file = here::here('data', 'hospital_geo', 'p9_gp_practices.Rds')
)

# alternative - using elective admissions
df_p9_alt <-
  df_trust_elective |> 
  dplyr::mutate(
    project_id = 'P9',
    project_code = '2023-052',
    method = 'Hospital - elective admissions MSOA-to-GP practice'
  ) |> 
  dplyr::filter(
    # limit to North Middlesex Trust
    trust_code == 'RAP',
    # limit to first past the post MSOAS, i.e. where the hospital is the dominant provider
    fptp == TRUE,
    # limit to latest 3 years
    catchment_year >= df_trust_elective |>
      dplyr::select(catchment_year) |> 
      dplyr::distinct() |> 
      dplyr::slice_max(order_by = catchment_year, n = 3) |> 
      dplyr::pull(catchment_year) |> 
      min()
  ) |> 
  # get the msoa codes
  dplyr::select(
    project_id, 
    project_code, 
    trust_code, 
    trust_name, 
    msoa, 
    method) |> 
  dplyr::distinct() |> 
  dplyr::inner_join(
    y = df_gp_msoa,
    by = dplyr::join_by(msoa == msoa21cd)
  ) 

# save this lookup for later use
saveRDS(
  object = df_p9_alt,
  file = here::here('data', 'hospital_geo', 'p9_alt_gp_practices.Rds')
)


## P12 (2023-047) ---------------------------------------------------------------
# ?
# application form indicates these are training type events open to three ICS (North Central London, North East London and Mid & South Essex)
# It seems the areas where the trial implemented has posters
# ? review posters to identify areas?
# * Colne valley PCN - improving anticoagulation
# Enfield Care Network: Chalfont Surgery - !! kidney focussed
# * Chelmsford City Health PCN
# The Woodberry Practice !! kidney focussed
# * Wentworth Medical Practice, NCL
# Haringey GP Federation !! kidney focussed
# Medicus Health Partners, Enfield Unity PCN !! kidney focussed
# Medicus Health Partners, Enfield Unity PCN !! kidney focussed
# Medicus Health Partners, Enfield Unity PCN !! lipid focussed
# Wanstead and Woodford PCN, North-East London !! lipid focussed
# * Leyton Healthcare - AF prevalance ? not expecting a change
# City Square Medical Group, NEL !! weight loss focussed
# Tulasi Medical Centre, NEL !! kidney focussed

# search for matching practices
# df_geo <- vroom::vroom(
#   file = here::here('data', 'general', 'intervention_counterfac_lu.csv')
# )

df_p12 <-
  tibble::tribble(
    ~prac_code, ~focus,
    # Colne Valley PCN
    'F81011', 'AF - anticoagulation',
    'F81020', 'AF - anticoagulation',
    'F81068', 'AF - anticoagulation',
    'F81119', 'AF - anticoagulation',
    'F81730', 'AF - anticoagulation',
    'Y00293', 'AF - anticoagulation',
    # Enfield Care Network
    'F85023', 'Kidney',
    'F85025', 'Kidney',
    'F85039', 'Kidney',
    'F85072', 'Kidney',
    'F85634', 'Kidney',
    'F85682', 'Kidney',
    'Y03402', 'Kidney',
    # Chelmsford City Health PCN
    'F81040', 'AF - prevalance',
    'F81057', 'AF - prevalance',
    'F81083', 'AF - prevalance',
    'F81114', 'AF - prevalance',
    # The Woodberry Practice
    'F85020', 'Kidney',
    # Wentworth Medical Practice, NCL
    'E83035', 'Hypertension',
    # Haringey GP Federation,
    'F85014', 'Kidney',
    'F85063', 'Kidney',
    'F85688', 'Kidney',
    'Y01655', 'Kidney',
    'F85060', 'Kidney',
    'F85623', 'Kidney',
    'F85705', 'Kidney',
    'Y02117', 'Kidney',
    'Y03135', 'Kidney',
    'F85017', 'Kidney',
    'F85019', 'Kidney',
    'F85028', 'Kidney',
    'F85030', 'Kidney',
    'F85031', 'Kidney',
    'F85034', 'Kidney',
    'F85064', 'Kidney',
    'F85065', 'Kidney',
    'F85066', 'Kidney',
    'F85640', 'Kidney',
    'F85675', 'Kidney',
    'F85061', 'Kidney',
    'F85067', 'Kidney', 
    'F85069', 'Kidney', 
    'Y03035', 'Kidney',
    'F85008', 'Kidney',
    'F85046', 'Kidney',
    'F85669', 'Kidney',
    'F85697', 'Kidney',
    'F85007', 'Kidney',
    'F85013', 'Kidney',
    'F85071', 'Kidney',
    'F85615', 'Kidney',
    'F85628', 'Kidney',
    'Y05330', 'Kidney',
    # Medicus Health Partners
    'F85002', 'Kidney & Lipid',
    # Wanstead and Woodford PCN
    'F86012', 'Lipid',
    'F86013', 'Lipid',
    'F86020', 'Lipid',
    'F86023', 'Lipid',
    'F86032', 'Lipid',
    'F86064', 'Lipid',
    'F86066', 'Lipid',
    'F86641', 'Lipid',
    'F86658', 'Lipid',
    'F86691', 'Lipid',
    'F86731', 'Lipid',
    # Leyton Healthcare
    'F86074', 'AF - prevalence',
    # City Square Medical Group
    'F84114', 'Weight',
    # Tulasi Medical Centre
    'F82660', 'Kidney'
  ) |> 
  dplyr::mutate(method = 'Manual - case study poster review') |> 
  # add in additional details about these practices
  dplyr::left_join(
    y = df_matching |> 
      dplyr::select(prac_code, practice_name, practice_postcode),
    by = dplyr::join_by(prac_code == prac_code)
  )

# save this lookup for later use
saveRDS(
  object = df_p12,
  file = here::here('data', 'hospital_geo', 'p12_gp_practices.Rds')
)

## P16 (2023-167) --------------------------------------------------------------
# North Central London cryptogenic stroke pathway
# UCL, London
df_p16 <-
  df_trust_emergency |> 
  dplyr::mutate(
    project_id = 'P16',
    project_code = '2023-167',
    method = 'Hospital - emergency admissions MSOA-to-GP practice'
  ) |> 
  dplyr::filter(
    # limit to University College London Hospitals NHS Foundation Trust
    trust_code == 'RRV',
    # limit to first past the post MSOAS, i.e. where the hospital is the dominant provider
    fptp == TRUE,
    # limit to latest 3 years
    catchment_year >= df_trust_emergency |>
      dplyr::select(catchment_year) |> 
      dplyr::distinct() |> 
      dplyr::slice_max(order_by = catchment_year, n = 3) |> 
      dplyr::pull(catchment_year) |> 
      min()
  ) |> 
  # get the msoa codes
  dplyr::select(
    project_id, 
    project_code, 
    trust_code, 
    trust_name, 
    msoa, 
    method) |> 
  dplyr::distinct() |> 
  dplyr::inner_join(
    y = df_gp_msoa,
    by = dplyr::join_by(msoa == msoa21cd)
  ) 

# save this lookup for later use
saveRDS(
  object = df_p16,
  file = here::here('data', 'hospital_geo', 'p16_gp_practices.Rds')
)

## Other (2023-090) ------------------------------------------------------------
# ** Non-case study site **
# University Hospital Birmingham. Queen Elizabeth Hospital. 
# A&E based ? emergency admissions

df_2023_090 <-
  df_trust_emergency |> 
  dplyr::mutate(
    project_id = '',
    project_code = '2023-090',
    method = 'Hospital - emergency admissions MSOA-to-GP practice'
  ) |> 
  dplyr::filter(
    # limit to University Hospitals Birmingham NHS Foundation Trust
    trust_code == 'RRK',
    # limit to first past the post MSOAS, i.e. where the hospital is the dominant provider
    fptp == TRUE,
    # limit to latest 3 years
    catchment_year >= df_trust_emergency |>
      dplyr::select(catchment_year) |> 
      dplyr::distinct() |> 
      dplyr::slice_max(order_by = catchment_year, n = 3) |> 
      dplyr::pull(catchment_year) |> 
      min()
  ) |> 
  # get the msoa codes
  dplyr::select(
    project_id, 
    project_code, 
    trust_code, 
    trust_name, 
    msoa, 
    method) |> 
  dplyr::distinct() |> 
  dplyr::inner_join(
    y = df_gp_msoa,
    by = dplyr::join_by(msoa == msoa21cd)
  ) 

# add this lookup to the 'others'
lst_others <- list(df_2023_090)

## Other (2023-162) ------------------------------------------------------------
# ** Non-case study site **
# 2023-162   345*	Chelsea and Westminster NHS FT. Borough of Houslow. 
# Hounslow Health PCN

# This needs assigning via manual PCN

df_2023_162 <-
  df_geo |> 
  dplyr::filter(pcn_name == 'HOUNSLOW HEALTH PCN') |> 
  dplyr::mutate(
    project_id = '',
    project_code = '2023-162',
    method = 'Manual PCN'
  ) |> 
  dplyr::select(
    project_id,
    project_code,
    method,
    prac_code = practice_code,
    practice_name,
    practice_postcode
  )

# add this lookup to the 'others'
lst_others <- c(lst_others, list(df_2023_162)) |> unique()

## Other (2023-189) ------------------------------------------------------------
# ** Non-case study site **
# 2023-189    69*	Dorset County Hospital NHS FT. 
# TIA clinic (?inpatient admission route)

df_2023_189 <-
  df_trust_elective |> 
  dplyr::mutate(
    project_id = '',
    project_code = '2023-189',
    method = 'Hospital - elective admissions MSOA-to-GP practice'
  ) |> 
  dplyr::filter(
    # limit to University Hospitals Birmingham NHS Foundation Trust
    trust_code == 'RBD',
    # limit to first past the post MSOAS, i.e. where the hospital is the dominant provider
    fptp == TRUE,
    # limit to latest 3 years
    catchment_year >= df_trust_elective |>
      dplyr::select(catchment_year) |> 
      dplyr::distinct() |> 
      dplyr::slice_max(order_by = catchment_year, n = 3) |> 
      dplyr::pull(catchment_year) |> 
      min()
  ) |> 
  # get the msoa codes
  dplyr::select(
    project_id, 
    project_code, 
    trust_code, 
    trust_name, 
    msoa, 
    method) |> 
  dplyr::distinct() |> 
  dplyr::inner_join(
    y = df_gp_msoa,
    by = dplyr::join_by(msoa == msoa21cd)
  ) 

# add this lookup to the 'others'
lst_others <- c(lst_others, list(df_2023_189)) |> unique()

## Other (2023-226) ------------------------------------------------------------
# ** Non-case study site **
# 2023-226   120*	University Hospitals Coventry and Warwickshire. 
# Pharmacist-led clinic in outpatient and primary care. 
# Looking at groups post ACS (acute coronary syndrome)

df_2023_226 <-
  df_trust_emergency |> 
  dplyr::mutate(
    project_id = '',
    project_code = '2023-226',
    method = 'Hospital - emergency admissions MSOA-to-GP practice'
  ) |> 
  dplyr::filter(
    # limit to University Hospitals Coventry and Warwickshire NHS Trust
    trust_code == 'RKB',
    # limit to first past the post MSOAS, i.e. where the hospital is the dominant provider
    fptp == TRUE,
    # limit to latest 3 years
    catchment_year >= df_trust_emergency |>
      dplyr::select(catchment_year) |> 
      dplyr::distinct() |> 
      dplyr::slice_max(order_by = catchment_year, n = 3) |> 
      dplyr::pull(catchment_year) |> 
      min()
  ) |> 
  # get the msoa codes
  dplyr::select(
    project_id, 
    project_code, 
    trust_code, 
    trust_name, 
    msoa, 
    method) |> 
  dplyr::distinct() |> 
  dplyr::inner_join(
    y = df_gp_msoa,
    by = dplyr::join_by(msoa == msoa21cd)
  ) 

# add this lookup to the 'others'
lst_others <- c(lst_others, list(df_2023_226)) |> unique()

## Other (2023-232) ------------------------------------------------------------
# ** Non-case study site **
# 2023-232   340*	Halton CCG. They mentioned 7 of 14 practices participated. 
# Emailed the leads for additional information. They replied back - 
# Fir Park Medical Centre, Murdishaw Health Centre, Oaks Place Surgery, 
# Grove House Practice, The Beeches Medical Centre, Bevan Group Practice, 
# Castlefields Health Centre.

# This needs assigning via manual practice

df_2023_232 <-
  df_geo |> 
  dplyr::filter(practice_code %in% c(
    # WIDNES PCN
    'N81035', # FIR PARK MEDICAL CENTRE
    'N81619', # OAKS PLACE SURGERY
    'N81037', # THE BEECHES MEDICAL CTR
    'N81011', # BEVAN GROUP PRACTICE
    # RUNCORN PCN
    'N81072', # MURDISHAW
    'N81066', # GROVE HOUSE PRACTICE
    'N81019' # CASTLEFIELDS HEALTH CENTRE
    )
  ) |> 
  dplyr::mutate(
    project_id = '',
    project_code = '2023-232',
    method = 'Manual GP practice'
  ) |> 
  dplyr::select(
    project_id,
    project_code,
    method,
    prac_code = practice_code,
    practice_name,
    practice_postcode
  )

# add this lookup to the 'others'
lst_others <- c(lst_others, list(df_2023_232)) |> unique()

## Other (2024-003) ------------------------------------------------------------
# 14 2024-003    69*	Dorset County Hospital NHS FT - improving AF clinic. 
# ? Elective

df_2024_003 <-
  df_trust_elective |> 
  dplyr::mutate(
    project_id = '',
    project_code = '2024-003',
    method = 'Hospital - elective admissions MSOA-to-GP practice'
  ) |> 
  dplyr::filter(
    # limit to Dorset County Hospital NHS Foundation Trust
    trust_code == 'RBD',
    # limit to first past the post MSOAS, i.e. where the hospital is the dominant provider
    fptp == TRUE,
    # limit to latest 3 years
    catchment_year >= df_trust_elective |>
      dplyr::select(catchment_year) |> 
      dplyr::distinct() |> 
      dplyr::slice_max(order_by = catchment_year, n = 3) |> 
      dplyr::pull(catchment_year) |> 
      min()
  ) |> 
  # get the msoa codes
  dplyr::select(
    project_id, 
    project_code, 
    trust_code, 
    trust_name, 
    msoa, 
    method) |> 
  dplyr::distinct() |> 
  dplyr::inner_join(
    y = df_gp_msoa,
    by = dplyr::join_by(msoa == msoa21cd)
  ) 

# add this lookup to the 'others'
lst_others <- c(lst_others, list(df_2024_003)) |> unique()

## combine 'others' ------------------------------------------------------------
# create a tibble to hold this 'other' data
df_other_hospitals <-
  dplyr::bind_rows(lst_others)

# save this file for future use
saveRDS(
  object = df_other_hospitals,
  file = here::here('data', 'hospital_geo', 'df_other_hospital_grants.Rds')
)
