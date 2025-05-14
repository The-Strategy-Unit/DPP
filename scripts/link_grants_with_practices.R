#' -----------------------------------------------------------------------------
#' LINK GRANTS TO PRACTICE | PCN | NHS REGION
#' 
#' DPP grants come in a variety of geographical footprints, ranging from PCN
#' to as vague as whole NHS regions
#' 
#' This script attempts to programmatically associate grants with GP practices
#' so that counterfactuals can be identified.
#' -----------------------------------------------------------------------------


# libraries ----
library(tidyverse)
library(readxl)
library(here)

# read the files ----

## practice | pcn | region ----
get_practice_geo_data <- function() {
  df_geo <- vroom::vroom(
    file = here('data', 'general', 'intervention_counterfac_lu.csv')
  )
}

## grant details ----
get_grant_data <- function(df_geo) {
  df_grants <- read_xlsx(
    path = here('data', 'general', 'grants.xlsx')
  ) |> 
    janitor::clean_names() |> 
    #filter(!is.na(grant_id)) |> # this may exclude some genuine grants
    filter(
      !is.na(internal_dsuk_project_lead),
      #!is.na(initiative_commencement)
    ) |> 
    rename(
      initiative_title = initiative_title_replication_of_titles_highlighted,
      activity_update = activity_intervention_and_progress_update_120124
    ) |> 
    mutate(
      
      # when did the project start
      calc_start = dplyr::if_else(
        condition = nchar(initiative_commencement) == 5,
        true = initiative_commencement |>
          as.integer() |> 
          base::as.Date(origin = '1899-12-30', optional = T),
        false = initiative_commencement |> 
          lubridate::dmy()
      ),
      
      # create a unique grant_id where one doesn't exist
      pseudo_id_p1 = stringr::str_extract(
        string = initative_nhs_contact_email_address, 
        pattern = "(\\w+)@"
      ),
      pseudo_id_p2 = commited_initiative_costs |>
        trimws() |> 
        stringr::str_replace(pattern = "£", replacement = "") |> 
        stringr::str_replace(pattern = ",", replacement = "") |> 
        base::as.numeric() |> 
        magrittr::divide_by(1000L) |> 
        base::floor(),
      grant_id = dplyr::coalesce(
        grant_id,
        glue::glue('pseudo_{pseudo_id_p1}_{pseudo_id_p2}')
      ),
      
      calc_end = estimated_initiative_completion_date |> 
        as.integer() |> 
        as.Date(origin = '1899-12-30', optional = T),
      
      # standardise geographies 
      calc_org = other_organisation_type_free_text |> 
        str_to_upper(),
      
      calc_pcn = pcn_if_applicable |> 
        str_to_upper() |> 
        na_if(y = 'MULTIPLE'),
      
      calc_icb = icb_area_if_applicable |> 
        str_to_upper() |> 
        na_if(y = 'NATIONAL'),
      
      calc_region = nhs_region |> 
        str_to_upper(),
      
      # flag organisations
      flag_hospital = case_when(
        str_detect(string = calc_org, pattern = 'HOSPITAL|TRUST|HEART|NHS FOUNDATION') ~ T,
        .default = F
      ),
      
      flag_practice = case_when(
        str_detect(string = initiative_title, pattern = 'Surgery|surgery|Practice') ~ T,
        calc_org %in% df_geo$PRACTICE_NAME ~ T,
        grant_id == '2023-188' ~ T,
        grant_id == '2023-144' ~ T,
        .default = F
      ),
      
      flag_pcn = case_when(
        str_detect(string = calc_org, pattern = 'PCN') & !str_detect(string = calc_org, pattern = 'FEDERATION') ~ T,
        str_detect(string = toupper(activity_update), pattern = 'PCN|PRIMARY CARE NETWORK') ~ T,
        calc_org %in% df_geo$PCN_NAME ~ T,
        grant_id == '2023-115' ~ T,
        grant_id == '2023-119' ~ T,
        grant_id == '2023-092' ~ T,
        grant_id == '2023-194' ~ T,
        .default = F
      ),
      
      flag_subicb = case_when(
        grant_id == '2023-030' ~ T,
        grant_id == '2023-065' ~ T,
        grant_id == '2023-089' ~ T,
        grant_id == '2023-151' ~ T,
        .default = F
      ),
      
      flag_icb = case_when(
        str_detect(string = calc_org, pattern = 'ICB|FEDERATION') ~ T, # NB, assuming federations cover most / all ICB
        str_detect(string = toupper(activity_update), pattern = 'INTEGRATED CARE SYSTEM|INTEGRATED CARE BOARD') ~ T,
        grant_id == '2023-065' ~ T,
        grant_id == '2023-177' ~ T,
        grant_id == '2023-066' ~ T,
        grant_id == '2023-064' ~ T,
        grant_id == '2023-185' ~ T,
        .default = F
      ),
      
      flag_region = case_when(
        calc_region %in% df_geo$COMM_REGION_NAME ~ T,
        .default = F
      ),
      
      flag_exception = case_when(
        other_organisation_type_free_text %in% c(
          'Third Party Clinical Pharmacy - Interface Clinical Services'
        ) ~ T,
        grant_id == '2023-041' ~ T,
        #grant_id == '2023-098' ~ T, # our health partnership, collection of PCNs
        .default = F
      ),
      
      flag_ignore = case_when(
        icb_area_if_applicable == 'National' ~ T,
        .default = F
      ),
      
      calc_exception = case_when(
        # practices
        calc_pcn == 'AEGIS HEALTHCARE PCN' ~ 'WILLOWS HEALTH',
        calc_org == 'WORDSLEY GREEN SURGERY' ~ 'WORDSLEY GREEN HEALTH CENTRE',
        calc_org == 'LONGFORD MEDICAL CENTRE' ~ 'LONGFORD PRIMARY CARE CENTRE',
        calc_org == 'WOODSTOCK BOWER SURGERY' ~ 'WOODSTOCK BOWER GROUP PRACTICE',
        calc_pcn == 'SOUND PCN' ~ 'FRIARY HOUSE SURGERY',
        calc_org == 'WICKHAM GROUP SURGERY' ~ 'WICKHAM SURGERY',
        calc_org == 'BOSVENA HEALTH PRACTICE' ~ 'BOSVENA HEALTH',
        grant_id == '2023-188' ~ 'BRIGHTON STATION HEALTH CENTRE',
        grant_id == '2023-144' ~ 'ALRESFORD SURGERY',
        
        # pcn
        calc_org == 'MODALITY LLP' ~ 'SWB MODALITY PCN',
        calc_org == 'WYRE FOREST HEALTH PARTNERSHIP' ~ 'WYRE FOREST HEALTH PARTNERSHIP PCN',
        calc_pcn == 'WAKEFIELD HEALTH ALLIANCE CENTRAL PCN' ~ 'FIVE TOWNS PCN',
        calc_pcn == 'MALVERN TOWN PCN' ~ 'SOUTH WORCESTERSHIRE MALVERN TOWN PCN',
        grant_id == '2023-109' ~ 'COVENTRY NAVIGATION 1 PCN',
        grant_id == '2320-075' ~ 'PIONEERS INTEGRATED PARTNERSHIP PCN',
        grant_id == '2023-095' ~ 'REDDITCH & BROMSGROVE & DISTRICT PCN',
        grant_id == '2023-130' ~ "DEAN'S AND CENTRAL BRIGHTON PCN",
        grant_id == '2023-125' ~ 'SOUTH WORCESTERSHIRE MALVERN TOWN PCN',
        grant_id == '2023-128' ~ 'WYRE FOREST HEALTH PARTNERSHIP PCN',
        grant_id == '2023-131' ~ 'SOUTH WORCS WORCESTER CITY GP PCN',
        grant_id == '2023-133' ~ 'REDDITCH & BROMSGROVE KINGFISHER PCN',
        #grant_id == '2023-041' ~ 'WAKEFIELD HEALTH ALLIANCE SOUTH PCN', # moved to sub-icb link
        grant_id == '2023-115' ~ 'VIADUCT CARE PCN',
        grant_id == '2023-119' ~ 'SOUTH WORCS DROITWITCH & OMBERSLEY PCN',
        grant_id == '2023-092' ~ 'WALSALL NORTH PCN',
        grant_id == '2023-194' ~ 'WEYMOUTH & PORTLAND PCN',
        
        # sub-icb / ccg
        grant_id == '2023-030' ~ 'NHS Cheshire and Merseyside ICB - 02E',
        grant_id == '2023-065' ~ 'NHS Bedfordshire, Luton and Milton Keynes ICB - M1J4Y',
        grant_id == '2023-089' ~ 'NHS Derby and Derbyshire ICB - 15M',
        grant_id == '2023-151' ~ 'NHS Nottingham and Nottinghamshire ICB - 52R',
        grant_id == '2023-041' ~ 'NHS West Yorkshire ICB - 03R',
        
        # icb
        calc_org == 'YORKSHIRE & HUMBER ACADEMIC HEALTH SCIENCE NETWORK' ~ 'NHS West Yorkshire Integrated Care Board',
        calc_org == 'HEALTH INNOVATION NETWORK SOUTH LONDON' ~ 'NHS South East London Integrated Care Board',
        calc_org == 'MID HAMPSHIRE HEALTHCARE LIMITED' ~ 'NHS Hampshire and Isle of Wight Integrated Care Board',
        calc_org == 'PRIMARY CARE WARWICKSHIRE' ~ 'NHS Coventry and Warwickshire Integrated Care Board',
        initiative_title == 'AF detection and development of pathways for AF management and anticoagulation' ~ 'NHS South East London Integrated Care Board',
        initiative_title == 'Improving cardiovascular outcomes across Leicester, Leicestershire and Rutland (LLR)' ~ 'NHS Leicester, Leicestershire and Rutland Integrated Care Board',
        initiative_title == 'AF detection screening programme' ~ 'NHS Derby and Derbyshire Integrated Care Board',
        grant_id == '2023-065' ~ 'NHS Bedfordshire, Luton and Milton Keynes Integrated Care Board',
        grant_id == '2023-052' ~ 'NHS North Central London Integrated Care Board',
        grant_id == '2023-167' ~ 'NHS North Central London Integrated Care Board',
        grant_id == '2023-047' ~ 'NHS North Central London Integrated Care Board',
        grant_id == '2023-088' ~ 'NHS Greater Manchester Integrated Care Board',
        grant_id == '2023-146' ~ 'NHS Bristol, North Somerset and South Gloucestershire Integrated Care Board',
        grant_id == '2023-177' ~ 'NHS Birmingham and Solihull Integrated Care Board',
        grant_id == '2023-066' ~ 'NHS Suffolk and North East Essex Integrated Care Board',
        grant_id == '2023-064' ~ 'NHS Leicester, Leicestershire and Rutland Integrated Care Board',
        grant_id == '2023-185' ~ 'NHS North Central London Integrated Care Board'
        
      ),
      
      # parse cost
      calc_cost = commited_initiative_costs |> 
        str_remove_all(pattern = '£|,') |> 
        as.numeric(),
      
      calc_cost_top_10pc = calc_cost >= quantile(calc_cost, probs = 0.9),
      
      # decide what type of link to make
      calc_link = case_when(
        # handle exceptions
        flag_exception ~ 'Exception TBC',
        
        # then sub-icbs
        flag_subicb ~ 'Sub-ICB',
        
        # start with known PCNs
        flag_pcn ~ 'PCN',
        
        # then go with practices
        flag_practice ~ 'Practice',
        
        # then icbs
        flag_icb ~ 'ICB',
        # national initiatives
        flag_ignore ~ 'National',
        # regions
        flag_region ~ 'Regional',
        # hospitals
        flag_hospital ~ 'Hospital',
        # now go with PCNs where they are valid
        calc_pcn %in% df_geo$PCN_NAME ~ 'PCN',
        calc_exception %in% df_geo$PCN_NAME ~ 'PCN',
        # now go with single sites
        calc_org %in% df_geo$PRACTICE_NAME ~ 'Practice',
        calc_exception %in% df_geo$PRACTICE_NAME ~ 'Practice',
        # now icbs
        calc_exception %in% df_geo$ICB_NAME ~ 'ICB',
        # default to unassigned
        .default = 'Unassigned'
      ) |> 
        as_factor() |> 
        fct_infreq()
    )
}

# linking data ----
## linking grants with practices ----
link_grants_with_geo <- function(df_grants, df_geo) {
  df_geo_lookup <- bind_rows(
    # practices
    df_grants |>
      filter(calc_link == 'Practice') |>
      mutate(
        temp_practice_name = coalesce(calc_exception, calc_org),
        temp_pcn_name = coalesce(calc_pcn, calc_exception),
        temp_icb_name = coalesce(icb_area_if_applicable, calc_exception)
      ) |>
      select(grant_id, temp_practice_name, temp_pcn_name, temp_icb_name) |>
      left_join(
        y = df_geo |>
          select(PRACTICE_CODE, PRACTICE_NAME, PCN_NAME, ICB_NAME, SUB_ICB_LOCATION_NAME),
        by = c('temp_practice_name' = 'PRACTICE_NAME'),
        keep = T
      ) |>
      mutate(
        method = 'Individual practice',
        score =
          coalesce(temp_pcn_name == PCN_NAME, 0) +
          coalesce(temp_icb_name == ICB_NAME, 0)
      ) |> 
      slice_max(score, n = 1, by = grant_id),
    
    # pcns
    df_grants |>
      filter(calc_link == 'PCN') |>
      mutate(
        temp_pcn_name = coalesce(calc_exception, calc_pcn),
        temp_icb_name = coalesce(icb_area_if_applicable, calc_exception)
      ) |>
      select(grant_id, temp_pcn_name, temp_icb_name) |>
      left_join(
        y = df_geo |>
          select(PRACTICE_CODE, PRACTICE_NAME, PCN_NAME, ICB_NAME, SUB_ICB_LOCATION_NAME),
        by = c('temp_pcn_name' = 'PCN_NAME'),
        relationship = 'many-to-many',
        keep = T
      ) |>
      mutate(
        method = 'Primary Care Network',
        score = coalesce(temp_icb_name == ICB_NAME, 0)
      ),
    
    # icb
    df_grants |>
      filter(calc_link == 'ICB') |> 
      mutate(
        temp_icb_name = coalesce(calc_exception, icb_area_if_applicable)
      ) |> 
      select(grant_id, temp_icb_name) |> 
      left_join(
        y = df_geo |> 
          select(PRACTICE_CODE, PRACTICE_NAME, PCN_NAME, ICB_NAME, SUB_ICB_LOCATION_NAME),
        by = c('temp_icb_name' = 'ICB_NAME'),
        relationship = 'many-to-many',
        keep = T
      ) |> 
      mutate(
        method = 'Integrated Care Board',
        score = 1
      ),
    
    # hospital
    df_grants |> 
      filter(calc_link == 'Hospital') |> 
      mutate(
        temp_icb_name = coalesce(calc_exception, icb_area_if_applicable)
      ) |> 
      select(grant_id, temp_icb_name) |> 
      left_join(
        y = df_geo |> 
          select(PRACTICE_CODE, PRACTICE_NAME, PCN_NAME, ICB_NAME, SUB_ICB_LOCATION_NAME),
        by = c('temp_icb_name' = 'ICB_NAME'),
        relationship = 'many-to-many',
        keep = T
      ) |> 
      mutate(
        method = 'Hospital',
        score = 1
      ),
    
    # sub-icb
    df_grants |> 
      filter(calc_link == 'Sub-ICB') |> 
      mutate(
        temp_subicb_name = coalesce(calc_exception)
      ) |> 
      select(grant_id, temp_subicb_name) |> 
      left_join(
        y = df_geo |> 
          select(PRACTICE_CODE, PRACTICE_NAME, PCN_NAME, ICB_NAME, SUB_ICB_LOCATION_NAME),
        by = c('temp_subicb_name' = 'SUB_ICB_LOCATION_NAME'),
        relationship = 'many-to-many',
        keep = T
      ) |> 
      mutate(
        method = 'Sub-ICB',
        score = 1
      ),
    
  ) |> 
    mutate(method = method |> fct() |> fct_infreq())
}


# main -----
# get some data
df_geo <- get_practice_geo_data()
df_grants <- get_grant_data(df_geo = df_geo)
df_geo_lookup <- link_grants_with_geo(df_grants, df_geo)

saveRDS(
  object = df_grants,
  file = here::here('data', 'general', 'df_grants.Rds')
)

# compile a list of PCN changes to make
df_manual_changes_pcn <- 
  tibble::tribble(
    ~grant_id,   ~temp_pcn_name,
    
    # Clair Huckerby - Our Health Partnership - this list comes from their website
    # https://www.ourhealthpartnership.com/services/
    '2023-098',  'ALLIANCE OF SUTTON PRACTICES PCN',
    '2023-098',  'BOURNVILLE AND NORTHFIELD PCN',
    '2023-098',  'KINGSTANDING, ERDINGTON & NECHELLS PCN',
    '2023-098',  'MOSELEY, BILLESLEY & YARDLEY WOOD PCN',
    '2023-098',  'QUINTON AND HARBORNE PCN',
    '2023-098',  'SHARD END AND KITTS GREEN PCN',
    '2023-098',  'SOUTH BIRMINGHAM ALLIANCE PCN',
    '2023-098',  'SE SHROPSHIRE PCN',
    '2023-098',  'SW SHROPSHIRE PCN',
    '2023-098',  'WEOLEY AND RUBERY PCN',
    
    # M Kahn - Wakefield palpitation service
    # informed in chat that cover CCG (sub-ICB region)
    '2023-041',  'BRIGANTES PCN',
    '2023-041',  'FIVE TOWNS PCN',
    '2023-041',  'PONTEFRACT AND KNOTTINGLEY PCN',
    '2023-041',  'TRINITY HEALTH GROUP PCN',
    '2023-041',  'WAKEFIELD HEALTH ALLIANCE SOUTH PCN',
    '2023-041',  'WAKEFIELD NORTH PCN',
    '2023-041',  'WEST WAKEFIELD PCN',
    
    # Zainab Khanbhai - Hillingdon Confederation
    # read that project covers six of Hillingdon's PCNs
    # also coded with project id 'pseudo_khanbhai@_241'
    '2024-700',  'COLNE UNION PCN',
    '2024-700',  'SYNERGY PCN',
    '2024-700',  'NORTH CONNECT PCN',
    '2024-700',  'HH COLLABORATIVE PCN',
    '2024-700',  'LONG LANE FIRST CARE GROUP PCN',
    '2024-700',  'CELADINE HEALTH & METROCARE PCN',
    
    'pseudo_khanbhai@_241',  'COLNE UNION PCN',
    'pseudo_khanbhai@_241',  'SYNERGY PCN',
    'pseudo_khanbhai@_241',  'NORTH CONNECT PCN',
    'pseudo_khanbhai@_241',  'HH COLLABORATIVE PCN',
    'pseudo_khanbhai@_241',  'LONG LANE FIRST CARE GROUP PCN',
    'pseudo_khanbhai@_241',  'CELADINE HEALTH & METROCARE PCN',
    
    # Remote Detection and Diagnosis of AF project: Thurrock Pilot
    'pseudo_raj@_48', 'ASOP PCN',
    'pseudo_raj@_48', 'GRAYS PCN',
    'pseudo_raj@_48', 'STANFORD-LE-HOPE PCN',
    'pseudo_raj@_48', 'TILBURY AND CHADWELL PCN',
    
    # Remote Detection and Diagnosis of AF project: SPINE Pilot
    'pseudo_nanda@_30', 'SPINE PCN',
    
    # Remote Detection and Diagnosis of AF project: Pershore Upton Pilot
    'pseudo_pferenc@_25', 'SOUTH WORCS PERSHORE & UPTON PCN',
    
    # Detection Sponsorship - Midlands Medicines Partnership
    'pseudo_crossman@_16', 'MMP CENTRAL AND NORTH PCN',
    
    # Detection Sponsorship - Erewash
    'pseudo_gooch2@_16', 'EREWASH PCN',
    
    # Detection Sponsorship - Sheffield
    'pseudo_tsoneva@_11', 'WEST 5 PCN',
    'pseudo_tsoneva@_11', 'GPA1 PCN',
    'pseudo_tsoneva@_11', 'STUDENT & CENTRAL SHEFFIELD PCN',
    'pseudo_tsoneva@_11', 'FOUNDRY PCN',
    'pseudo_tsoneva@_11', 'NETWORK NORTH PCN',
    'pseudo_tsoneva@_11', 'SAPA 5 PCN',
    'pseudo_tsoneva@_11', 'PEAK EDGE PCN',
    'pseudo_tsoneva@_11', 'PORTER VALLEY PCN',
    'pseudo_tsoneva@_11', 'HILLSBOROUGH PCN',
    'pseudo_tsoneva@_11', 'HEELEY PLUS PCN',
    'pseudo_tsoneva@_11', 'TOWNSHIPS 2 PCN',
    'pseudo_tsoneva@_11', 'TOWNSHIPS 1 PCN',
    'pseudo_tsoneva@_11', 'CITY CENTRE AND UNIVERSITY SHU PCN',
    'pseudo_tsoneva@_11', 'SEVEN HILLS PCN',
    'pseudo_tsoneva@_11', 'UPPER DON VALLEY PCN',
    'pseudo_tsoneva@_11', 'UOS STUDENT PCN',
    
    # Detection sponsorship - Doncaster LPC
    'pseudo_nickhunter19@_1', 'DONCASTER NORTH WEST PCN',
    'pseudo_nickhunter19@_1', 'DONCASTER EAST PCN',
    'pseudo_nickhunter19@_1', 'DONCASTER CENTRAL PCN',
    'pseudo_nickhunter19@_1', 'DONCASTER NORTH PCN',
    'pseudo_nickhunter19@_1', 'DONCASTER SOUTH PCN',
    'pseudo_nickhunter19@_1', '4 DONCASTER PCN',
    
    # Future Innovations in Novel Detection of atrial Fibrillation (FIND -AF): A clinical implementation project. Bradford and Wake field
    'pseudo_gale3@_56', 'BRADFORD CITY 5 PCN',
    'pseudo_gale3@_56', 'BRADFORD NORTH WEST PCN',
    'pseudo_gale3@_56', 'NORTH BRADFORD PCN',
    'pseudo_gale3@_56', 'BRADFORD CITY 6 PCN',
    'pseudo_gale3@_56', 'BRADFORD CITY 4 PCN',
    'pseudo_gale3@_56', 'WEST WAKEFIELD PCN',
    'pseudo_gale3@_56', 'WAKEFIELD HEALTH ALLIANCE SOUTH PCN',
    'pseudo_gale3@_56', 'WAKEFIELD NORTH PCN',
    
    # Remote Detection and Diagnosis of AF project: East cornwall Pilot
    'pseudo_kallis@_51', 'EAST CORNWALL PCN',
    
    # Remote Detection and Diagnosis of AF project: SDSmyhealthcare GP Federation Pilot
    'pseudo_dhesi1@_79', 'BALSALL HEATH, SPARKHILL & MOSELEY PCN',
    
    # Leicestershire Primary Care Cardiovascular Hubs
    'pseudo_mashru@_130', 'CROSS COUNTIES PCN',
    'pseudo_mashru@_130', 'NORTH BLABY PCN',
    
    # IIS Detect AF
    'pseudo_NA_74', 'GATESHEAD CENTRAL SOUTH PCN',
    'pseudo_NA_74', 'GATESHEAD EAST PCN',
    'pseudo_NA_74', 'BIRTLEY AND CENTRAL GATESHEAD PCN',
    'pseudo_NA_74', 'GATESHEAD INNER WEST PCN',
    'pseudo_NA_74', 'GATESHEAD OUTER WEST PCN',
    'pseudo_NA_74', 'NEWCASTLE CENTRAL HEALTH PCN',
    'pseudo_NA_74', 'NEWCASTLE INNER WEST PCN',
    'pseudo_NA_74', 'JESMOND - LOWER GOSFORTH PCN',
    'pseudo_NA_74', 'NORTH GOSFORTH PCN',
    'pseudo_NA_74', 'NEWCASTLE EAST PCN',
    'pseudo_NA_74', 'WEST END FAMILY HEALTH PCN',
    'pseudo_NA_74', 'NEWCASTLE OUTER WEST PCN'
    
    
  )

# add this lookup to the grants data
df_manual_changes_pcn <- 
  df_manual_changes_pcn |> 
  dplyr::mutate(method = 'Manual PCN') |> 
  # add in the geographical data
  dplyr::left_join(
    y = df_geo |> 
      select(PRACTICE_CODE, PRACTICE_NAME, PCN_NAME, ICB_NAME, SUB_ICB_LOCATION_NAME),
    by = dplyr::join_by(temp_pcn_name == PCN_NAME),
    relationship = 'many-to-many'
  )

# compile a list of PRACTICE changes to make
df_manual_changes_prac <-
  tibble::tribble(
    ~grant_id,           ~temp_practice_code,
    'pseudo_cottam@_45', 'P81086',
    'pseudo_cottam@_45', 'P81668',
    'pseudo_cottam@_45', 'P81674',
    'pseudo_cottam@_45', 'P81646',
    'pseudo_cottam@_45', 'P81138',
    'pseudo_cottam@_45', 'P81092',
    'pseudo_cottam@_45', 'P81073',
    'pseudo_cottam@_45', 'P81089',
    'pseudo_cottam@_45', 'N82646',
    'pseudo_cottam@_45', 'N82033',
    'pseudo_cottam@_45', 'N82091',
    'pseudo_cottam@_45', 'N82070',
  )

df_manual_changes_prac <- 
  df_manual_changes_prac |> 
  dplyr::mutate(method = 'Manual Practice') |> 
  # add in the geographical data
  dplyr::left_join(
    y = df_geo |> 
      select(PRACTICE_CODE, PRACTICE_NAME, PCN_NAME, ICB_NAME, SUB_ICB_LOCATION_NAME),
    by = dplyr::join_by(temp_practice_code == PRACTICE_CODE)
  )

df_geo_lookup2 <- df_geo_lookup |> 
  # remove any reference to these manually-adjusted grants
  filter(!grant_id %in% c(
    df_manual_changes_pcn$grant_id, 
    df_manual_changes_prac$grant_id)) |> 
  # bind the new data to it
  dplyr::bind_rows(
    df_manual_changes_pcn,
    df_manual_changes_prac
  )


# save for later use
saveRDS(
  object = df_geo_lookup2,
  file = here::here('data', 'project', 'project_practice_lu.Rds')
)

# other processes ----------------------

# find our health partnership
# df_geo_lookup |> 
#   dplyr::filter(grant_id == '2023-041') |> 
#   view()
# 
# df_geo_lookup2 |> 
#   dplyr::filter(grant_id == '2023-041') |> 
#   view()

# get a lookup of gp practice code to intervention
df_gp_intervention <- 
  df_geo_lookup2 |> 
  janitor::clean_names() |> 
  dplyr::select(practice_code, practice_name) |> 
  dplyr::distinct(.keep_all = TRUE) |> 
  dplyr::mutate(flag_intervention = TRUE)

# save for future use
saveRDS(
  object = df_gp_intervention,
  file = here::here('data', 'general', 'intervention_practices.Rds')
)



dplyr::mutate(
  flag_intervention = as.numeric(flag_intervention)
)

# 
# 
# # how many practices are subject to a DPP grant?
# df_geo_lookup |> 
#   summarise(practices = n_distinct(PRACTICE_CODE, na.rm = T))
# 
# # summary of grants by geography
# df_grants |> count(calc_link)
# 
# # any geo_lookups with missing data?
# df_geo_lookup |> 
#   filter(is.na(PRACTICE_CODE)) |> 
#   summarise(
#     grants = n_distinct(grant_id),
#     rows = n()
#   )
# 
# # any grants not found in geo_lookup?
# df_grants |> 
#   filter(!grant_id %in% df_geo_lookup$grant_id) |> 
#   count(calc_link)
# 
# # what is the cost distribution of grants?
# df_grants |> 
#   mutate(calc_cost_group = ntile(calc_cost, 4) |> as.character() |> fct()) |>  
#   ggplot(aes(x = calc_cost, fill = calc_cost_group)) +
#   geom_histogram(binwidth = 6000, position = 'identity', colour = 'white') +
#   scale_x_continuous(labels = scales::label_currency(prefix = '£', scale = 1/1000, suffix = 'k'), n.breaks = 6) +
#   bbplot::bbc_style() +
#   labs(
#     title = 'Most grants are below £75k',
#     caption = 'Binwidth = £6k'
#   ) +
#   theme(
#     legend.position = 'none',
#     plot.caption = element_text()
#   )
# 
# # cost distribution broken by geo
# df_grants |> 
#   mutate(calc_cost_group = ntile(calc_cost, 4) |> as.character() |> fct()) |>  
#   ggplot(aes(x = calc_cost, fill = calc_cost_group)) +
#   geom_histogram(binwidth = 6000, position = 'identity', colour = 'white') +
#   facet_wrap(~calc_link, ncol = 1) +
#   scale_x_continuous(labels = scales::label_currency(prefix = '£', scale = 1/1000, suffix = 'k'), n.breaks = 6) +
#   bbplot::bbc_style() +
#   labs(
#     title = 'Grant value by geography',
#     caption = 'Binwidth = £6k'
#   ) +
#   theme(
#     legend.position = 'none',
#     plot.caption = element_text()
#   )
# 
# # checking coverage (can be hidden afterwards)
# 
# unmatched
# unmatched <- df_grants |>
#   filter(
#     !flag_hospital,
#     !flag_practice,
#     !flag_pcn,
#     !flag_icb,
#     !flag_exception,
#     !flag_ignore
#   )
# # 
# # # hospitals
# # df_grants |> 
# #   filter(flag_hospital) |> 
# #   summarise(across(starts_with('flag_'), sum), total = n())
# # 
# # # practices
# # df_grants |> 
# #   filter(flag_practice) |> 
# #   summarise(across(starts_with('flag_'), sum), total = n())
# # 
# # # pcn
# # df_grants |> 
# #   filter(flag_pcn) |> 
# #   summarise(across(starts_with('flag_'), sum), total = n())
# # 
# # # icb
# # df_grants |> 
# #   filter(flag_icb) |> 
# #   summarise(across(starts_with('flag_'), sum), total = n())
# # 
# # # exceptions
# # df_grants |> 
# #   filter(flag_exception) |> 
# #   summarise(across(starts_with('flag_'), sum), total = n())
# # 
# # # ignore
# # df_grants |> 
# #   filter(flag_ignore) |> 
# #   summarise(across(starts_with('flag_'), sum), total = n())
# # 
# # # not flagged at all
# # df_grants |> 
# #   filter(
# #     !flag_hospital,
# #     !flag_practice,
# #     !flag_pcn,
# #     !flag_icb,
# #     !flag_exception,
# #     !flag_ignore
# #   ) |> 
# #   summarise(across(starts_with('flag_'), sum), total = n())
# 
# 
# # map of icb -----
# library(leaflet)
# library(sf)
# 
# # summarise practices by icb
# icb_summary <- df_geo |> 
#   # add a marker for where involved in DPP
#   left_join(
#     y = df_geo_lookup |> 
#       select(PRACTICE_CODE) |> 
#       distinct() |> 
#       mutate(flag_dpp = T),
#     by = 'PRACTICE_CODE'
#   ) |> 
#   # flag pcn with at least one practice in dpp
#   mutate(
#     flag_pcn_has_dpp_practice = n_distinct(PRACTICE_CODE[flag_dpp], na.rm = T) > 0,
#     .by = PCN_CODE
#   ) |> 
#   summarise(
#     practices_total = n_distinct(PRACTICE_CODE, na.rm = T),
#     practices_dpp = n_distinct(PRACTICE_CODE[flag_dpp], na.rm = T),
#     pcn_total = n_distinct(PCN_CODE, na.rm = T),
#     pcn_dpp = n_distinct(PCN_CODE[flag_pcn_has_dpp_practice], na.rm = T),
#     .by = ONS_ICB_CODE
#   ) |> 
#   mutate(
#     dpp_coverage_practice = practices_dpp / practices_total,
#     dpp_coverage_pcn = pcn_dpp / pcn_total,
#     dpp_colour = na_if(dpp_coverage_practice, 0)
#   )
# 
# # get icb boundary data
# url <- 'https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Integrated_Care_Boards_April_2023_EN_BGC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson'
# icb <- st_read(url)
# 
# # add our details to the boundary
# icb_dpp <- icb |> 
#   left_join(
#     y = icb_summary,
#     by = c('ICB23CD' = 'ONS_ICB_CODE')
#   )
# 
# #bins <- seq(from = 0, to = 1, by = 0.2)
# #bins <- c(-Inf, 0, 0.1, 0.3, 0.5, 1, Inf)
# #pal <- colorBin('YlOrRd', domain = icb_dpp$dpp_coverage, bins = bins)
# #pal <- colorBin('Blues', domain = icb_dpp$dpp_colour, bins = bins, na.color = 'white')
# pal <- colorBin('Blues', domain = icb_dpp$dpp_colour, na.color = 'white')
# labels <- glue::glue(
#   '<strong>{str_replace(string = icb_dpp$ICB23NM, "Integrated Care Board", "ICB")}</strong><br/>',
#   'DPP practice coverage: {scales::percent(icb_dpp$dpp_coverage_practice, accuracy = 1)} ({icb_dpp$practices_dpp} of {icb_dpp$practices_total} practices)<br/>',
#   'DPP PCN coverage: {scales::percent(icb_dpp$dpp_coverage_pcn, accuracy = 1)} ({icb_dpp$pcn_dpp} of {icb_dpp$pcn_total} PCNs)'
# ) |> 
#   lapply(htmltools::HTML)
# 
# icb_dpp |> 
#   leaflet() |> 
#   addPolygons(
#     fillColor = ~pal(dpp_colour),
#     weight = 1, 
#     opacity = 1,
#     color = 'white',
#     fillOpacity = 0.6,
#     highlightOptions = highlightOptions(
#       weight = 3,
#       fillOpacity = 0.8,
#       bringToFront = T
#     ),
#     label = labels,
#     labelOptions = labelOptions(
#       style = list('font-weight' = 'normal', padding = '3px 8px'),
#       textsize = '15px',
#       direction = 'auto'
#     )
#   ) |> 
#   addLegend(
#     pal = pal,
#     values = ~dpp_coverage_practice,
#     opacity = 0.7,
#     title = NULL
#   ) |> 
#   addProviderTiles(providers$Stadia.AlidadeSmooth)
#   
