
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Detect, Protect and Perfect (DPP)

<!-- badges: start -->

<!-- badges: end -->

The [NHS Long Term
Plan](https://www.longtermplan.nhs.uk/areas-of-work/cardiovascular-disease/)
set out the aim of identifying and supporting patients at risk of
stroke. Atrial fibrillation (AF) is a key risk factor for stroke and has
become a focus for transformative interventions.

During 2022/23 and 2023/24, c.£26 million was invested in a funding
programme to expand access to direct oral anticoagulants (DOACs) -
[recommended by
NICE](https://bnf.nice.org.uk/treatment-summaries/arrhythmias/#atrial-fibrillation)
as the most effective treatment for preventing clots that can cause
strokes in patients with AF. The programme had three pillars:

- **Detect:** reduce incidence of stroke by diagnosing more patients
  with AF

- **Protect:** ensure patients diagnosed with AF are offered
  anticoagulation, where clinically appropriate

- **Perfect:** ensure patients with AF are on the correct dose of the
  best value DOAC where clinically appropriate.

## About this repo

All data used in this analysis is within the public domain. Details
regarding funding applications have been withheld.

The recommended approach to re-create the analysis is:

- Install dependencies,

- Obtain matching data,

- Obtain outcome data,

- Run the analysis files.

Details on how to perform each step are provided below.

### Installing dependencies

Run the [`get_dependencies.R`](scripts/get_dependencies.R) script to
identify the packages used within this analysis. NB, this file requires
at least `renv`, `janitor` and `dplyr` to first be installed on your
system to run.

### Matching data

This analysis uses a Propensity Score Matching Differences-in-Difference
(PSM-DiD) approach. In this method, GP practices which are part of the
intervention group are matched with GP practices that are not part of
any DPP project (the control group).

The variables used to match practices are grouped into clinical and
socio-economic factors:

#### Clinical factors

| Matching variable | Rationale | Data source |
|----|----|----|
| Proportion of the practice population aged 65 years or older | The risk of developing AF doubles with each progressive decade and exceeds 20% by age 80 years | [GP registered population data](https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice) by NHS Digital |
| Prevalence of obesity at the practice | Obesity (defined as BMI \>= 30) is an independent risk factor for the development of AF | [Fingertips](https://fingertips.phe.org.uk/) |
| Prevalence of diabetes at the practice | Diabetes mellitus is an independent risk factor for AF, especially in young people | [Fingertips](https://fingertips.phe.org.uk/) |
| Prevalence of hypertension at the practice | People with hypertension have a 1.7-fold higher risk of developing AF compared with people with blood pressure in the normal range | [Fingertips](https://fingertips.phe.org.uk/) |
| Gender proportion | Age-adjusted incidence, prevalence and lifetime risk of AF is higher in men compared with women | [GP registered population data](https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice) by NHS Digital |

#### Socio-economic factors

| Matching variable | Rationale | Data source |
|----|----|----|
| Registered practice population per full time equivalent (FTE) clinician | The number of patients per clinical WTE can be a marker for a range of factors such as funding available to the practice, presence of other clinical staff, clinical needs of the local population and changes in the local population | [GP workforce data](https://digital.nhs.uk/data-and-information/publications/statistical/general-and-personal-medical-services/30-april-2023) by NHS Digital |
| Weighted deprivation score per practice | Greater socio-economic deprivation is associated with poorer outcomes across a range of health issues, including AF | [Fingertips](https://fingertips.phe.org.uk/) |
| Rural-Urban classification of the practice | Distance can be a barrier to accessing healthcare. Rurality can be a proxy measure for ease of healthcare access and has implications in the care of people with AF | [Rural-Urban classification](https://www.ons.gov.uk/methodology/geography/geographicalproducts/ruralurbanclassifications) based on practice postcode |

These data for these variables need to be downloaded.

- Get a list of GP practice details from the
  [ODS](https://www.odsdatasearchandexport.nhs.uk/) API using
  [`get_gp_details.R`](scripts/get_gp_details.R)

- Get monthly details of GP practice registered populations from [NHS
  Digital](https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice)
  using
  [`get_gp_registered_patients.R`](scripts/get_gp_registered_patients.R)

- Process this data to extract out relevant details using
  [`get_gp_population_summary.R`](scripts/get_gp_population_summary.R)

- Get matching data from Fingertips using
  [`data_fingertips.R`](scripts/data_fingertips.R)

- Download [GP workforce
  data](https://digital.nhs.uk/data-and-information/publications/statistical/general-and-personal-medical-services/30-april-2023)
  for April 2023

- Process GP workforce information using
  [`get_gp_workforce.R`](scripts/get_gp_workforce.R)

- Process all the matching data using
  [`get_matching_variables_df.R`](scripts/get_matching_variables_df.R)

### Outcome data

The outcome data was obtained from the
[CVDPREVENT](https://www.cvdprevent.nhs.uk/) audit via their API.

Use the `get_cvd_data.R` script to download the data.

### Impute missing matching data

- Investigate the amount of missing data using
  [`dpp_matching_variables_missingness.qmd`](outputs/dpp_matching_variables_missingness.qmd)

- Perform multiple imputation to ‘fill-in’ missing matching data using
  [`dpp_impute_missing_matching_variables.qmd`](outputs/dpp_impute_missing_matching_variables.qmd)

### Assign GP practices to projects

GP practices are assigned to each funded intervention based on the
geography specified as part of funding applications.

- Get a [lookup from GP practice to PCN, CCG, ICB and NHS
  Region](https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/august-2024)
  from NHS Digital. Name this file `intervention_counterfac_lu.csv`.

- Get a list of funded initiatives and their respective start / end
  dates using
  [`get_dpp_project_final_list.R`](scripts/get_dpp_project_final_list.R)
  NB, this requires a copy of the funding application spreadsheet, which
  is not currently publicly available.

- Link funding applications with practices using
  [`link_grants_with_practices.R`](scripts/link_grants_with_practices.R)
  NB, this step is not possible without the above list of funding
  applications.

Some hospital-based projects have a unique list of practices or use a
method of practice assignment which is based on dominant provider of
care based on MSOA geography. To assign practices based on this approach
you need to:

- Download a [lookup of hospital catchment
  areas](https://app.box.com/s/qh8gzpzeo1firv1ezfxx2e6c4tgtrudl)
  produced by Office for Health Improvement and Disparities (OHID). NB,
  this is the ‘2022 Trust Catchment Populations_Supplementary MSOA
  Analysis.xlsx’ file.

- Download a lookup from postcode to MSOA from the [Open Geography
  Portal](https://open-geography-portalx-ons.hub.arcgis.com/datasets/ons::postcode-to-oa-2021-to-lsoa-to-msoa-to-lad-november-2024-best-fit-lookup-in-the-uk/about).

- Use [`get_gps_per_hospital.R`](scripts/get_gps_per_hospital.R) to
  produce catchment areas based on the dominant provider of care in each
  MSOA, which are then linked with GP practices based on postcode to
  MSOA lookup.

### Matching and impact analyses

The process of matching intervention GP practices with control GP
practices and the impact analyses are done at national, case-study
(aggregate) and case-study (individual) levels.

- The national-level analysis can be reproduced using
  [`dpp_programme_report_template.qmd`](outputs/dpp_programme_report_template.qmd).

- The case-study (aggregate) analyses can be reproduced using
  [`dpp_overall_report_template.qmd`](outputs/dpp_overall_report_template.qmd).

- The individual case-study reports can all be reproduced using
  [`dpp_gp_report_template.qmd`](outputs/dpp_gp_report_template.qmd).
  Which project the template produces a report for is controlled by the
  parameters set within the `yaml` header of the document:

  - `project_id` controls which project is reported, using the ‘P’
    nomenclature, where ‘P1’ is the ID for project 1.

  - `alternate_data` is used where there are multiple ways of assigning
    GP practices to the intervention area. Setting this to `FALSE` will
    mean the default GP practice assignment method defined in
    `link_grants_with_practices.R` is used. Setting to `TRUE` (default)
    will use any alternative method of assignment, as detailed in the
    `dpp_gp_report_template.qmd` script.

  - `alternate_data_details` is used to specify which of multiple GP
    practice assignment methods are to be used. Commonly used parameters
    include “emergency” and “af”. “emergency” is used for hospital-based
    projects to assign GP practices based on MSOAs where the hospital is
    the dominant provider of emergency care. “af” is used for a single
    project in which only GP practices which undertook activities to
    improve care of atrial fibrillation are to be counted.

  - `save_did` is used to control whether the underlying data is saved
    for future use with the `calc_percentage_difference.R` script.

``` yaml
---
params:
    project_id: "P20"
    alternate_data: TRUE
    alternate_data_details: ""
    save_did: TRUE
---
```
