
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

Run the `get_dependencies.R` script to identify the packages used within
this analysis. NB, this file requires at least `renv`, `janitor` and
`dplyr` to first be installed on your system to run.

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
  `get_gp_details.R`

- Get monthly details of GP practice registered populations from [NHS
  Digital](https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice)
  using `get_gp_registered_patients.R`

- Process this data to extract out relevant details using
  `get_gp_population_summary.R`

- Get matching data from Fingertips using `data_fingertips.R`

- Download [GP workforce
  data](https://digital.nhs.uk/data-and-information/publications/statistical/general-and-personal-medical-services/30-april-2023)
  for April 2023

- Process GP workforce information using `get_gp_workforce.R`

- Process all the matching data using `get_matching_variables_df.R`

### Outcome data

The outcome data was obtained from the
[CVDPREVENT](https://www.cvdprevent.nhs.uk/) audit via their API.

Use the `get_cvd_data.R` script to download the data.

### Impute missing matching data

- Investigate the amount of missing data using
  `dpp_matching_variables_missingness.qmd`

- Perform multiple imputation to ‘fill-in’ missing matching data using
  `dpp_impute_missing_matching_variables.qmd`
