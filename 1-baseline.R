#################################################################################
#   R-script:     1-baseline.R
#   Project:		  "Global economic costs due to vivax malaria and the potential impact of its radical cure: A modelling study" by A Devine et al. (PlosMed)
#
#
#   Data used:    (1) "mapdata.xlsx" from Battle et al. 2019 Lancet
#                 (2) "tsb.xlsx" from Battle et al. 2019 Lancet
#                 (3) "gdp.xlsx" from World Bank: GDP per capita estimates (updated 2020 Oct 15) 
#                 (4) "provcost.xlsx" from WHO-CHOICE 2011
#                 (5) "gdp_deflators.xlsx" from World Bank: GDP deflator (annual %) (updated 2020 Oct 15)
#                 (6) "lcu.xlsx" from World Bank Official exchange rate (Updated 2020 October 15)
#                 (7)  "severe_prop.xlsx" from Rahimi 2014 Mal J
#                 (8) "diag&drugs.xlsx" from WHO 2018 World Malaria Report
#                 (9) "DxCosts.xlsx" from Devine 2019 WHO Bull, de Oliveira 2012 Mal J, & Peixoto 2016 Mal J
#                 (10) "drugcosts.xlsx" from MSH 2015 Int'l Medical Products Price Guide & Hill 2018 BMJ Global Health
#                 (11) "PQexclusions.xlsx" from Howes 2012 PlosMed & Baird 2018 Mal
#
#   Data created:	"baseline.Rdata" & "baseline.csv"
#
#   Purpose:  		Loading input data required and analysing the baseline costs of P. vivax in 2017 USD
#
#   Updated:	    14-April-2021
#   Authors: 		  Niamh Meagher & Angela Devine
#################################################################################

#   NOTE: All data used can be found in the 'inputs' folder. Sources for these described in the publication (see S2 File in particular).

# NOTE: Parameters values are described as mean (base), low (low bound), & high (high bound)

rm(list = ls())

options(scipen = 999)   # Turn off the scientific notation default



####   POPULATION & INCIDENCE   ####

# Load data: country level case counts - "mapdata.xlsx"
case.dat <-
  readxl::read_xlsx("inputs/mapdata.xlsx", range = "A1:U136", sheet = "pv_case_data_new")
# Relevant variables:  "incidence_count_rmean", "incidence_count_LCI" and "incidence_count_UCI"

# Edit data:  (1) Keep country name, iso country code, year, age group, population and incidence counts (mean, LCI & UCI)
#             (2) Round to nearest whole number
incidence.dat <- case.dat %>%
  select(
    c(
      Name,
      iso3,
      Year,
      Pop,
      age_group,
      incidence_count_rmean,
      incidence_count_LCI,
      incidence_count_UCI
    )
  ) %>%
  mutate_if(is.numeric, round)



####   TREATMENT SEEKING BEHAVIOUR   ####

# Load data - proportion seeking behavior "tsb.xlsx"
# NOTE: Treatment seeking behavior is constant between age groups

tsb.dat <-
  readxl::read_xlsx("inputs/tsb.xlsx", range = "A1:K316", sheet = "2017")

# Edit data:  (1) Select only one record per country as estimates don't vary for infants/children/adults
#             (2) Rename 'country_name' to 'Name'
tsb.dat <- tsb.dat %>%
  select(c(country_name, ts_mean_any, ts_low_any, ts_high_any)) %>%
  rename(Name = country_name) %>%
  distinct(Name, .keep_all = TRUE)

# Check missing TSB values
lapply(tsb.dat[-1], function(x)
  tsb.dat$Name[which(is.na(x))])
# No missing values for any of the TSB values

# Rename "East Timor" to "Timor-Leste" in the TSB data so that datasets can be joined together
tsb.dat$Name <-
  str_replace(tsb.dat$Name, "East Timor", "Timor-Leste")

incidence.dat <-
  merge(incidence.dat, tsb.dat[, c("Name", "ts_mean_any", "ts_low_any", "ts_high_any")],
        by = "Name", all.x = T)

# Calculate number seeking treatment: incidence * proportion seeking treatment.
# Need to:  (1) Do this calculation for the mean, low and high estimates of incidence and proportion.
#               Apply individually to each age group.
#           (2) Round numbers produced to the nearest integer
incidence.dat <- incidence.dat %>%
  mutate(tscount_mean = incidence_count_rmean * ts_mean_any) %>%
  mutate(tscount_low = incidence_count_LCI * ts_low_any) %>%
  mutate(tscount_high = incidence_count_UCI * ts_high_any) %>%
  mutate_at(vars(starts_with("tscount")), round, 0)
# Create a working dataframe:
rm(case.dat, tsb.dat)
incidence.dat <- incidence.dat %>%
  rename(
    country = Name,
    year = Year,
    population = Pop,
    mean_incidence = incidence_count_rmean,
    low_incidence = incidence_count_LCI,
    high_incidence = incidence_count_UCI,
    mean_tsb_prop = ts_mean_any,
    low_tsb_prop = ts_low_any,
    high_tsb_prop = ts_high_any,
    mean_tsb_count = tscount_mean,
    low_tsb_count = tscount_low,
    high_tsb_count = tscount_high
  )
baseline <- incidence.dat

# Remove North Korea - not included in analysis because missing many data points
baseline <- baseline[!(baseline$country == "North Korea"), ]



####   GROSS DOMESTIC PRODUCT   ####

# Load the data - updated "gdp"
gdp.dat <-
  readxl::read_xlsx("inputs/gdp.xlsx", range = "A4:BL268", sheet = "Data")

# Select the 2017 data
table(gdp.dat[, 3:4])

gdp.dat <- gdp.dat %>%
  select("Country Name", "2017") %>%
  mutate_if(is.numeric, round, 2) %>%
  rename(country = 'Country Name', gdp2017_percap = '2017')

# Check for missing GDP values
lapply(gdp.dat[-1], function(x)
  gdp.dat$country[which(is.na(x))])
# Some countries have an alternate name, others don't have GDP/capita estimates from 2017.
# Summarise these when joining the lists together (as GDP data is global).

# For missing GDP values for Eritrea, Venezuela and Somalia to be input manually
# This is from UNdata GDP per capita estimates (updated 2020/02/10)
eritrea.gdp   <- 1737
venezuela.gdp <- 8436
somalia.gdp   <- 103
# Replace these values in the the gdp.dat dataframe
gdp.dat$gdp2017_percap <-
  replace(gdp.dat$gdp2017_percap,
          gdp.dat$country == "Eritrea",
          eritrea.gdp)
gdp.dat$gdp2017_percap <-
  replace(gdp.dat$gdp2017_percap,
          gdp.dat$country == "Venezuela, RB",
          venezuela.gdp)
gdp.dat$gdp2017_percap <-
  replace(gdp.dat$gdp2017_percap,
          gdp.dat$country == "Somalia",
          somalia.gdp)
# Names to be edited: (1) Iran, Islamic Rep. (becomes Iran)
#                     (2) Lao PDR (becomes Laos)
#                     (3) Korea, Rep. (becomes South Korea)
#                     (4) Venezuela, RB (becomes Venezuela)
#                     (5) Yemen, Rep. (becomes Yemen)
gdp.dat$country <-
  str_replace(gdp.dat$country, "Iran, Islamic Rep.", "Iran")
gdp.dat$country <-
  str_replace(gdp.dat$country, "Lao PDR", "Laos")
gdp.dat$country <-
  str_replace(gdp.dat$country, "Korea, Rep.", "South Korea")
gdp.dat$country <-
  str_replace(gdp.dat$country, "Venezuela, RB", "Venezuela")
gdp.dat$country <-
  str_replace(gdp.dat$country, "Yemen, Rep.", "Yemen")
# Calculate GDP per capita per day: GDP per capita / 365
gdp.dat <- gdp.dat %>%
  mutate(gdp2017_percapday = gdp2017_percap / 365)

# Merging data into our working dataframe
baseline <-
  merge(baseline, gdp.dat[, c("country", "gdp2017_percap", "gdp2017_percapday")],
        by = "country", all.x = T)
# Check what's missing
lapply(baseline[-1], function(x)
  baseline$country[which(is.na(x))])
# No missing GDPs



####  HEALTHCARE PROVIDER VISIT COST   ####

# This section calculates the cost of inpatient and outpatient visits to the healthcare providers.

# Load the cost data - 'provcost.xlsx'
visit_costs.dat <- readxl::read_xlsx("inputs/provcost.xlsx")
# Rename data
visit_costs.dat <- visit_costs.dat %>%
  rename(country = 'country_name',
         ip_cost2008 = 'Inpatient (2008 LCU)',
         op_cost2008 = 'OP (2008 LCU)')

# Check for missing costs
lapply(visit_costs.dat[-1], function(x)
  visit_costs.dat$country[which(is.na(x))])
# Costs are missing for Somalia so these are replaced with the costs from Ethiopia.
visit_costs.dat$ip_cost2008 <-
  replace(
    visit_costs.dat$ip_cost2008,
    visit_costs.dat$country == "Somalia",
    as.numeric(visit_costs.dat[which(visit_costs.dat$country == "Ethiopia"), "ip_cost2008"])
  )
visit_costs.dat$op_cost2008 <-
  replace(
    visit_costs.dat$op_cost2008,
    visit_costs.dat$country == "Somalia",
    as.numeric(visit_costs.dat[which(visit_costs.dat$country == "Ethiopia"), "op_cost2008"])
  )

# Check for missing costs
lapply(visit_costs.dat[-1], function(x)
  visit_costs.dat$country[which(is.na(x))])
# None of the countries we are examining are missing

# Rename "East Timor/Timor-Leste" to "Timor-Leste" in the provider cost data
visit_costs.dat$country <-
  str_replace(visit_costs.dat$country,
              "East Timor/Timor-Leste",
              "Timor-Leste")

# Load GDP deflators to inflate costs from 2008 to 2017 - "gdp_deflators.xlsx"
deflators.dat <-
  readxl::read_xlsx("inputs/gdp_deflators.xlsx", sheet = "Sheet1")
deflators.dat <- deflators.dat %>%
  select(c("Country Name", "2017")) %>%
  rename(country = 'Country Name', deflators2017 = '2017')

# Names to be edited:     (1) Iran, Islamic Rep. (becomes Iran)
#                         (2) Lao PDR (becomes Laos)
#                         (3) Korea, Rep. (becomes South Korea)
#                         (4) Venezuela, RB (becomes Venezuela)
#                         (5) Yemen, Rep. (becomes Yemen)
deflators.dat$country <-
  str_replace(deflators.dat$country, "Iran, Islamic Rep.", "Iran")
deflators.dat$country <-
  str_replace(deflators.dat$country, "Lao PDR", "Laos")
deflators.dat$country <-
  str_replace(deflators.dat$country, "Korea, Rep.", "South Korea")
deflators.dat$country <-
  str_replace(deflators.dat$country, "Venezuela, RB", "Venezuela")
deflators.dat$country <-
  str_replace(deflators.dat$country, "Yemen, Rep.", "Yemen")

# Merge the deflators to the cost.dat dataframe
visit_costs.dat <-
  merge(visit_costs.dat,
        deflators.dat[, c("country", "deflators2017")],
        by = "country",
        all.x = T)
# Check for missing deflators
lapply(visit_costs.dat[-1], function(x)
  visit_costs.dat$country[which(is.na(x))])
# None are missing

# Convert local currencies to USD - "lcu.xlsx"
lcu.dat <-
  readxl::read_xlsx("inputs/lcu.xlsx", range = "A4:BL268", sheet = "Data")
# Keep 2017 conversion rates
lcu.dat <- lcu.dat %>%
  select(c("Country Name", "2017")) %>%
  rename(country = 'Country Name', lcu_US = '2017')
# Names to be edited:     (1) Iran, Islamic Rep. (becomes Iran)
#                         (2) Lao PDR (becomes Laos)
#                         (3) Korea, Rep. (becomes South Korea)
#                         (4) Venezuela, RB (becomes Venezuela)
#                         (5) Yemen, Rep. (becomes Yemen)
lcu.dat$country <-
  str_replace(lcu.dat$country, "Iran, Islamic Rep.", "Iran")
lcu.dat$country <-
  str_replace(lcu.dat$country, "Lao PDR", "Laos")
lcu.dat$country <-
  str_replace(lcu.dat$country, "Korea, Rep.", "South Korea")
lcu.dat$country <-
  str_replace(lcu.dat$country, "Venezuela, RB", "Venezuela")
lcu.dat$country <-
  str_replace(lcu.dat$country, "Yemen, Rep.", "Yemen")

# Merge the currency exchange rates to the cost.dat dataframe
visit_costs.dat <-
  merge(visit_costs.dat, lcu.dat[, c("country", "lcu_US")],
        by = "country", all.x = T)
# Check for missing exchange rates
lapply(visit_costs.dat[-1], function(x)
  visit_costs.dat$country[which(is.na(x))])
# No countries we are examining are missing

# Clean up the other dataframes:
rm(deflators.dat,
   lcu.dat,
   eritrea.gdp,
   venezuela.gdp,
   somalia.gdp)

# Parameter - Severe Pv cases requiring hospitalization "severe_prop.xlsx"
severe.dat <- readxl::read_xlsx("inputs/severe_prop.xlsx")
mean_sev_prop <- as.numeric(severe.dat[1, "Model value"])
low_sev_prop <- as.numeric(severe.dat[1, "low bound"])
high_sev_prop <- as.numeric(severe.dat[1, "high bound"])
# Remove dataframe
rm(severe.dat)

# Perform the required calculations:
#       (1) Total cost of IP visit in $USD (3 days) = (ip_cost2008*3*deflators2017)/lcu_US
#       (2) Total cost of OP visit in $USD (1 day) = (op_cost2008*deflators2017)/lcu_US
#       (3) Mean visit cost/case ($USD) =
#                (IP visit estimate*mean_sev_prop)+(OP visit estimate*(1-mean_sev_prop))
#       (4) Low bound visit cost/case ($USD) =
#                (IP visit estimate*low_sev_prop)+(OP visit estimate*(1-low_sev_prop))
#       (5) High bound visit cost/case ($USD) =
#                (IP visit estimate*high_sev_prop)+(OP visit estimate*(1-high_sev_prop))
# Calculation 1 & 2:
visit_costs.dat <- visit_costs.dat %>%
  mutate(
    ip_visit_cost = (ip_cost2008 * 3 * deflators2017) / lcu_US,
    op_visit_cost = (op_cost2008 * deflators2017) / lcu_US
  )
# Calculation 3, 4 & 5:
visit_costs.dat <- visit_costs.dat %>%
  mutate(
    mean_cost_pervisit = (ip_visit_cost * mean_sev_prop) + (op_visit_cost *
                                                              (1 - mean_sev_prop)),
    low_cost_pervisit = (ip_visit_cost * low_sev_prop) + (op_visit_cost *
                                                            (1 - low_sev_prop)),
    high_cost_pervisit = (ip_visit_cost * high_sev_prop) + (op_visit_cost *
                                                              (1 - high_sev_prop))
  )

# Merge data into the working dataframe
baseline <-
  merge(baseline, visit_costs.dat, by = "country", all.x = T)

# Check for missing values
lapply(baseline[-1], function(x)
  baseline$country[which(is.na(x))])
# No missing data



####   DIAGNOSTIC COSTS   ####

# This section calculates the cost of diagnosis to the healthcare providers.
# load the percent diagnosed by RDT and the total % of cases that are diagnosed - "diag&drugs.xlsx"
diag_costs.dat <-
  readxl::read_xlsx("inputs/diag&drugs.xlsx", range = "A1:J642", sheet = "Table 2")

# Editing to the original data:   (1) Keep only 2017 data.
#                                 (2) Fix the names of the columns
#                                 (3) Convert headers with WHO region in 'country' to a region variable
#                                 (4) Use LOCF to replace the missing region names
#                                 (5) Remove headers with WHO region in 'country'
#                                 (6) Replace the missing values "-" with 0 for 'diag_2017'
#                                 (7) Convert 'diag_2017' to numeric data type.
#                                 (8) Round 'diag_2017' to the nearest integer.
#                                 (9) Use LOCF to replace the missing country names.
#                                 (10) Reshape to wide data format.
#                                 (11) Rename the newly created variables appropriately
diag_costs.dat <- diag_costs.dat %>%
  select("WHO region Country/area", "...2", "2017") %>%
  rename(country = 'WHO region Country/area',
         case_no = '...2',
         diag_2017 = '2017') %>%
  mutate(
    region = case_when(
      country == "AFRICAN" ~ "AFRO",
      country == "AMERICAS" ~ "PAHO",
      country == "EASTERN MEDITERRANEAN" ~ "EMRO",
      country == "EUROPEAN" ~ "EURO",
      country == "SOUTH-EAST  ASIA" ~ "SEARO",
      country == "WESTERN PACIFIC" ~ "WPRO"
    )
  ) %>%
  mutate_at(vars(region), na.locf) %>%
  filter(
    !country %in% c(
      "AFRICAN",
      "AMERICAS",
      "EASTERN MEDITERRANEAN",
      "EUROPEAN",
      "SOUTH-EAST  ASIA",
      "WESTERN PACIFIC"
    )
  ) %>%
  mutate(diag_2017 = replace(diag_2017, which(diag_2017 == "-"), "0")) %>%
  mutate_at(vars(diag_2017), as.numeric) %>%
  mutate_at(vars(diag_2017), round, 0) %>%
  mutate_at(vars(country), na.locf) %>%
  pivot_wider(names_from = case_no, values_from = diag_2017) %>%
  rename(
    expected_cases = 'Presumed and confirmed',
    micro = 'Microscopy examined',
    micro_cnf = 'Confirmed with microscopy',
    rdt = 'RDT examined',
    rdt_cnf = 'Confirmed with RDT',
    import = 'Imported cases'
  )

# Use this data to calculate:
#       (1) Proportion of cases diagnosed by RDT = rdt_cnf/(rdt_cnf+micro_cnf)
#       (2) Proportion of cases that are confirmed by a diagnostic test = (rdt_cnf+micro_cnf)/expected_cases
diag_costs.dat <- diag_costs.dat %>%
  mutate(
    rdt_cnf_prop = rdt_cnf / (rdt_cnf + micro_cnf),
    cnf_prop = (rdt_cnf + micro_cnf) / expected_cases
  )

summary(diag_costs.dat$cnf_prop) # Check for proportions that are >1 (these need to be changed to 1)
diag_costs.dat[which(diag_costs.dat$cnf_prop > 1), "country"]
# Argentina and Peru are >1 so replaced with 1
diag_costs.dat[which(diag_costs.dat$cnf_prop > 1), "cnf_prop"] <-
  1

# Check for missing values
lapply(diag_costs.dat[-1], function(x)
  diag_costs.dat$country[which(is.na(x))])
# No missing values


# Diagnosis cost details "DxCosts.xlsx"
diagcost.dat <- readxl::read_xlsx("inputs/DxCosts.xlsx")
# AFRO
afro_rdt <-
  as.numeric(diagcost.dat[diagcost.dat$region == "AFRO" &
                            diagcost.dat$test == "mRDT", "base"])
afro_micro <-
  as.numeric(diagcost.dat[diagcost.dat$region == "AFRO" &
                            diagcost.dat$test == "micro", "base"])
# EMRO
emro_rdt <-
  as.numeric(diagcost.dat[diagcost.dat$region == "EMRO" &
                            diagcost.dat$test == "mRDT", "base"])
emro_micro <-
  as.numeric(diagcost.dat[diagcost.dat$region == "EMRO" &
                            diagcost.dat$test == "micro", "base"])
# PAHO
paho_rdt <-
  as.numeric(diagcost.dat[diagcost.dat$region == "PAHO" &
                            diagcost.dat$test == "mRDT", "base"])
paho_micro <-
  as.numeric(diagcost.dat[diagcost.dat$region == "PAHO" &
                            diagcost.dat$test == "micro", "base"])
# SEARO
searo_rdt <-
  as.numeric(diagcost.dat[diagcost.dat$region == "SEARO" &
                            diagcost.dat$test == "mRDT", "base"])
searo_micro <-
  as.numeric(diagcost.dat[diagcost.dat$region == "SEARO" &
                            diagcost.dat$test == "micro", "base"])
# WPRO
wpro_rdt <-
  as.numeric(diagcost.dat[diagcost.dat$region == "WPRO" &
                            diagcost.dat$test == "mRDT", "base"])
wpro_micro <-
  as.numeric(diagcost.dat[diagcost.dat$region == "WPRO" &
                            diagcost.dat$test == "micro", "base"])

# Remove source table
rm(diagcost.dat)

# Calculate the cost of a malaria diagnosis per case using region-specific costs:
#   dx_cost_percase = (proportion of cases confirmed * proportion confirmed by RDT * RDT cost) +
#   (proportion of cases confirmed * proportion of cases confirmed by microsocpy * microscopy cost)
#   where proportion of cases confirmed by microscopy = 1 - proportion of cases confirmed by RDT

# Apply microscopy cost to all cases in Peru as RDT diagnosed cases are also confirmed with microscopy.
diag_costs.dat <- diag_costs.dat %>%
  mutate(
    mean_dxcost_percase = case_when(
      region == "AFRO" ~ (cnf_prop * rdt_cnf_prop * afro_rdt) +
        (cnf_prop * (1 - rdt_cnf_prop) *
           afro_micro),
      region == "EMRO" ~ (cnf_prop * rdt_cnf_prop * emro_rdt) +
        (cnf_prop * (1 - rdt_cnf_prop) *
           emro_micro),
      region == "PAHO" &
        country != "Peru" ~ (cnf_prop * rdt_cnf_prop * paho_rdt) +
        (cnf_prop * (1 - rdt_cnf_prop) *
           paho_micro),
      region == "SEARO" ~ (cnf_prop *
                             rdt_cnf_prop * searo_rdt) +
        (cnf_prop * (1 - rdt_cnf_prop) *
           searo_micro),
      region == "WPRO" ~ (cnf_prop * rdt_cnf_prop * wpro_rdt) +
        (cnf_prop * (1 - rdt_cnf_prop) *
           wpro_micro),
      country == "Peru" ~ (cnf_prop * rdt_cnf_prop * paho_rdt) +
        (cnf_prop * paho_micro)
    )
  ) %>%
  mutate(
    low_dxcost_percase = mean_dxcost_percase - (0.5 * mean_dxcost_percase),
    high_dxcost_percase = mean_dxcost_percase + (0.5 * mean_dxcost_percase)
  )

# Change country names to match those listed in the baseline dataframe.
# Names to be edited:  (1) Bolivia (Plurinational State of) (becomes Bolivia)
#                  (2) Iran (Islamic Republic of) (becomes Iran)
#                  (3) Lao People's Democratic Republic (becomes Laos)
#                  (4) Republic of Korea (becomes South Korea)
#                  (5) Venezuela (Bolivarian Republic of) (becomes Venezuela)
#                  (6) Viet Nam (becomes Vietnam)
diag_costs.dat$country <-
  str_replace(diag_costs.dat$country,
              "Bolivia \\(Plurinational State of\\)",
              "Bolivia")
diag_costs.dat$country <-
  str_replace(diag_costs.dat$country,
              "Iran \\(Islamic Republic of\\)",
              "Iran")
diag_costs.dat$country <-
  str_replace(diag_costs.dat$country,
              "Lao People's Democratic Republic",
              "Laos")
diag_costs.dat$country <-
  str_replace(diag_costs.dat$country, "Republic of Korea", "South Korea")
diag_costs.dat$country <- str_replace(diag_costs.dat$country,
                                      "Venezuela \\(Bolivarian Republic of\\)",
                                      "Venezuela")
diag_costs.dat$country <-
  str_replace(diag_costs.dat$country, "Viet Nam", "Vietnam")

# Merge data into the working dataframe
baseline <-
  merge(baseline, diag_costs.dat, by = "country", all.x = T)

# Check for missing values
lapply(baseline[-1], function(x)
  baseline$country[which(is.na(x))])
# No missing values



####   PROVIDER TREATMENT COST   ####

# This section calculates the cost of drug treatments for the healthcare providers.
# To get the blood stage drug treatment used in each country - "diag&drugs.xlsx"
treatment_costs.dat <-
  readxl::read_xlsx("inputs/diag&drugs.xlsx", range = "A2:F103", sheet = "Table 1")

# Editing to the data:   (1) Keep only P. vivax treatment and country names.
#                        (2) Rename of columns
#                        (3) Convert headers with WHO region in 'country' to a region variable
#                        (4) Use LOCF to replace the missing region names
#                        (5) Remove headers with WHO region in 'country'
#                        (6) Replace the missing values "-" with NA for 'pv_treatment'
treatment_costs.dat <- treatment_costs.dat %>%
  select("Uncomplicated unconfirmed", "Treatment") %>%
  rename(country = 'Uncomplicated unconfirmed', pv_treatment = 'Treatment') %>%
  mutate(
    region = case_when(
      country == "AFRICAN" ~ "AFRO",
      country == "AMERICAS" ~ "PAHO",
      country == "EASTERN MEDITERRANEAN" ~ "EMRO",
      country == "SOUTH-EAST ASIA" ~ "SEARO",
      country == "WESTERN PACIFIC" ~ "WPRO"
    )
  ) %>%
  mutate_at(vars(region), na.locf) %>%
  filter(
    !country %in% c(
      "AFRICAN",
      "AMERICAS",
      "EASTERN MEDITERRANEAN",
      "SOUTH-EAST ASIA",
      "WESTERN PACIFIC"
    )
  ) %>%
  mutate(pv_treatment = replace(pv_treatment, which(pv_treatment ==
                                                      "-"), NA))

# Abbreviations:(1) AL - Artemether-lumefantrine
#               (2) AS+AQ - Artesunate-amodiaquine
#               (3) DHA-PP - Dihydroartemisinin-piperaquine
#               (4) ASMQ - Artesunate and mefloquine 
table(treatment_costs.dat$pv_treatment)
count(treatment_costs.dat, pv_treatment)
# Clarified blood stage treatment for the following countries that list more than the above abbreviations
# China:    CQ chosen because listed first
# Laos:     AL chosen because listed as 1st line
# Malaysia: AL is an assumption

# Create a variable for blood stage treatment
treatment_costs.dat <- treatment_costs.dat %>%
  mutate(
    bloodstage = case_when(
      str_detect(pv_treatment, "AL") ~ "AL",
      str_detect(pv_treatment, "AQ") ~ "ASAQ",
      str_detect(pv_treatment, "MQ") ~ "ASMQ",
      str_detect(pv_treatment, "CQ") ~ "CQ",
      str_detect(pv_treatment, "DHA-PP") ~ "DHP",
      country == "Malaysia" ~ "AL"
    )
  ) %>%
  select("country", "region", "bloodstage")

# Load costs for each blood-stage treatment - 'drugcosts.xlsx'
drugcost.dat <- readxl::read_xlsx("inputs/drugcosts.xlsx")
# AL
al_cost <- as.numeric(drugcost.dat[5, "Cost"])
# ASAQ
asaq_cost <- as.numeric(drugcost.dat[4, "Cost"])
# CQ
cq_cost <- as.numeric(drugcost.dat[3, "Cost"])
# DHA-PP
dhapp_cost <- as.numeric(drugcost.dat[6, "Cost"])
# ASMQ
asmq_cost <- as.numeric(drugcost.dat[7, "Cost"])
# PQ - cost per low dose treatment
pq_cost <- as.numeric(drugcost.dat[1, "Cost"])
# Proportion of patients who are prescribed primaquine in baseline
# The base and low values are assumptions. The high value is from Douglas 2017 PlosMed
pq_prescribed_mean <- 0.4
pq_prescribed_low <- 0.1
pq_prescribed_high <- 0.8

# Remove drug cost table
rm(drugcost.dat)

# Calculate the cost of blood stage cure per case
treatment_costs.dat <- treatment_costs.dat %>%
  mutate(
    mean_bloodstage_cost = case_when(
      bloodstage == "CQ" ~ cq_cost,
      bloodstage == "AL" ~ al_cost,
      bloodstage == "ASAQ" ~ asaq_cost,
      bloodstage == "DHP" ~ dhapp_cost,
      bloodstage == "ASMQ" ~ asmq_cost
    )
  ) %>%
  mutate(
    low_bloodstage_cost = mean_bloodstage_cost - (0.5 * mean_bloodstage_cost),
    high_bloodstage_cost = mean_bloodstage_cost + (0.5 * mean_bloodstage_cost)
  )

# Determine which countries use PQ - 'diag&drugs.xlsx'
pq_use.dat <-
  readxl::read_xlsx("inputs/diag&drugs.xlsx", range = "A2:F103", sheet = "Table 1")

pq_use.dat <- pq_use.dat %>%
  select("Uncomplicated unconfirmed", "Treatment") %>%
  rename(country = "Uncomplicated unconfirmed", pv_treatment = "Treatment") %>%
  filter(
    !country %in% c(
      "AFRICAN",
      "AMERICAS",
      "EASTERN MEDITERRANEAN",
      "EUROPEAN",
      "SOUTH-EAST ASIA",
      "WESTERN PACIFIC"
    )
  ) %>%
  mutate(pq_use = case_when(str_detect(pv_treatment, "PQ") ~ "Y",
                            TRUE ~ "N")) %>%
  select(-pv_treatment)

treatment_costs.dat <-
  merge(treatment_costs.dat,
        pq_use.dat,
        by = "country",
        all.x = T)
# Remove the pq_use.dat dataframe
rm(pq_use.dat)

# Some countries need to be changed to match those listed in the baseline dataframe.
# Names to be edited: (1) Bolivia (Plurinational State of) (becomes Bolivia)
#                     (2) Iran (Islamic Republic of) (becomes Iran)
#                     (3) Lao People's Democratic Republic (becomes Laos)
#                     (4) Republic of Korea (becomes South Korea)
#                     (5) Venezuela (Bolivarian Republic of) (becomes Venezuela)
#                     (6) Viet Nam (becomes Vietnam)
treatment_costs.dat$country <-
  str_replace(treatment_costs.dat$country,
              "Bolivia \\(Plurinational State of\\)",
              "Bolivia")
treatment_costs.dat$country <-
  str_replace(treatment_costs.dat$country,
              "Iran \\(Islamic Republic of\\)",
              "Iran")
treatment_costs.dat$country <-
  str_replace(treatment_costs.dat$country,
              "Lao People's Democratic Republic",
              "Laos")
treatment_costs.dat$country <-
  str_replace(treatment_costs.dat$country, "Republic of Korea", "South Korea")
treatment_costs.dat$country <-
  str_replace(treatment_costs.dat$country,
              "Venezuela \\(Bolivarian Republic of\\)",
              "Venezuela")
treatment_costs.dat$country <-
  str_replace(treatment_costs.dat$country, "Viet Nam", "Vietnam")

# Merge the treatment cost data into our working dataframe
baseline <-
  merge(
    baseline,
    treatment_costs.dat,
    by = c("country", "region"),
    all.x = T
  )

lapply(baseline[-1], function(x)
  baseline$country[which(is.na(x))])
# Madagascar missing bloodstage treatment information - use Pf treatment (ASAQ) and costings as appropriate
baseline$bloodstage[baseline$country == "Madagascar"] <-
  "ASAQ"
baseline$mean_bloodstage_cost[baseline$country == "Madagascar"] <-
  asaq_cost
baseline$low_bloodstage_cost[baseline$country == "Madagascar"] <-
  asaq_cost - (0.5 * asaq_cost)
baseline$high_bloodstage_cost[baseline$country == "Madagascar"] <-
  asaq_cost + (0.5 * asaq_cost)

rm(al_cost, asaq_cost, cq_cost, dhapp_cost)



####   G6PD COST   ####

#Load cost of G6PD diagnostics in countries currently using G6PD testing - "DxCosts.xlsx"
g6pddx_costs.dat <- readxl::read_xlsx("inputs/DxCosts.xlsx")

# Extract cost for FST testing - applied just to Malaysia
# WPRO
fst_wpro_mean <-
  as.numeric(subset(g6pddx_costs.dat, region == "WPRO" &
                      test == "fst")$base)
fst_wpro_low <- fst_wpro_mean - (0.5 * fst_wpro_mean)
fst_wpro_high <- fst_wpro_mean + (0.5 * fst_wpro_mean)

# Add this cost to Malaysia only
baseline <-
  baseline %>% mutate(
    g6pd_dx_mean = 0,
    g6pd_dx_low = 0,
    g6pd_dx_high = 0
  )
baseline[baseline$country == "Malaysia", "g6pd_dx_mean"] <-
  fst_wpro_mean
baseline[baseline$country == "Malaysia", "g6pd_dx_low"] <-
  fst_wpro_low
baseline[baseline$country == "Malaysia", "g6pd_dx_high"] <-
  fst_wpro_high

lapply(baseline[-1], function(x)
  baseline$country[which(is.na(x))])
# Nothing missing (but values only apply to Malaysia)
rm(g6pddx_costs.dat, fst_wpro_mean, fst_wpro_low, fst_wpro_high)

####   SENSITIVITY AND SPECIFICITY OF G6PD TESTING   ####

# Diagnostic accuracy is taken from Ley 2019 PlosMed
g6pd.sens <- 0.96
g6pd.spec <- 0.95



####   CASES INELIGIBLE FOR PQ   ####

# A proportion of cases are ineligible for PQ treatment:
#     (1) Those who test G6PDd (in countries with G6PD screening)
#     (2) Those who are pregnant or lactating
pq.exclusions <- readxl::read_xlsx("inputs/PQexclusions.xlsx")

pq.exclusions <- pq.exclusions %>%
  select(-"Country_ID") %>%
  rename("country" = "Country",
         "g6pdd" = "G6PDd",
         "preg.lact" = "preg.lact.exclude")

# Names to be edited: (1) Iran, Islamic Rep. (becomes Iran)
#                     (2) Lao PDR (becomes Laos)
#                     (3) Korea, Rep. (becomes South Korea)
#                     (4) Viet Nam (becomes Vietnam)
pq.exclusions$country <-
  str_replace(pq.exclusions$country, "Iran  \\(Islamic Republic of\\)", "Iran")
pq.exclusions$country <-
  str_replace(pq.exclusions$country,
              "Lao People's Democratic Republic",
              "Laos")
pq.exclusions$country <-
  str_replace(pq.exclusions$country, "Korea, Republic of", "South Korea")
pq.exclusions$country <-
  str_replace(pq.exclusions$country, "Viet Nam", "Vietnam")

baseline <-
  merge(baseline,
        pq.exclusions,
        by = c("country"),
        all.x = T)
# Pregnancy/lactating proportions only applies to adult cases - set infants/children to 0
baseline[baseline$age_group != "adults", "preg.lact"] <- 0

lapply(baseline[-1], function(x)
  baseline$country[which(is.na(x))])
# Nothing missing
rm(pq.exclusions)


####   TOTAL PROVIDER COST CALCULATIONS   ####

# Applied to patients seeking treatment
# For all countries except Malaysia:
#     - Adults =
#         (Mean Malaria Dx Cost/Case + Mean P.v. Bloodstage Cure Cost/Case + Mean Visit Cost/Case) * Mean TSB +
#            [(PQ Full Low Dose Cost * (1-Prop. Preg/Lact) * PQ Pres. Prop. * Mean TSB) if country reports using PQ]
#     - Children =
#         (Mean Malaria Dx Cost/Case + Mean P.v. Bloodstage Cure Cost/Case + Mean Visit Cost/Case) * Mean TSB +
#            [(PQ Full Low Dose Cost * PQ Pres. Prop. * Mean TSB) if country reports using PQ]]
#     - Infants =
#         (Mean Malaria Dx Cost/Case + Mean P.v. Bloodstage Cure Cost/Case + Mean Visit Cost/Case) * Mean TSB +
#            [(PQ Full Low Dose Cost * 0.8 * PQ Pres. Prop. * Mean TSB) if country reports using PQ]
#
# For Malaysia (only country with G6PD screening):
#     - Adults =
#         (Mean Malaria Dx Cost/Case + Mean P.v. Bloodstage Cure Cost/Case + Mean Visit Cost/Case) * Mean TSB +
#            (FST Dx Cost * (1-Prop. Preg/Lact) * Mean TSB) +
#            (PQ Full Low Dose Cost * [G6PDd * (1 - Sensitivity) + (1 - G6PDd) * Specificity] * (1-Prop. Preg/Lact)) * Mean TSB
#     - Children =
#         (Mean Malaria Dx Cost/Case + Mean P.v. Bloodstage Cure Cost/Case + Mean Visit Cost/Case) * Mean TSB +
#            (FST Dx Cost * Mean TSB) +
#            (PQ Full Low Dose Cost * [G6PDd * (1 - Sensitivity) + (1 - G6PDd) * Specificity]) * Mean TSB
#     - Infants =
#         (Mean Malaria Dx Cost/Case + Mean P.v. Bloodstage Cure Cost/Case + Mean Visit Cost/Case) * Mean TSB +
#            (FST Dx Cost * 0.8 * Mean TSB) +
#            (PQ Full Low Dose Cost * [G6PDd * (1 - Sensitivity) + (1 - G6PDd) * Specificity]) * 0.8 * Mean TSB

baseline <- baseline %>%
  mutate(
    mean_total_provcost =
      case_when(
        age_group == "adults" & country != "Malaysia" & pq_use == "N" ~
          (
            mean_dxcost_percase + mean_bloodstage_cost + mean_cost_pervisit
          ) * mean_tsb_count,
        age_group == "adults" &
          country != "Malaysia" & pq_use == "Y" ~
          (
            mean_dxcost_percase + mean_bloodstage_cost + mean_cost_pervisit
          ) * mean_tsb_count +
          (pq_cost * (1 - preg.lact) * pq_prescribed_mean) * mean_tsb_count,
        age_group == "adults" & country == "Malaysia" ~
          (
            mean_dxcost_percase + mean_bloodstage_cost + mean_cost_pervisit
          ) * mean_tsb_count +
          (g6pd_dx_mean * (1 - preg.lact)) * mean_tsb_count +
          (pq_cost * (
            g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
          ) * (1 - preg.lact)) * mean_tsb_count,
        age_group == "children" &
          country != "Malaysia" & pq_use == "N" ~
          (
            mean_dxcost_percase + mean_bloodstage_cost + mean_cost_pervisit
          ) * mean_tsb_count,
        age_group == "children" &
          country != "Malaysia" & pq_use == "Y" ~
          (
            mean_dxcost_percase + mean_bloodstage_cost + mean_cost_pervisit
          ) * mean_tsb_count +
          (pq_cost * pq_prescribed_mean) * mean_tsb_count,
        age_group == "children" & country == "Malaysia" ~
          (
            mean_dxcost_percase + mean_bloodstage_cost + mean_cost_pervisit
          ) * mean_tsb_count +
          (g6pd_dx_mean * mean_tsb_count) +
          (pq_cost * (
            g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
          )) * mean_tsb_count,
        age_group == "infants" &
          country != "Malaysia" & pq_use == "N" ~
          (
            mean_dxcost_percase + mean_bloodstage_cost + mean_cost_pervisit
          ) * mean_tsb_count,
        age_group == "infants" &
          country != "Malaysia" & pq_use == "Y" ~
          (
            mean_dxcost_percase + mean_bloodstage_cost + mean_cost_pervisit
          ) * mean_tsb_count +
          (pq_cost * 0.8 * pq_prescribed_mean) * mean_tsb_count,
        age_group == "infants" & country == "Malaysia" ~
          (
            mean_dxcost_percase + mean_bloodstage_cost + mean_cost_pervisit
          ) * mean_tsb_count +
          (g6pd_dx_mean * 0.8 * mean_tsb_count) +
          (pq_cost * (
            g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
          )) * 0.8 * mean_tsb_count
      ),
    low_total_provcost =
      case_when(
        age_group == "adults" & country != "Malaysia" & pq_use == "N" ~
          (low_dxcost_percase + low_bloodstage_cost + low_cost_pervisit) * low_tsb_count,
        age_group == "adults" &
          country != "Malaysia" & pq_use == "Y" ~
          (low_dxcost_percase + low_bloodstage_cost + low_cost_pervisit) * low_tsb_count +
          (pq_cost * (1 - preg.lact) * pq_prescribed_low) * low_tsb_count,
        age_group == "adults" & country == "Malaysia" ~
          (low_dxcost_percase + low_bloodstage_cost + low_cost_pervisit) * low_tsb_count +
          (g6pd_dx_low * (1 - preg.lact)) * low_tsb_count +
          (pq_cost * (
            g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
          ) * (1 - preg.lact)) * low_tsb_count,
        age_group == "children" &
          country != "Malaysia" & pq_use == "N" ~
          (low_dxcost_percase + low_bloodstage_cost + low_cost_pervisit) * low_tsb_count,
        age_group == "children" &
          country != "Malaysia" & pq_use == "Y" ~
          (low_dxcost_percase + low_bloodstage_cost + low_cost_pervisit) * low_tsb_count +
          (pq_cost * pq_prescribed_low) * low_tsb_count,
        age_group == "children" & country == "Malaysia" ~
          (low_dxcost_percase + low_bloodstage_cost + low_cost_pervisit) * low_tsb_count +
          (g6pd_dx_low * low_tsb_count) +
          (pq_cost * (
            g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
          )) * low_tsb_count,
        age_group == "infants" &
          country != "Malaysia" & pq_use == "N" ~
          (low_dxcost_percase + low_bloodstage_cost + low_cost_pervisit) * low_tsb_count,
        age_group == "infants" &
          country != "Malaysia" & pq_use == "Y" ~
          (low_dxcost_percase + low_bloodstage_cost + low_cost_pervisit) * low_tsb_count +
          (pq_cost * 0.8 * pq_prescribed_low) * low_tsb_count,
        age_group == "infants" & country == "Malaysia" ~
          (low_dxcost_percase + low_bloodstage_cost + low_cost_pervisit) * low_tsb_count +
          (g6pd_dx_low * 0.8 * low_tsb_count) +
          (pq_cost * (
            g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
          )) * 0.8 * low_tsb_count
      ),
    high_total_provcost =
      case_when(
        age_group == "adults" & country != "Malaysia" & pq_use == "N" ~
          (
            high_dxcost_percase + high_bloodstage_cost + high_cost_pervisit
          ) * high_tsb_count,
        age_group == "adults" &
          country != "Malaysia" & pq_use == "Y" ~
          (
            high_dxcost_percase + high_bloodstage_cost + high_cost_pervisit
          ) * high_tsb_count +
          (pq_cost * (1 - preg.lact) * pq_prescribed_high) * high_tsb_count,
        age_group == "adults" & country == "Malaysia" ~
          (
            high_dxcost_percase + high_bloodstage_cost + high_cost_pervisit
          ) * high_tsb_count +
          (g6pd_dx_high * (1 - preg.lact)) * high_tsb_count +
          (pq_cost * (
            g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
          ) * (1 - preg.lact)) * high_tsb_count,
        age_group == "children" &
          country != "Malaysia" & pq_use == "N" ~
          (
            high_dxcost_percase + high_bloodstage_cost + high_cost_pervisit
          ) * high_tsb_count,
        age_group == "children" &
          country != "Malaysia" & pq_use == "Y" ~
          (
            high_dxcost_percase + high_bloodstage_cost + high_cost_pervisit
          ) * high_tsb_count +
          (pq_cost * pq_prescribed_high) * high_tsb_count,
        age_group == "children" & country == "Malaysia" ~
          (
            high_dxcost_percase + high_bloodstage_cost + high_cost_pervisit
          ) * high_tsb_count +
          (g6pd_dx_high * high_tsb_count) +
          (pq_cost * (
            g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
          )) * high_tsb_count,
        age_group == "infants" &
          country != "Malaysia" & pq_use == "N" ~
          (
            high_dxcost_percase + high_bloodstage_cost + high_cost_pervisit
          ) * high_tsb_count,
        age_group == "infants" &
          country != "Malaysia" & pq_use == "Y" ~
          (
            high_dxcost_percase + high_bloodstage_cost + high_cost_pervisit
          ) * high_tsb_count +
          (pq_cost * 0.8 * pq_prescribed_high) * high_tsb_count,
        age_group == "infants" & country == "Malaysia" ~
          (
            high_dxcost_percase + high_bloodstage_cost + high_cost_pervisit
          ) * high_tsb_count +
          (g6pd_dx_high * 0.8 * high_tsb_count) +
          (pq_cost * (
            g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
          )) * 0.8 * high_tsb_count
      )
  )

# Missing calculations?
lapply(baseline[-1], function(x)
  baseline$country[which(is.na(x))])
# No missing calculations



####   HOUSEHOLD COST DATA   ####

# Files - "hhcosts.xlsx" and "dayslost.xlsx" (Devine 2019)
household_costs.dat <- readxl::read_xlsx("inputs/hhcosts.xlsx")
# Total direct household costs per case by region
dayslost.dat <- readxl::read_xlsx("inputs/dayslost.xlsx")
# Indirect patient costs (days lost to illness/caring duties)

# Total household cost per case
baseline <- baseline %>%
  mutate(
    med_hhcost_percase =
      case_when(
        region == "AFRO" ~ as.numeric(household_costs.dat[which(household_costs.dat$`WHO region` ==
                                                                  "AFRO"), "Base"]),
        region == "EMRO" ~ as.numeric(household_costs.dat[which(household_costs.dat$`WHO region` ==
                                                                  "EMRO"), "Base"]),
        region == "PAHO" ~ as.numeric(household_costs.dat[which(household_costs.dat$`WHO region` ==
                                                                  "PAHO"), "Base"]),
        region == "SEARO" ~ as.numeric(household_costs.dat[which(household_costs.dat$`WHO region` ==
                                                                   "SEARO"), "Base"]),
        region == "WPRO" ~ as.numeric(household_costs.dat[which(household_costs.dat$`WHO region` ==
                                                                  "WPRO"), "Base"])
      ),
    low_hhcost_percase =
      case_when(
        region == "AFRO" ~ as.numeric(household_costs.dat[which(household_costs.dat$`WHO region` ==
                                                                  "AFRO"), "Low"]),
        region == "EMRO" ~ as.numeric(household_costs.dat[which(household_costs.dat$`WHO region` ==
                                                                  "EMRO"), "Low"]),
        region == "PAHO" ~ as.numeric(household_costs.dat[which(household_costs.dat$`WHO region` ==
                                                                  "PAHO"), "Low"]),
        region == "SEARO" ~ as.numeric(household_costs.dat[which(household_costs.dat$`WHO region` ==
                                                                   "SEARO"), "Low"]),
        region == "WPRO" ~ as.numeric(household_costs.dat[which(household_costs.dat$`WHO region` ==
                                                                  "WPRO"), "Low"])
      ),
    high_hhcost_percase =
      case_when(
        region == "AFRO" ~ as.numeric(household_costs.dat[which(household_costs.dat$`WHO region` ==
                                                                  "AFRO"), "High"]),
        region == "EMRO" ~ as.numeric(household_costs.dat[which(household_costs.dat$`WHO region` ==
                                                                  "EMRO"), "High"]),
        region == "PAHO" ~ as.numeric(household_costs.dat[which(household_costs.dat$`WHO region` ==
                                                                  "PAHO"), "High"]),
        region == "SEARO" ~ as.numeric(household_costs.dat[which(household_costs.dat$`WHO region` ==
                                                                   "SEARO"), "High"]),
        region == "WPRO" ~ as.numeric(household_costs.dat[which(household_costs.dat$`WHO region` ==
                                                                  "WPRO"), "High"])
      )
  )

# Days lost per case (applies to adults and children, not infants)
baseline <- baseline %>%
  mutate(
    med_dayslost_percase =
      case_when(
        region == "AFRO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                           "AFRO"), "patientdays"]),
        region == "EMRO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                           "EMRO"), "patientdays"]),
        region == "PAHO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                           "PAHO"), "patientdays"]),
        region == "SEARO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                            "SEARO"), "patientdays"]),
        region == "WPRO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                           "WPRO"), "patientdays"])
      ),
    low_dayslost_percase =
      case_when(
        region == "AFRO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                           "AFRO"), "patientdays.low"]),
        region == "EMRO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                           "EMRO"), "patientdays.low"]),
        region == "PAHO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                           "PAHO"), "patientdays.low"]),
        region == "SEARO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                            "SEARO"), "patientdays.low"]),
        region == "WPRO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                           "WPRO"), "patientdays.low"])
      ),
    high_dayslost_percase =
      case_when(
        region == "AFRO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                           "AFRO"), "patientdays.high"]),
        region == "EMRO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                           "EMRO"), "patientdays.high"]),
        region == "PAHO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                           "PAHO"), "patientdays.high"]),
        region == "SEARO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                            "SEARO"), "patientdays.high"]),
        region == "WPRO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                           "WPRO"), "patientdays.high"])
      )
  )

# Baseline scenario only applies productivity losses to children and adults only:
# Change the patient days lost to 0 for infants
baseline[(which(baseline$age_group == "infants")),
         c("med_dayslost_percase",
           "low_dayslost_percase",
           "high_dayslost_percase")] <- 0

# Carer days lost per case (applies to all age groups)
baseline <- baseline %>%
  mutate(
    med_carerdays_percase =
      case_when(
        region == "AFRO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                           "AFRO"), "carerdays"]),
        region == "EMRO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                           "EMRO"), "carerdays"]),
        region == "PAHO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                           "PAHO"), "carerdays"]),
        region == "SEARO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                            "SEARO"), "carerdays"]),
        region == "WPRO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                           "WPRO"), "carerdays"])
      ),
    low_carerdays_percase =
      case_when(
        region == "AFRO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                           "AFRO"), "carerdays.low"]),
        region == "EMRO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                           "EMRO"), "carerdays.low"]),
        region == "PAHO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                           "PAHO"), "carerdays.low"]),
        region == "SEARO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                            "SEARO"), "carerdays.low"]),
        region == "WPRO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                           "WPRO"), "carerdays.low"])
      ),
    high_carerdays_percase =
      case_when(
        region == "AFRO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                           "AFRO"), "carerdays.high"]),
        region == "EMRO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                           "EMRO"), "carerdays.high"]),
        region == "PAHO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                           "PAHO"), "carerdays.high"]),
        region == "SEARO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                            "SEARO"), "carerdays.high"]),
        region == "WPRO" ~ as.numeric(dayslost.dat[which(dayslost.dat$Country ==
                                                           "WPRO"), "carerdays.high"])
      )
  )

# Check if anything is missing.
lapply(baseline[-1], function(x)
  baseline$country[which(is.na(x))])
# No missing values

####   TOTAL PATIENT COSTS   ####

# Required calculations:
#         - Mean direct household costs = med_hhcost_percase * mean_tsb_count
#         - Low direct household costs = low_hhcost_percase * low_tsb_count
#         - High direct household costs = high_hhcost_percase * high_tsb_count
#
#         - Mean productivity cost per case = med_dayslost_percase * gdp2016_percapday
#         - Low productivity cost per case = low_dayslost_percase * gdp2016_percapday
#         - High productivity cost per case = high_dayslost_percase * gdp2016_percapday
#
#         - Mean carer productivity cost per case = med_carerdays_percase * gdp2016_percapday
#         - Low carer productivity cost per case = low_carerdays_percase * gdp2016_percapday
#         - High carer productivity cost per case = high_carerdays_percase * gdp2016_percapday
#
#         - Mean indirect household costs =
#                     (Mean productivity cost per case + mean carer productivity cost per case) * mean_incidence
#         - Low indirect household costs =
#                     (Low productivity cost per case + low carer productivity cost per case) * low_incidence
#         - High indirect household costs =
#                     (High productivity cost per case + high carer productivity cost per case) * high_incidence
#
#         - Mean total household costs = mean direct household costs + mean indirect household costs
#         - Low total household costs = low direct household costs + low indirect household costs
#         - High total household costs = high direct household costs + high indirect household costs

baseline <- baseline %>%
  mutate(
    mean_direct_patcost = med_hhcost_percase * mean_tsb_count,
    low_direct_patcost = low_hhcost_percase * low_tsb_count,
    high_direct_patcost = high_hhcost_percase * high_tsb_count,
    mean_lostprod_percase = med_dayslost_percase * gdp2017_percapday,
    low_lostprod_percase = low_dayslost_percase * gdp2017_percapday,
    high_lostprod_percase = high_dayslost_percase * gdp2017_percapday,
    mean_carerlost_percase = med_carerdays_percase * gdp2017_percapday,
    low_carerlost_percase = low_carerdays_percase * gdp2017_percapday,
    high_carerlost_percase = high_carerdays_percase * gdp2017_percapday,
    mean_indirect_patcost = (mean_lostprod_percase + mean_carerlost_percase) * mean_incidence,
    low_indirect_patcost = (low_lostprod_percase + low_carerlost_percase) * low_incidence,
    high_indirect_patcost = (high_lostprod_percase + high_carerlost_percase) * high_incidence,
    mean_total_patcost = mean_direct_patcost + mean_indirect_patcost,
    low_total_patcost = low_direct_patcost + low_indirect_patcost,
    high_total_patcost = high_direct_patcost + high_indirect_patcost
  )

# Check what's missing
lapply(baseline[-1], function(x)
  baseline$country[which(is.na(x))])
# No missing values



####   CALCULATING TOTAL COSTS   ####

# Required calculations:
#         - Total cost estimate = mean_total_provcost + mean_total_patcost
#         - Low cost estimate = low_total_provcost + low_total_patcost
#         - High cost estimate = high_total_provcost + high_total_patcost

baseline <- baseline %>%
  mutate(
    mean_total_cost = mean_total_provcost + mean_total_patcost,
    low_total_cost = low_total_provcost + low_total_patcost,
    high_total_cost = high_total_provcost + high_total_patcost
  )

is.na(baseline[, 75:77])
#    No missing values



####   ONE-WAY SENSITIVITY ANALYSIS   ####

# SA: Change days lost to illness to apply to adults only.
baseline <-
  baseline %>% mutate(
    sa_med_dayslost_percase = med_dayslost_percase,
    sa_low_dayslost_percase = low_dayslost_percase,
    sa_high_dayslost_percase = high_dayslost_percase
  )

# Change cost of days lost to illness to 0 for infants and children
baseline[(which(baseline$age_group != "adults")),
         c("sa_med_dayslost_percase",
           "sa_low_dayslost_percase",
           "sa_high_dayslost_percase")] <- 0

# Sensitivity analysis calculations
#         - Mean direct household costs = med_hhcost_percase * mean_tsb_count
#                    -> no change
#         - Mean productivity cost per case = sa_med_dayslost_percase * gdp2016_percapday
#                    -> should be lower
#         - Mean carer productivity cost per case = med_carerdays_percase * gdp2016_percapday
#                    -> no change
#         - Mean indirect household costs =
#               (Mean productivity cost per case + mean carer productivity cost per case) * mean_incidence
#                    -> should be lower
#         - Mean total household costs = mean direct household costs + mean indirect household costs
#                    -> should be lower
#         - Total cost estimate = mean_total_provcost + mean_total_patcost
#                    -> should be lower

baseline <- baseline %>%
  mutate(
    sa_mean_direct_patcost = med_hhcost_percase * mean_tsb_count,
    sa_mean_lostprod_percase = sa_med_dayslost_percase * gdp2017_percapday,
    sa_mean_carerlost_percase = med_carerdays_percase * gdp2017_percapday,
    sa_mean_indirect_patcost = (sa_mean_lostprod_percase + sa_mean_carerlost_percase) * mean_incidence,
    sa_mean_total_patcost = sa_mean_direct_patcost + sa_mean_indirect_patcost,
    sa_mean_total_cost = mean_total_provcost + sa_mean_total_patcost
  )

# Check what's missing
lapply(baseline[-1], function(x)
  baseline$country[which(is.na(x))])
# No missing values for countries that are included in the analysis



####   OUTPUT AS CSV   ####

output2 <- baseline %>%
  select(
    c(
      country,
      mean_total_provcost,
      mean_direct_patcost,
      mean_indirect_patcost,
      mean_total_cost,
      low_total_cost,
      high_total_cost,
      sa_mean_total_cost
    )
  ) %>%
  mutate_if(is.numeric, round) %>%
  group_by(country) %>%
  summarise_if(is.numeric, sum) %>%
  bind_rows(summarise_all(., list(function(x) if(is.numeric(x)) sum(x) else "Total"))) %>%
  rename(
    "Country" = country,
    "Provider Costs" = mean_total_provcost,
    "Direct Household Costs" = mean_direct_patcost,
    "Indirect Household Costs" = mean_indirect_patcost,
    "Mean Total Cost" = mean_total_cost,
    "Low Total Cost" = low_total_cost,
    "High Total Cost" = high_total_cost,
    "SA Estimate" = sa_mean_total_cost
  )
write.csv(output2, 'outputs/baseline.csv', row.names = FALSE)

# Save baseline data to be used for PSA & output tables
save.image(file = "outputs/baseline.Rdata")