#####################################################################################################################
#   R-script:     2-radical-cure.R
#   Project:		  "Global economic costs due to vivax malaria and the potential impact of its radical cure: A modelling study" by A Devine et al.
#
#   Data used:	  (1) "baseline.RData"
#                 (2) "DxCosts.xlsx" from Devine 2019 WHO Bull, de Oliveira 2012 Mal J, & Peixoto 2016 Mal J
#                 (3) "drugcosts.xlsx" from MSH 2015 Int'l Medical Products Price Guide & Hill 2018 BMJ Global Health
#
#   Data created:	"radcure.RData", "radcure_psa.RData", "RadicalCure_SupPQ_AgeGroups.csv", "RadicalCure_SupPQ.csv",
#                 "RadicalCure_UnsupPQ.csv" & "RadicalCure_UnsupPQ_AgeGroups.csv"
#
#   Purpose:  		Loading input data for the cost burden of P. vivax in 2017 USD in supervised and unsupervised radical cure scenarios
#
#   Date:			    14-April-2021
#   Author: 		  Niamh Meagher & Angela Devine
#####################################################################################################################

# NOTE: Parameters values are described as mean (base), low (low bound), & high (high bound)

rm(list = ls())

# Run the baseline analysis
source("1-baseline.R") 
rm(list = setdiff(ls(), "baseline"))



####   PROVIDER G6PD SCREENING COSTS   ####
# Load costs for G6PD screening per WHO region - "DxCosts.xlsx"

g6pddx_costs <- readxl::read_xlsx("inputs/DxCosts.xlsx")
## G6PD RDT costs
# AFRO
g6pd_afro_mean <-
  as.numeric(subset(g6pddx_costs, region == "AFRO" &
                      test == "gRDT")$base)
g6pd_afro_low <- g6pd_afro_mean - (0.5 * g6pd_afro_mean)
g6pd_afro_high <- g6pd_afro_mean + (0.5 * g6pd_afro_mean)
# EMRO
g6pd_emro_mean <-
  as.numeric(subset(g6pddx_costs, region == "EMRO" &
                      test == "gRDT")$base)
g6pd_emro_low <- g6pd_emro_mean - (0.5 * g6pd_emro_mean)
g6pd_emro_high <- g6pd_emro_mean + (0.5 * g6pd_emro_mean)
# PAHO
g6pd_paho_mean <-
  as.numeric(subset(g6pddx_costs, region == "PAHO" &
                      test == "gRDT")$base)
g6pd_paho_low <- g6pd_paho_mean - (0.5 * g6pd_paho_mean)
g6pd_paho_high <- g6pd_paho_mean + (0.5 * g6pd_paho_mean)
# SEARO is the mean of other 4 regions (except Indonesia has specific G6PD RDT cost)
g6pd_searo_mean <-
  as.numeric(subset(g6pddx_costs, region == "SEARO" &
                      test == "gRDT")$base)
g6pd_searo_low <- g6pd_searo_mean - (0.5 * g6pd_searo_mean)
g6pd_searo_high <- g6pd_searo_mean + (0.5 * g6pd_searo_mean)
# Indonesia
g6pd_indon_mean <-
  as.numeric(subset(g6pddx_costs, region == "Indonesia" &
                      test == "gRDT")$base)
g6pd_indon_low <- g6pd_indon_mean - (0.5 * g6pd_indon_mean)
g6pd_indon_high <-
  g6pd_indon_mean + (0.5 * g6pd_indon_mean)
# WPRO
g6pd_wpro_mean <-
  as.numeric(subset(g6pddx_costs, region == "WPRO" &
                      test == "gRDT")$base)
g6pd_wpro_low <- g6pd_wpro_mean - (0.5 * g6pd_wpro_mean)
g6pd_wpro_high <- g6pd_wpro_mean + (0.5 * g6pd_wpro_mean)

# FST cost for Malaysia since this is current practice
# WPRO
fst_wpro_mean <-
  as.numeric(subset(g6pddx_costs, region == "WPRO" &
                      test == "fst")$base)
fst_wpro_low <- fst_wpro_mean - (0.5 * fst_wpro_mean)
fst_wpro_high <- fst_wpro_mean + (0.5 * fst_wpro_mean)

# Prepare G6PD screening cost data by country
baseline <- baseline %>%
  select(-c("g6pd_dx_mean", "g6pd_dx_low", "g6pd_dx_high")) %>%
  mutate(
    g6pd_rdt_mean =
      case_when(
        region == "AFRO" ~ g6pd_afro_mean,
        region == "EMRO" ~ g6pd_emro_mean,
        region == "PAHO" ~ g6pd_paho_mean,
        region == "SEARO" &
          country != "Indonesia" ~ g6pd_searo_mean,
        region == "WPRO" ~ g6pd_wpro_mean,
        country == "Indonesia" ~ g6pd_indon_mean
      ),
    g6pd_rdt_low =
      case_when(
        region == "AFRO" ~ g6pd_afro_low,
        region == "EMRO" ~ g6pd_emro_low,
        region == "PAHO" ~ g6pd_paho_low,
        region == "SEARO" &
          country != "Indonesia" ~ g6pd_searo_low,
        region == "WPRO" ~ g6pd_wpro_low,
        country == "Indonesia" ~ g6pd_indon_low
      ),
    g6pd_rdt_high =
      case_when(
        region == "AFRO" ~ g6pd_afro_high,
        region == "EMRO" ~ g6pd_emro_high,
        region == "PAHO" ~ g6pd_paho_high,
        region == "SEARO" &
          country != "Indonesia" ~ g6pd_searo_high,
        region == "WPRO" ~ g6pd_wpro_high,
        country == "Indonesia" ~ g6pd_indon_high
      )
  )

rm(list = setdiff(
  ls(),
  c(
    "baseline",
    "g6pddx_costs",
    "fst_wpro_mean",
    "fst_wpro_low",
    "fst_wpro_high"
  )
))



######   CALCULATIONS FOR PERCENT REDUCTION IN CASES   ######

# Proportion of recurrent cases prevented by full course of high-dose primaquine
# This is from Commons 2020 AJTMH
mean_PQ.base = 0.88
low_PQ.base = 0.82
high_PQ.base = 0.92



####   POPULATION ELIGIBLE FOR G6PD TESTING   ####
# Get "a" parameters - these are used to calculate cases averted
# pop.tested.a - the proportion of those who would be tested for G6PD deficiency 
# Assumptions about how many people will receive a G6PD test:
#   Infants - Assumed 20% of these ineligible because less than 1 year old
#   Children - All eligible
#   Adults - All eligible except those who are pregnant or lactating - 'preg.lact'  
# Rounded to nearest integer
baseline <- baseline %>%
  mutate(
    pop.tested.a_mean =
      case_when(
        age_group == "infants" ~ mean_tsb_count * (1 - 0.20),
        age_group == "children" ~ mean_tsb_count,
        age_group == "adults" ~ (1 - preg.lact) *
          mean_tsb_count
      ),
    pop.tested.a_low =
      case_when(
        age_group == "infants" ~ low_tsb_count * (1 - 0.20),
        age_group == "children" ~ low_tsb_count,
        age_group == "adults" ~ (1 - preg.lact) *
          low_tsb_count
      ),
    pop.tested.a_high =
      case_when(
        age_group == "infants" ~ high_tsb_count * (1 - 0.20),
        age_group == "children" ~ high_tsb_count,
        age_group == "adults" ~ (1 - preg.lact) *
          high_tsb_count
      )
  ) %>%
  mutate_at(vars(pop.tested.a_mean, pop.tested.a_low, pop.tested.a_high),
            ~round(., 0))



####   POPULATION RECEIVING PQ   ####
# Calculate the population that could potentially receive an effective dose of primaquine (PQ.pop.eff in S2_File)
# This depends on the sensitivity and specificity of G6PD RDT (from Ley 2019 PlosMed)
g6pd.sens <- 0.96
g6pd.sens.ll <- 0.90
g6pd.sens.ul <- 0.99
g6pd.spec <- 0.95
g6pd.spec.ll <- 0.92
g6pd.spec.ul <- 0.96
# And the proportion of people who have G6PD deficiency 
# Assume that PQ will be prescribed to all who test G6PD negative (i.e. true negatives & false negatives)
# But that PQ won't be effective population in the false negatives (so these are excluded from the effective population)
# Rounded to the nearest integer
baseline <- baseline %>%
  mutate(
    PQ.pop.a_mean = pop.tested.a_mean * ((
      g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
    )),
    PQ.pop.eff.a_mean = pop.tested.a_mean * ((1 - g6pdd) * g6pd.spec),
    PQ.pop.a_low = pop.tested.a_low * ((
      g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
    )),
    PQ.pop.eff.a_low = pop.tested.a_low * ((1 - g6pdd) * g6pd.spec),
    PQ.pop.a_high = pop.tested.a_high * ((
      g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
    )),
    PQ.pop.eff.a_high = pop.tested.a_high * ((1 - g6pdd) * g6pd.spec)
  ) %>%
  # Also want to count # of G6PDd who receive PQ
  mutate(G6PDd.recPQ = pop.tested.a_mean * (g6pdd * (1 - g6pd.sens))) %>%
  mutate_at(
    vars(
      PQ.pop.a_mean,
      PQ.pop.a_low,
      PQ.pop.a_high,
      PQ.pop.eff.a_mean,
      PQ.pop.eff.a_low,
      PQ.pop.eff.a_high,
      G6PDd.recPQ
    ),
    ~round(., 0)
  )



####  ESTIMATE THE EFFECTIVENESS OF PQ FROM THE BASELINE ANALYSIS   ####
# Applied to patients who seek treatment

# Proportion of patients who are prescribed primaquine in baseline
# The base and low values are assumptions. The high value is from Douglas 2017 PlosMed
pq_prescribed_mean <- 0.4
pq_prescribed_low <- 0.1
pq_prescribed_high <- 0.8

# Effectiveness of primaquine without supervision
# The base and high values are assumptions. The low value is from Douglas 2017 PlosMed
unsupervised.eff_mean <- 0.4
unsupervised.eff_low <- 0.10 
unsupervised.eff_high <- 0.70

# Calculate the usual proportion of individuals who are assumed to receive an effective dose of PQ in the baseline analysis (baseline.PQ in S4_File).
usual.PQ_mean <- pq_prescribed_mean * unsupervised.eff_mean
usual.PQ_low <- pq_prescribed_low * unsupervised.eff_low
usual.PQ_high <- pq_prescribed_high * unsupervised.eff_high



######  SUPERVISED RADICAL CURE SCENARIO   ######

# Create copy of data to refer back to for unsupervised high dose PQ scenario
baseline2 <- baseline

###
# Calculate Incidence for Supervised Strategy
###
# STEP 1: Cases averted - use baseline estimates to calculate the number of cases averted by implementing supervised radical cure 
# Accounting for reduced cases in countries that already use PQ
# Round to nearest integer
baseline <- baseline %>%
  mutate(
    cases.av =
      case_when(
        pq_use == "N" ~ PQ.pop.eff.a_mean * mean_PQ.base,
        pq_use == "Y" ~ PQ.pop.eff.a_mean * (mean_PQ.base -
                                               usual.PQ_mean)
      )
  ) %>%
  mutate_at(vars(cases.av), round, 0)

# STEP 2: New incidence values - subtract cases averted from the baseline incidence to get an updated
#                                incidence to calculate the costs for the radical cure scenario.
baseline <- baseline %>%
  mutate(sup.incidence = mean_incidence - cases.av)

# STEP 3: Get "b" parameters - these are to be used in cost calculations to determine the new costs
#                              associated with the scenario
#                            - uses a single new incidence estimate, but applies other low/high values
# Population seeking treatment
baseline <- baseline %>%
  mutate(
    sup.mean_tsb_count = sup.incidence * mean_tsb_prop,
    sup.low_tsb_count = sup.incidence * low_tsb_prop,
    sup.high_tsb_count = sup.incidence * high_tsb_prop
  ) %>%
  mutate_at(vars(sup.mean_tsb_count, sup.low_tsb_count, sup.high_tsb_count),
            round,
            0)

# Population receiving G6PD tests - pop.tested.b
baseline <- baseline %>%
  mutate(
    pop.tested.b_mean =
      case_when(
        age_group == "infants" ~ sup.mean_tsb_count * (1 - 0.20),
        age_group == "children" ~ sup.mean_tsb_count,
        age_group == "adults" ~ (1 - preg.lact) * sup.mean_tsb_count
      ),
    pop.tested.b_low =
      case_when(
        age_group == "infants" ~ sup.low_tsb_count * (1 - 0.20),
        age_group == "children" ~ sup.low_tsb_count,
        age_group == "adults" ~ (1 - preg.lact) * sup.low_tsb_count
      ),
    pop.tested.b_high =
      case_when(
        age_group == "infants" ~ sup.high_tsb_count * (1 - 0.20),
        age_group == "children" ~ sup.high_tsb_count,
        age_group == "adults" ~ (1 - preg.lact) * sup.high_tsb_count
      )
  ) %>%
  mutate_at(vars(pop.tested.b_mean, pop.tested.b_low, pop.tested.b_high),
            ~round(., 0))

# Calculate population prescribed PQ - PQ.pop.b 
# Calculate population prescribed PQ who receive an effective dose of PQ - PQ.pop.eff.b
baseline <- baseline %>%
  mutate(
    PQ.pop.b_mean = baseline$pop.tested.b_mean * ((
      g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
    )),
    PQ.pop.eff.b_mean = pop.tested.b_mean * ((1 - g6pdd) * g6pd.spec),
    PQ.pop.b_low = pop.tested.b_low * ((
      g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
    )),
    PQ.pop.eff.b_low = pop.tested.b_low * ((1 - g6pdd) * g6pd.spec),
    PQ.pop.b_high = pop.tested.b_high * ((
      g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
    )),
    PQ.pop.eff.b_high = pop.tested.b_high * ((1 - g6pdd) * g6pd.spec)
  ) %>%
  mutate_at(
    vars(
      PQ.pop.b_mean,
      PQ.pop.b_low,
      PQ.pop.b_high,
      PQ.pop.eff.b_mean,
      PQ.pop.eff.b_low,
      PQ.pop.eff.b_high
    ),
    ~round(., 0)
  )

###
# Calculate  Costs for Supervised Radical Cure scenario
###
# Cost of screening
baseline <- baseline %>%
  mutate(
    cScreening = case_when(
      country != "Malaysia" ~ pop.tested.b_mean * g6pd_rdt_mean,
      country == "Malaysia" ~ pop.tested.b_mean *
        fst_wpro_mean
    )
  )
# Cost of PQ supervision 
# Assuming one hour per visit for 13 days and an 8 hour day using GDP/capita/day
baseline <- baseline %>%
  mutate(
    hd_cSupervision_base = PQ.pop.b_mean * ((gdp2017_percapday / 8) * 13),
    hd_cSupervision_sa1 = PQ.pop.b_mean * ((gdp2017_percapday / 8) *
                                             6),
    hd_cSupervision_sa2 = PQ.pop.b_mean * ((gdp2017_percapday / 8) *
                                             1)
  )

# PQ cost - 'drugcosts.xlsx'   
drugcost.dat <- readxl::read_xlsx("inputs/drugcosts.xlsx")
# Using high dose PQ so need to double the low dose cost
pq_lowdose_cost <- as.numeric(drugcost.dat[1, "Cost"])
pq_highdose_cost <- 2 * pq_lowdose_cost
rm(drugcost.dat)

# Cost of Patient Management
# Provider cost per person treated 
# Provide cost includes: 1) Cost of visit/case seeking treatment
#                        2) Cost of malaria Dx/case seeking treatment
#                        3) Cost of bloodstage cure/case seeking treatment
#                        4) Cost of high dose PQ for those eligible and testing G6PD normal
baseline <- baseline %>%
  mutate(
    cCare = (
      mean_cost_pervisit + mean_dxcost_percase + mean_bloodstage_cost
    ) * sup.mean_tsb_count +
      (pq_highdose_cost * PQ.pop.b_mean)
  )


### OVERALL COST ESTIMATES
# Total Provider Cost (supervised radical cure)
#    Cost of Screening + Cost of Supervision + Cost per person treated
baseline <- baseline %>%
  mutate(sup.total_provcost = cScreening + hd_cSupervision_base + cCare)

# Direct Household Costs
#    Direct Cost Per Case * Number Seeking Treating
baseline <- baseline %>%
  mutate(sup.dirhh_cost = med_hhcost_percase * sup.mean_tsb_count)

# Indirect Household Costs
#    (Lost productivity per case + Carer days lost per case) * Incidence
baseline <- baseline %>%
  mutate(sup.indirhh_cost = (mean_lostprod_percase + mean_carerlost_percase) * sup.incidence)

# Total Household Costs
#    Direct Household Costs + Indirect Household Costs
baseline <- baseline %>%
  mutate(sup.totalhh_cost = sup.dirhh_cost + sup.indirhh_cost)

# Overall costs
#     Total Provider Cost + Total Household Costs
baseline <- baseline %>%
  mutate(sup.overallcost_mean = sup.total_provcost + sup.totalhh_cost)




####   SENSITIVITY ANALYSIS   ####
# EFFECTIVENESS OF PQ: Proportion of recurrent cases prevented by full course of high-dose primaquine
# One-way SA for effectiveness of PQ - changes all calculations so repeat new analysis
effect_sa1 <- baseline
effect_sa2 <- baseline


# SA1 - Low effective PQ
###
# Calculate Incidence for Supervised Strategy
###
# STEP 1: Cases averted - use baseline estimates to calculate the number of cases averted by implementing
#                         supervised radical cure 
# Account for reduced cases in countries that already use PQ
# Round to nearest integer
effect_sa1 <- effect_sa1 %>%
  mutate(
    cases.av =
      case_when(
        pq_use == "N" ~ PQ.pop.eff.a_mean * low_PQ.base,
        pq_use == "Y" ~ PQ.pop.eff.a_mean * (low_PQ.base -
                                               usual.PQ_mean)
      )
  ) %>%
  mutate_at(vars(cases.av), round, 0)

# STEP 2: New incidence values - subtract cases averted from the baseline incidence to get an updated
#                                incidence to calculate the costs for the radical cure scenario.
effect_sa1 <- effect_sa1 %>%
  mutate(sup.incidence = mean_incidence - cases.av)

# STEP 3: Get "b" parameters - these are to be used in cost calculations to determine the new costs
#                              associated with the scenario
#                            - uses a single new incidence estimate, but applies other low/high parameter values parameters
# Population seeking treatment
effect_sa1 <- effect_sa1 %>%
  mutate(
    sup.mean_tsb_count = sup.incidence * mean_tsb_prop,
    sup.low_tsb_count = sup.incidence * low_tsb_prop,
    sup.high_tsb_count = sup.incidence * high_tsb_prop
  ) %>%
  mutate_at(vars(sup.mean_tsb_count, sup.low_tsb_count, sup.high_tsb_count),
            round,
            0)
# Population receiving G6PD tests - pop.tested.b
effect_sa1 <- effect_sa1 %>%
  mutate(
    pop.tested.b_mean =
      case_when(
        age_group == "infants" ~ sup.mean_tsb_count * (1 - 0.20),
        age_group == "children" ~ sup.mean_tsb_count,
        age_group == "adults" ~ (1 - preg.lact) * sup.mean_tsb_count
      ),
    pop.tested.b_low =
      case_when(
        age_group == "infants" ~ sup.low_tsb_count * (1 - 0.20),
        age_group == "children" ~ sup.low_tsb_count,
        age_group == "adults" ~ (1 - preg.lact) * sup.low_tsb_count
      ),
    pop.tested.b_high =
      case_when(
        age_group == "infants" ~ sup.high_tsb_count * (1 - 0.20),
        age_group == "children" ~ sup.high_tsb_count,
        age_group == "adults" ~ (1 - preg.lact) * sup.high_tsb_count
      )
  ) %>%
  mutate_at(vars(pop.tested.b_mean, pop.tested.b_low, pop.tested.b_high),
            ~round(., 0))
# Calculate population prescribed PQ - PQ.pop.b 
# Calculate population prescribed PQ who receive an effective dose of PQ
effect_sa1 <- effect_sa1 %>%
  mutate(
    PQ.pop.b_mean = pop.tested.b_mean * ((
      g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
    )),
    PQ.pop.eff.b_mean = pop.tested.b_mean * ((1 - g6pdd) * g6pd.spec),
    PQ.pop.b_low = pop.tested.b_low * ((
      g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
    )),
    PQ.pop.eff.b_low = pop.tested.b_low * ((1 - g6pdd) * g6pd.spec),
    PQ.pop.b_high = pop.tested.b_high * ((
      g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
    )),
    PQ.pop.eff.b_high = pop.tested.b_high * ((1 - g6pdd) * g6pd.spec)
  ) %>%
  mutate_at(
    vars(
      PQ.pop.b_mean,
      PQ.pop.b_low,
      PQ.pop.b_high,
      PQ.pop.eff.b_mean,
      PQ.pop.eff.b_low,
      PQ.pop.eff.b_high
    ),
    ~round(., 0)
  )

###
# Calculate  Costs for Supervised Strategy
###
# Cost of screening
effect_sa1 <- effect_sa1 %>%
  mutate(
    cScreening = case_when(
      country != "Malaysia" ~ pop.tested.b_mean * g6pd_rdt_mean,
      country == "Malaysia" ~ pop.tested.b_mean *
        fst_wpro_mean
    )
  )
# Cost of supervision 
effect_sa1 <- effect_sa1 %>%
  mutate(hd_cSupervision_base = PQ.pop.b_mean * ((gdp2017_percapday /
                                                    8) * 13))

# Cost of Patient Management
# Provider cost per person treated 
# Provide cost includes: 1) Cost of visit/case seeking treatment
#                        2) Cost of malaria Dx/case seeking treatment
#                        3) Cost of bloodstage cure/case seeking treatment
#                        4) Cost of high dose PQ for those eligible and testing G6PD normal
effect_sa1 <- effect_sa1 %>%
  mutate(
    cCare = (
      mean_cost_pervisit + mean_dxcost_percase + mean_bloodstage_cost
    ) * sup.mean_tsb_count +
      (pq_highdose_cost * PQ.pop.b_mean)
  )

# OVERALL COST ESTIMATES
# Total Provider Cost (supervised radical cure)
#    Cost of Screening + Cost of Supervision + Cost of Care
effect_sa1 <- effect_sa1 %>%
  mutate(sup.total_provcost = cScreening + hd_cSupervision_base + cCare)

# Direct Household Costs
#    Direct Cost Per Case * Number Seeking Treating
effect_sa1 <- effect_sa1 %>%
  mutate(sup.dirhh_cost = med_hhcost_percase * sup.mean_tsb_count)

# Indirect Household Costs
#    (Lost productivity per case + Carer days lost per case) * Incidence
effect_sa1 <- effect_sa1 %>%
  mutate(sup.indirhh_cost = (mean_lostprod_percase + mean_carerlost_percase) * sup.incidence)

# Total Household Costs
#    Direct Household Costs + Indirect Household Costs
effect_sa1 <- effect_sa1 %>%
  mutate(sup.totalhh_cost = sup.dirhh_cost + sup.indirhh_cost)

# Overall costs
#     Total Provider Cost + Total Household Costs
effect_sa1 <- effect_sa1 %>%
  mutate(sup.overallcost_mean = sup.total_provcost + sup.totalhh_cost)


# SA2 - High effective PQ
###
# Calculate Incidence for Supervised Strategy
###
# STEP 1: Cases averted - use baseline estimates to calculate the number of cases averted by implementing
#                         supervised radical cure 
# Account for reduced cases in countries that already use PQ
# Round to nearest integer
effect_sa2 <- effect_sa2 %>%
  mutate(
    cases.av =
      case_when(
        pq_use == "N" ~ PQ.pop.eff.a_mean * high_PQ.base,
        pq_use == "Y" ~ PQ.pop.eff.a_mean * (high_PQ.base -
                                               usual.PQ_mean)
      )
  ) %>%
  mutate_at(vars(cases.av), round, 0)

# STEP 2: New incidence values - subtract cases averted from the baseline incidence to get an updated
#                                incidence to calculate the costs for the radical cure scenario.
effect_sa2 <- effect_sa2 %>%
  mutate(sup.incidence = mean_incidence - cases.av)

# STEP 3: Get "b" parameters - these are to be used in cost calculations to determine the new costs
#                              associated with the scenario
#                            - uses a single new incidence estimate, but applies other low/high parameters
# Population seeking treatment
effect_sa2 <- effect_sa2 %>%
  mutate(
    sup.mean_tsb_count = sup.incidence * mean_tsb_prop,
    sup.low_tsb_count = sup.incidence * low_tsb_prop,
    sup.high_tsb_count = sup.incidence * high_tsb_prop
  ) %>%
  mutate_at(vars(sup.mean_tsb_count, sup.low_tsb_count, sup.high_tsb_count),
            round,
            0)
# Population receiving G6PD tests - pop.tested.b
effect_sa2 <- effect_sa2 %>%
  mutate(
    pop.tested.b_mean =
      case_when(
        age_group == "infants" ~ sup.mean_tsb_count * (1 - 0.20),
        age_group == "children" ~ sup.mean_tsb_count,
        age_group == "adults" ~ (1 - preg.lact) * sup.mean_tsb_count
      ),
    pop.tested.b_low =
      case_when(
        age_group == "infants" ~ sup.low_tsb_count * (1 - 0.20),
        age_group == "children" ~ sup.low_tsb_count,
        age_group == "adults" ~ (1 - preg.lact) * sup.low_tsb_count
      ),
    pop.tested.b_high =
      case_when(
        age_group == "infants" ~ sup.high_tsb_count * (1 - 0.20),
        age_group == "children" ~ sup.high_tsb_count,
        age_group == "adults" ~ (1 - preg.lact) * sup.high_tsb_count
      )
  ) %>%
  mutate_at(vars(pop.tested.b_mean, pop.tested.b_low, pop.tested.b_high),
            ~round(., 0))
# Calculate population prescribed PQ - PQ.pop.b 
# Calculate population prescribed PQ who receive an effective dose of PQ - PQ.pop.eff.b
effect_sa2 <- effect_sa2 %>%
  mutate(
    PQ.pop.b_mean = pop.tested.b_mean * ((
      g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
    )),
    PQ.pop.eff.b_mean = pop.tested.b_mean * ((1 - g6pdd) * g6pd.spec),
    PQ.pop.b_low = pop.tested.b_low * ((
      g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
    )),
    PQ.pop.eff.b_low = pop.tested.b_low * ((1 - g6pdd) * g6pd.spec),
    PQ.pop.b_high = pop.tested.b_high * ((
      g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
    )),
    PQ.pop.eff.b_high = pop.tested.b_high * ((1 - g6pdd) * g6pd.spec)
  ) %>%
  mutate_at(
    vars(
      PQ.pop.b_mean,
      PQ.pop.b_low,
      PQ.pop.b_high,
      PQ.pop.eff.b_mean,
      PQ.pop.eff.b_low,
      PQ.pop.eff.b_high
    ),
    ~round(., 0)
  )

###
# Calculate  Costs for Supervised Strategy
###
# Cost of screening
effect_sa2 <- effect_sa2 %>%
  mutate(
    cScreening = case_when(
      country != "Malaysia" ~ pop.tested.b_mean * g6pd_rdt_mean,
      country == "Malaysia" ~ pop.tested.b_mean *
        fst_wpro_mean
    )
  )
# Cost of supervision 
effect_sa2 <- effect_sa2 %>%
  mutate(hd_cSupervision_base = PQ.pop.b_mean * ((gdp2017_percapday /
                                                    8) * 13))

# Cost of Patient Management
# Provider cost per person treated 
# Provide cost includes: 1) Cost of visit/case seeking treatment
#                        2) Cost of malaria Dx/case seeking treatment
#                        3) Cost of bloodstage cure/case seeking treatment
#                        4) Cost of high dose PQ for those eligible and testing G6PD normal
effect_sa2 <- effect_sa2 %>%
  mutate(
    cCare = (
      mean_cost_pervisit + mean_dxcost_percase + mean_bloodstage_cost
    ) * sup.mean_tsb_count +
      (pq_highdose_cost * PQ.pop.b_mean)
  )

# OVERALL COST ESTIMATES
# Total Provider Cost (supervised radical cure)
#    Cost of Screening + Cost of Supervision + Cost of Care
effect_sa2 <- effect_sa2 %>%
  mutate(sup.total_provcost = cScreening + hd_cSupervision_base + cCare)

# Direct Household Costs
#    Direct Cost Per Case * Number Seeking Treating
effect_sa2 <- effect_sa2 %>%
  mutate(sup.dirhh_cost = med_hhcost_percase * sup.mean_tsb_count)

# Indirect Household Costs
#    (Lost productivity per case + Carer days lost per case) * Incidence
effect_sa2 <- effect_sa2 %>%
  mutate(sup.indirhh_cost = (mean_lostprod_percase + mean_carerlost_percase) * sup.incidence)

# Total Household Costs
#    Direct Household Costs + Indirect Household Costs
effect_sa2 <- effect_sa2 %>%
  mutate(sup.totalhh_cost = sup.dirhh_cost + sup.indirhh_cost)

# Overall costs
#    Total Provider Cost + Total Household Costs
effect_sa2 <- effect_sa2 %>%
  mutate(sup.overallcost_mean = sup.total_provcost + sup.totalhh_cost)



# DAYS OF SUPERVISION
# One-way SA for days required for supervision
# SA 1: 1.5 days required for supervision.
# SA 2: 0.25 days required for supervision
#  This changes the following calculations:
#     1) Total Provider Cost
#     2) Total Costs
baseline <- baseline %>%
  mutate(
    sup.total_provcost_sa1 = cScreening + hd_cSupervision_sa1 + cCare,
    sup.total_provcost_sa2 = cScreening + hd_cSupervision_sa2 + cCare,
    sup.overallcost_mean_sa1 = sup.total_provcost_sa1 + sup.totalhh_cost,
    sup.overallcost_mean_sa2 = sup.total_provcost_sa2 + sup.totalhh_cost
  )




####   OUTPUT RESULTS AS CSV   ####
output <- baseline %>%
  select(
    c(
      country,
      age_group,
      sup.total_provcost,
      sup.dirhh_cost,
      sup.indirhh_cost,
      sup.totalhh_cost,
      sup.overallcost_mean,
      sup.overallcost_mean_sa1,
      sup.overallcost_mean_sa2
    )
  ) %>%
  mutate_if(is.numeric, round) %>%
  bind_rows(summarise_all(., list(function(x) if(is.numeric(x)) sum(x) else "Total"))) %>%
  rename(
    "Country" = country,
    "Age Group" = age_group,
    "Total Provider Cost" = sup.total_provcost,
    "Direct Household Cost" = sup.dirhh_cost,
    "Indirect Household Cost" = sup.indirhh_cost,
    "Total Household Cost" = sup.totalhh_cost,
    "Overall Cost (Mean)" = sup.overallcost_mean,
    "SA: 1.5 Days Supervision" = sup.overallcost_mean_sa1,
    "SA: 0.25 Days Supervision" = sup.overallcost_mean_sa2
  )
write.csv(output,
          'outputs/RadicalCure_SupPQ_AgeGroups.csv',
          row.names = FALSE)

output2 <- baseline %>%
  select(
    c(
      country,
      age_group,
      sup.total_provcost,
      sup.dirhh_cost,
      sup.indirhh_cost,
      sup.totalhh_cost,
      sup.overallcost_mean,
      sup.overallcost_mean_sa1,
      sup.overallcost_mean_sa2
    )
  ) %>%
  mutate_if(is.numeric, round) %>%
  group_by(country) %>%
  summarise_if(is.numeric, sum) %>%
  bind_rows(summarise_all(., list(function(x) if(is.numeric(x)) sum(x) else "Total"))) %>%
  rename(
    "Country" = country,
    "Total Provider Cost" = sup.total_provcost,
    "Direct Household Cost" = sup.dirhh_cost,
    "Indirect Household Cost" = sup.indirhh_cost,
    "Total Household Cost" = sup.totalhh_cost,
    "Overall Cost (Mean)" = sup.overallcost_mean,
    "SA: 1.5 Days Supervision" = sup.overallcost_mean_sa1,
    "SA: 0.25 Days Supervision" = sup.overallcost_mean_sa2
  )
write.csv(output2, 'outputs/RadicalCure_SupPQ.csv', row.names =
            FALSE)

radicalcure_sup <- baseline







######  UNSUPERVISED RADICAL CURE SCENARIO   ######

# Reload original dataframe
baseline <- baseline2
rm(baseline2)

###
# Calculate Incidence for Unsupervised Strategy
###
# STEP 1: Cases averted - use baseline estimates to calculate the number of cases averted by implementing
#                         unsupervised radical cure 
# Account for reduced cases in countries that already use PQ
# Round to nearest integer
baseline <- baseline %>%
  mutate(
    cases.av =
      case_when(
        pq_use == "N" ~
          PQ.pop.eff.a_mean * unsupervised.eff_mean,
        pq_use == "Y" ~
          PQ.pop.eff.a_mean * (
            unsupervised.eff_mean - (pq_prescribed_mean * unsupervised.eff_mean)
          )
      )
  ) %>%
  mutate_at(vars(cases.av), round, 0)

# STEP 2: New incidence values - subtract cases averted from the baseline incidence to get an updated
#                                incidence to calculate the costs for the radical cure scenario.
baseline <- baseline %>%
  mutate(unsup.incidence = mean_incidence - cases.av)

# STEP 3: Get "b" parameters - these are to be used in cost calculations to determine the new costs
#                              associated with the scenario
#                            - uses a single new incidence estimate, but applies other low/high parameters
# Population seeking treatment
baseline <- baseline %>%
  mutate(
    unsup.mean_tsb_count = unsup.incidence * mean_tsb_prop,
    unsup.low_tsb_count = unsup.incidence * low_tsb_prop,
    unsup.high_tsb_count = unsup.incidence * high_tsb_prop
  ) %>%
  mutate_at(vars(
    unsup.mean_tsb_count,
    unsup.low_tsb_count,
    unsup.high_tsb_count
  ),
  round,
  0)
# Population receiving G6PD tests - pop.tested.b
baseline <- baseline %>%
  mutate(
    pop.tested.b_mean =
      case_when(
        age_group == "infants" ~ unsup.mean_tsb_count * (1 - 0.2),
        age_group == "children" ~ unsup.mean_tsb_count,
        age_group == "adults" ~ (1 - preg.lact) * unsup.mean_tsb_count
      ),
    pop.tested.b_low =
      case_when(
        age_group == "infants" ~ unsup.low_tsb_count * (1 - 0.2),
        age_group == "children" ~ unsup.low_tsb_count,
        age_group == "adults" ~ (1 - preg.lact) * unsup.low_tsb_count
      ),
    pop.tested.b_high =
      case_when(
        age_group == "infants" ~ unsup.high_tsb_count * (1 - 0.2),
        age_group == "children" ~ unsup.high_tsb_count,
        age_group == "adults" ~ (1 - preg.lact) * unsup.high_tsb_count
      )
  ) %>%
  mutate_at(vars(pop.tested.b_mean, pop.tested.b_low, pop.tested.b_high),
            ~round(., 0))
# Calculate population prescribed PQ - PQ.pop.b 
# Calculate population prescribed PQ who receive an effective dose of PQ - PQ.pop.eff.b
baseline <- baseline %>%
  mutate(
    PQ.pop.b_mean = pop.tested.b_mean * ((
      g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
    )),
    PQ.pop.eff.b_mean = pop.tested.b_mean * ((1 - g6pdd) * g6pd.spec),
    PQ.pop.b_low = pop.tested.b_low * ((
      g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
    )),
    PQ.pop.eff.b_low = pop.tested.b_low * ((1 - g6pdd) * g6pd.spec),
    PQ.pop.b_high = pop.tested.b_high * ((
      g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
    )),
    PQ.pop.eff.b_high = pop.tested.b_high * ((1 - g6pdd) * g6pd.spec)
  ) %>%
  mutate_at(
    vars(
      PQ.pop.b_mean,
      PQ.pop.b_low,
      PQ.pop.b_high,
      PQ.pop.eff.b_mean,
      PQ.pop.eff.b_low,
      PQ.pop.eff.b_high
    ),
    ~round(., 0)
  )


###
# Calculate Costs for Unsupervised Strategy
###
# Cost of screening
baseline <- baseline %>%
  mutate(
    cScreening = case_when(
      country != "Malaysia" ~ pop.tested.b_mean * g6pd_rdt_mean,
      country == "Malaysia" ~ pop.tested.b_mean *
        fst_wpro_mean
    )
  )

# Cost of Patient Management
# Provider cost per person treated 
# Provide cost includes: 1) Cost of visit/case seeking treatment
#                        2) Cost of malaria Dx/case seeking treatment
#                        3) Cost of bloodstage cure/case seeking treatment
#                        4) Cost of high dose PQ for those eligible and testing G6PD normal
baseline <- baseline %>%
  mutate(
    cCare = (
      mean_cost_pervisit + mean_dxcost_percase + mean_bloodstage_cost
    ) * unsup.mean_tsb_count +
      (pq_highdose_cost * PQ.pop.b_mean)
  )

# OVERALL COST ESTIMATES
# Total Provider Cost (unsupervised radical cure)
#    Cost of Screening + Cost of Care
baseline <- baseline %>%
  mutate(unsup.total_provcost = cScreening + cCare)

# Direct Household Costs
#    Direct Cost Per Case * Number Seeking Treating
baseline <- baseline %>%
  mutate(unsup.dirhh_cost = med_hhcost_percase * unsup.mean_tsb_count)

# Indirect Household Costs
#    (Lost productivity per case + Carer days lost per case) * Incidence
baseline <- baseline %>%
  mutate(
    unsup.indirhh_cost = (mean_lostprod_percase + mean_carerlost_percase) * unsup.incidence
  )

# Total Household Costs
#    Direct Household Costs + Indirect Household Costs
baseline <- baseline %>%
  mutate(unsup.totalhh_cost = unsup.dirhh_cost + unsup.indirhh_cost)

# Overall costs
#     Total Provider Cost + Total Household Costs
baseline <- baseline %>%
  mutate(unsup.overallcost_mean = unsup.total_provcost + unsup.totalhh_cost)




####   SENSITIVITY ANALYSIS   ####
# Effectiveness of primaquine without supervision: Proportion of recurrent cases prevented by unsupervised high-dose primaquine
# One-way SA for effectiveness of unsupervised PQ - changes all calculations so repeat new analysis
efficacy_sa1 <- baseline
efficacy_sa2 <- baseline


# SA1 - High unsupervised effectiveness of PQ
###
# Calculate Incidence for Unsupervised Strategy
###
# STEP 1: Cases averted - use baseline estimates to calculate the number of cases averted by implementing
#                         unsupervised radical cure 
# Account for reduced cases in countries that already use PQ
# Round to nearest integer
efficacy_sa1 <- efficacy_sa1 %>%
  mutate(
    cases.av =
      case_when(
        pq_use == "N" ~
          PQ.pop.eff.a_mean * unsupervised.eff_high,
        pq_use == "Y" ~
          PQ.pop.eff.a_mean * (
            unsupervised.eff_high - (pq_prescribed_mean * unsupervised.eff_high)
          )
      )
  ) %>%
  mutate_at(vars(cases.av), round, 0)

# STEP 2: New incidence values - subtract cases averted from the baseline incidence to get an updated
#                                incidence to calculate the costs for the radical cure scenario.
efficacy_sa1 <- efficacy_sa1 %>%
  mutate(unsup.incidence = mean_incidence - cases.av)

# STEP 3: Get "b" parameters - these are to be used in cost calculations to determine the new costs
#                              associated with the scenario
#                            - uses a single new incidence estimate, but applies other low/high parameters
# Population seeking treatment
efficacy_sa1 <- efficacy_sa1 %>%
  mutate(
    unsup.mean_tsb_count = unsup.incidence * mean_tsb_prop,
    unsup.low_tsb_count = unsup.incidence * low_tsb_prop,
    unsup.high_tsb_count = unsup.incidence * high_tsb_prop
  ) %>%
  mutate_at(vars(
    unsup.mean_tsb_count,
    unsup.low_tsb_count,
    unsup.high_tsb_count
  ),
  round,
  0)
# Population receiving G6PD tests - pop.tested.b
efficacy_sa1 <- efficacy_sa1 %>%
  mutate(
    pop.tested.b_mean =
      case_when(
        age_group == "infants" ~ unsup.mean_tsb_count * (1 - 0.2),
        age_group == "children" ~ unsup.mean_tsb_count,
        age_group == "adults" ~ (1 - preg.lact) * unsup.mean_tsb_count
      ),
    pop.tested.b_low =
      case_when(
        age_group == "infants" ~ unsup.low_tsb_count * (1 - 0.2),
        age_group == "children" ~ unsup.low_tsb_count,
        age_group == "adults" ~ (1 - preg.lact) * unsup.low_tsb_count
      ),
    pop.tested.b_high =
      case_when(
        age_group == "infants" ~ unsup.high_tsb_count * (1 - 0.2),
        age_group == "children" ~ unsup.high_tsb_count,
        age_group == "adults" ~ (1 - preg.lact) * unsup.high_tsb_count
      )
  ) %>%
  mutate_at(vars(pop.tested.b_mean, pop.tested.b_low, pop.tested.b_high),
            ~round(., 0))
# Calculate population prescribed PQ - PQ.pop.b 
# Calculate population prescribed PQ who receive an effective dose of PQ - PQ.pop.eff.b
efficacy_sa1 <- efficacy_sa1 %>%
  mutate(
    PQ.pop.b_mean = pop.tested.b_mean * ((
      g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
    )),
    PQ.pop.eff.b_mean = pop.tested.b_mean * ((1 - g6pdd) * g6pd.spec),
    PQ.pop.b_low = pop.tested.b_low * ((
      g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
    )),
    PQ.pop.eff.b_low = pop.tested.b_low * ((1 - g6pdd) * g6pd.spec),
    PQ.pop.b_high = pop.tested.b_high * ((
      g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
    )),
    PQ.pop.eff.b_high = pop.tested.b_high * ((1 - g6pdd) * g6pd.spec)
  ) %>%
  mutate_at(
    vars(
      PQ.pop.b_mean,
      PQ.pop.b_low,
      PQ.pop.b_high,
      PQ.pop.eff.b_mean,
      PQ.pop.eff.b_low,
      PQ.pop.eff.b_high
    ),
    ~round(., 0)
  )


###
# Calculate  Costs
###
# Cost of screening
efficacy_sa1 <- efficacy_sa1 %>%
  mutate(
    cScreening = case_when(
      country != "Malaysia" ~ pop.tested.b_mean * g6pd_rdt_mean,
      country == "Malaysia" ~ pop.tested.b_mean *
        fst_wpro_mean
    )
  )

# Cost of Patient Management
# Provider cost per person treated 
# Provide cost includes: 1) Cost of visit/case seeking treatment
#                        2) Cost of malaria Dx/case seeking treatment
#                        3) Cost of bloodstage cure/case seeking treatment
#                        4) Cost of high dose PQ for those eligible and testing G6PD normal
efficacy_sa1 <- efficacy_sa1 %>%
  mutate(
    cCare = (
      mean_cost_pervisit + mean_dxcost_percase + mean_bloodstage_cost
    ) * unsup.mean_tsb_count +
      (pq_highdose_cost * PQ.pop.b_mean)
  )

# OVERALL COST ESTIMATES
# Total Provider Cost (unsupervised radical cure)
#    Cost of Screening + Cost of Care
efficacy_sa1 <- efficacy_sa1 %>%
  mutate(unsup.total_provcost = cScreening + cCare)

# Direct Household Costs
#    Direct Cost Per Case * Number Seeking Treating
efficacy_sa1 <- efficacy_sa1 %>%
  mutate(unsup.dirhh_cost = med_hhcost_percase * unsup.mean_tsb_count)

# Indirect Household Costs
#    (Lost productivity per case + Carer days lost per case) * Incidence
efficacy_sa1 <- efficacy_sa1 %>%
  mutate(
    unsup.indirhh_cost = (mean_lostprod_percase + mean_carerlost_percase) * unsup.incidence
  )

# Total Household Costs
#    Direct Household Costs + Indirect Household Costs
efficacy_sa1 <- efficacy_sa1 %>%
  mutate(unsup.totalhh_cost = unsup.dirhh_cost + unsup.indirhh_cost)

# Overall costs
#    Total Provider Cost + Total Household Costs
efficacy_sa1 <- efficacy_sa1 %>%
  mutate(unsup.overallcost_mean = unsup.total_provcost + unsup.totalhh_cost)


# SA2 - Low unsupervised effectiveness of PQ
###
# Calculate Incidence for Unsupervised Radical Cure strategy
###
# STEP 1: Cases averted - use baseline estimates to calculate the number of cases averted by implementing
#                         unsupervised radical cure 
# Account for reduced cases in countries that already use PQ
# Round to nearest integer
efficacy_sa2 <- efficacy_sa2 %>%
  mutate(
    cases.av =
      case_when(
        pq_use == "N" ~
          PQ.pop.eff.a_mean * unsupervised.eff_low,
        pq_use == "Y" ~
          PQ.pop.eff.a_mean * (
            unsupervised.eff_low - (pq_prescribed_mean * unsupervised.eff_low)
          )
      )
  ) %>%
  mutate_at(vars(cases.av), round, 0)

# STEP 2: New incidence values - subtract cases averted from the baseline incidence to get an updated
#                                incidence to calculate the costs for the radical cure scenario.
efficacy_sa2 <- efficacy_sa2 %>%
  mutate(unsup.incidence = mean_incidence - cases.av)

# STEP 3: Get "b" parameters - these are to be used in cost calculations to determine the new costs
#                              associated with the scenario
#                            - uses a single new incidence estimate, but applies other low/high parameters
# Population seeking treatment
efficacy_sa2 <- efficacy_sa2 %>%
  mutate(
    unsup.mean_tsb_count = unsup.incidence * mean_tsb_prop,
    unsup.low_tsb_count = unsup.incidence * low_tsb_prop,
    unsup.high_tsb_count = unsup.incidence * high_tsb_prop
  ) %>%
  mutate_at(vars(
    unsup.mean_tsb_count,
    unsup.low_tsb_count,
    unsup.high_tsb_count
  ),
  round,
  0)
# Population receiving G6PD tests - pop.tested.b
efficacy_sa2 <- efficacy_sa2 %>%
  mutate(
    pop.tested.b_mean =
      case_when(
        age_group == "infants" ~ unsup.mean_tsb_count * (1 - 0.2),
        age_group == "children" ~ unsup.mean_tsb_count,
        age_group == "adults" ~ (1 - preg.lact) * unsup.mean_tsb_count
      ),
    pop.tested.b_low =
      case_when(
        age_group == "infants" ~ unsup.low_tsb_count * (1 - 0.2),
        age_group == "children" ~ unsup.low_tsb_count,
        age_group == "adults" ~ (1 - preg.lact) * unsup.low_tsb_count
      ),
    pop.tested.b_high =
      case_when(
        age_group == "infants" ~ unsup.high_tsb_count * (1 - 0.2),
        age_group == "children" ~ unsup.high_tsb_count,
        age_group == "adults" ~ (1 - preg.lact) * unsup.high_tsb_count
      )
  ) %>%
  mutate_at(vars(pop.tested.b_mean, pop.tested.b_low, pop.tested.b_high),
            ~round(., 0))
# Calculate population prescribed PQ - PQ.pop.b 
# Calculate population prescribed PQ who receive an effective dose of PQ - PQ.pop.eff.b
efficacy_sa2 <- efficacy_sa2 %>%
  mutate(
    PQ.pop.b_mean = pop.tested.b_mean * ((
      g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
    )),
    PQ.pop.eff.b_mean = pop.tested.b_mean * ((1 - g6pdd) * g6pd.spec),
    PQ.pop.b_low = pop.tested.b_low * ((
      g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
    )),
    PQ.pop.eff.b_low = pop.tested.b_low * ((1 - g6pdd) * g6pd.spec),
    PQ.pop.b_high = pop.tested.b_high * ((
      g6pdd * (1 - g6pd.sens) + (1 - g6pdd) * g6pd.spec
    )),
    PQ.pop.eff.b_high = pop.tested.b_high * ((1 - g6pdd) * g6pd.spec)
  ) %>%
  mutate_at(
    vars(
      PQ.pop.b_mean,
      PQ.pop.b_low,
      PQ.pop.b_high,
      PQ.pop.eff.b_mean,
      PQ.pop.eff.b_low,
      PQ.pop.eff.b_high
    ),
    ~round(., 0)
  )


###
# Calculate  Costs
###
# Cost of screening
efficacy_sa2 <- efficacy_sa2 %>%
  mutate(
    cScreening = case_when(
      country != "Malaysia" ~ pop.tested.b_mean * g6pd_rdt_mean,
      country == "Malaysia" ~ pop.tested.b_mean *
        fst_wpro_mean
    )
  )

# Cost of Patient Management
# Provider cost per person treated 
# Provide cost includes: 1) Cost of visit/case seeking treatment
#                        2) Cost of malaria Dx/case seeking treatment
#                        3) Cost of bloodstage cure/case seeking treatment
#                        4) Cost of high dose PQ for those eligible and testing G6PD normal
efficacy_sa2 <- efficacy_sa2 %>%
  mutate(
    cCare = (
      mean_cost_pervisit + mean_dxcost_percase + mean_bloodstage_cost
    ) * unsup.mean_tsb_count +
      (pq_highdose_cost * PQ.pop.b_mean)
  )

# OVERALL COST ESTIMATES
# Total Provider Cost (unsupervised radical cure)
#    Cost of Screening + Cost of Care
efficacy_sa2 <- efficacy_sa2 %>%
  mutate(unsup.total_provcost = cScreening + cCare)

# Direct Household Costs
#    Direct Cost Per Case * Number Seeking Treating
efficacy_sa2 <- efficacy_sa2 %>%
  mutate(unsup.dirhh_cost = med_hhcost_percase * unsup.mean_tsb_count)

# Indirect Household Costs
#    (Lost productivity per case + Carer days lost per case) * Incidence
efficacy_sa2 <- efficacy_sa2 %>%
  mutate(
    unsup.indirhh_cost = (mean_lostprod_percase + mean_carerlost_percase) * unsup.incidence
  )

# Total Household Costs
#    Direct Household Costs + Indirect Household Costs
efficacy_sa2 <- efficacy_sa2 %>%
  mutate(unsup.totalhh_cost = unsup.dirhh_cost + unsup.indirhh_cost)

# Overall costs
#    Total Provider Cost + Total Household Costs
efficacy_sa2 <- efficacy_sa2 %>%
  mutate(unsup.overallcost_mean = unsup.total_provcost + unsup.totalhh_cost)





####   OUTPUT RESULTS AS CSV   ####

output <- baseline %>%
  select(
    c(
      country,
      age_group,
      unsup.total_provcost,
      unsup.dirhh_cost,
      unsup.indirhh_cost,
      unsup.totalhh_cost,
      unsup.overallcost_mean
    )
  ) %>%
  mutate_if(is.numeric, round) %>%
  bind_rows(summarise_all(., list(function(x) if(is.numeric(x)) sum(x) else "Total"))) %>%
  rename(
    "Country" = country,
    "Age Group" = age_group,
    "Provider Cost" = unsup.total_provcost,
    "Direct Household Cost" = unsup.dirhh_cost,
    "Indirect Household Cost" = unsup.indirhh_cost,
    "Total Household Cost" = unsup.totalhh_cost,
    "Overall Cost" = unsup.overallcost_mean
  )
write.csv(output,
          'outputs/RadicalCure_UnsupPQ_AgeGroups.csv',
          row.names = FALSE)

output2 <- baseline %>%
  select(
    c(
      country,
      age_group,
      unsup.total_provcost,
      unsup.dirhh_cost,
      unsup.indirhh_cost,
      unsup.totalhh_cost,
      unsup.overallcost_mean
    )
  ) %>%
  mutate_if(is.numeric, round) %>%
  group_by(country) %>%
  summarise_if(is.numeric, sum) %>%
  bind_rows(summarise_all(., list(function(x) if(is.numeric(x)) sum(x) else "Total"))) %>%
  rename(
    "Country" = country,
    "Provider Cost" = unsup.total_provcost,
    "Direct Household Cost" = unsup.dirhh_cost,
    "Indirect Household Cost" = unsup.indirhh_cost,
    "Total Household Cost" = unsup.totalhh_cost,
    "Overall Cost" = unsup.overallcost_mean
  )
write.csv(output2, 'outputs/RadicalCure_UnsupPQ.csv', row.names =
            FALSE)

# save image for output tables
d <- Sys.Date()
radicalcure_unsup <- baseline
save.image(file = paste0("./outputs/radcure.RData"))

# Save baseline data to be used for PSA
radicalcure_unsup <- baseline
radcure <- baseline
rm(list = setdiff(
  ls(),
  c("radcure", "mean_PQ.base", "low_PQ.base", "high_PQ.base")
))
save.image(file = "outputs/radcure_psa.Rdata")