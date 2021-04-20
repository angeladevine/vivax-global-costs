#####################################################################################################################
#   R-script:     3a-PSA-parameters.R
#   Project:		  "Global economic costs due to vivax malaria and the potential impact of its radical cure: A modelling study" by A Devine et al.
#
#   Data used:	  "baseline.Rdata", "radcure_psa.Rdata"
#
#   Purpose:  		Creating a dataset for the parameter inputs
#
#   Date:			    14-Apr-2021
#   Author: 		  Niamh Meagher & Angela Devine
#####################################################################################################################


# Create a reduced dataframe of parameters for each country using the data from the baseline and radical cure scenarios
load(file = "outputs/baseline.Rdata")
load(file = "outputs/radcure_psa.Rdata")
# For ease of data entry, drop the single SA variables from the baseline data frame
baseline <- baseline %>% select(-(starts_with("sa_")))

# New create regexs to identify columns corresponding to the parameters of interest
baseline_pars <-
  "incidence|tsb_prop|cost_pervisit|dxcost_percase|g6pd_dx|bloodstage_cost|hhcost_percase|dayslost_percase|carerdays_percase"
radcure_pars <- "g6pd_rdt"

# Parameter list - to extract required parameters from dataframe
all.params <-
  data.frame(
    baseline %>% select(country, age_group, pq_use, preg.lact, gdp2017_percapday),
    radcure %>% select(g6pdd),
    baseline %>% select(matches(baseline_pars)),
    radcure %>% select(matches(radcure_pars)),
    "mean_g6pd_sens" = 0.96,
    "low_g6pd_sens" = 0.90,
    "high_g6pd_sens" = 0.99,
    "mean_g6pd_spec" = 0.95,
    "low_g6pd_spec" = 0.92,
    "high_g6pd_spec" = 0.96,
    "mean_PQ_cost" = pq_cost,
    "low_PQ_cost" = pq_cost - (0.5 * pq_cost),
    "high_PQ_cost" = pq_cost + (0.5 * pq_cost),
    "mean_PQ_pres" = pq_prescribed_mean,
    "low_PQ_pres" = pq_prescribed_low,
    "high_PQ_pres" = pq_prescribed_high,
    "mean_unsup_eff" = 0.4,
    "low_unsup_eff" = 0.10,
    "high_unsup_eff" = 0.70,
    "mean_supcost" = radcure$gdp2017_percapday *
      1.5,
    "low_supcost" = radcure$gdp2017_percapday *
      0.25,
    "high_supcost" = radcure$gdp2017_percapday *
      3.5,
    "mean_PQ.base" = mean_PQ.base,
    "low_PQ.base" = low_PQ.base,
    "high_PQ.base" = high_PQ.base
  )

# Rename columns so that they are formatted in the same way 
all.params <-
  all.params %>% rename(
    mean_g6pd_fst = g6pd_dx_mean,
    low_g6pd_fst = g6pd_dx_low,
    high_g6pd_fst = g6pd_dx_high,
    mean_hhcost_percase = med_hhcost_percase,
    mean_dayslost_percase = med_dayslost_percase,
    mean_carerdays_percase = med_carerdays_percase,
    mean_g6pd_rdt = g6pd_rdt_mean,
    low_g6pd_rdt = g6pd_rdt_low,
    high_g6pd_rdt = g6pd_rdt_high,
    mean_PQ_prop_relapses = mean_PQ.base,
    low_PQ_prop_relapses = low_PQ.base,
    high_PQ_prop_relapses = high_PQ.base
  )

# For carer days lost to illness, we will add a very small value to the lower bound (0.01)
# This allows a gamma distribution to be fit.
all.params <- all.params %>%
  mutate(
    low_carerdays_percase.01 = case_when(low_carerdays_percase == 0 ~ 0.01)
  )

# Remove everything except parameters and distribution list
rm(list = setdiff(ls(), "all.params"))