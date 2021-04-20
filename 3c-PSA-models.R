#####################################################################################################################
#   R-script:     3c-PSA-models.R
#   Project:		  "Global economic costs due to vivax malaria and the potential impact of its radical cure: A modelling study" by A Devine et al.
#
#   Purpose:  		Creating the model for the baseline and radical cures to fit for each sample of PSA parameters
#
#   Date:			    14-April-2021
#   Author: 		  Niamh Meagher & Angela Devine
#####################################################################################################################


baseline.model <- function(params) {
  output <- params %>%
    mutate(
      # To get the total cost estimate for the baseline
      total_provcost =
        case_when(
          age_group == "adults" & country != "Malaysia" & pq_use == "N" ~
            (par_dxcost_percase + par_bloodstage_cost + par_cost_pervisit) * (par_incidence * par_tsb_prop),
          age_group == "adults" &
            country != "Malaysia" & pq_use == "Y" ~
            (par_dxcost_percase + par_bloodstage_cost + par_cost_pervisit) * (par_incidence * par_tsb_prop) +
            (par_PQ_cost * (1 - preg.lact) * par_PQ_pres) * (par_incidence * par_tsb_prop),
          age_group == "adults" & country == "Malaysia" ~
            (par_dxcost_percase + par_bloodstage_cost + par_cost_pervisit) * (par_incidence * par_tsb_prop) +
            (par_g6pd_fst * (1 - preg.lact)) * (par_incidence * par_tsb_prop) +
            (par_PQ_cost * (
              g6pdd * (1 - par_g6pd_sens) + (1 - g6pdd) * par_g6pd_spec
            ) * (1 - preg.lact)) * (par_incidence * par_tsb_prop),
          age_group == "children" &
            country != "Malaysia" & pq_use == "N" ~
            (par_dxcost_percase + par_bloodstage_cost + par_cost_pervisit) * (par_incidence * par_tsb_prop),
          age_group == "children" &
            country != "Malaysia" & pq_use == "Y" ~
            (par_dxcost_percase + par_bloodstage_cost + par_cost_pervisit) * (par_incidence * par_tsb_prop) +
            (par_PQ_cost * par_PQ_pres) * (par_incidence * par_tsb_prop),
          age_group == "children" &
            country == "Malaysia" ~
            (par_dxcost_percase + par_bloodstage_cost + par_cost_pervisit) * (par_incidence * par_tsb_prop) +
            (par_g6pd_fst * (par_incidence * par_tsb_prop)) +
            (par_PQ_cost * (
              g6pdd * (1 - par_g6pd_sens) + (1 - g6pdd) * par_g6pd_spec
            )) * (par_incidence * par_tsb_prop),
          age_group == "infants" &
            country != "Malaysia" & pq_use == "N" ~
            (par_dxcost_percase + par_bloodstage_cost + par_cost_pervisit) * (par_incidence * par_tsb_prop),
          age_group == "infants" &
            country != "Malaysia" & pq_use == "Y" ~
            (par_dxcost_percase + par_bloodstage_cost + par_cost_pervisit) * (par_incidence * par_tsb_prop) +
            (par_PQ_cost * par_PQ_pres) * (par_incidence * par_tsb_prop * 0.8),
          age_group == "infants" & country == "Malaysia" ~
            (par_dxcost_percase + par_bloodstage_cost + par_cost_pervisit) * (par_incidence * par_tsb_prop) +
            (par_g6pd_fst * (par_incidence * par_tsb_prop * 0.8)) +
            (par_PQ_cost * (
              g6pdd * (1 - par_g6pd_sens) + (1 - g6pdd) * par_g6pd_spec
            )) * (par_incidence * par_tsb_prop * 0.8)
        ),
      dir_patcost = par_hhcost_percase * (par_incidence * par_tsb_prop),
      lostprod_percase = ifelse(
        age_group == "infants",
        0,
        par_dayslost_percase * gdp2017_percapday
      ),
      carerlost_percase = par_carerdays_percase * gdp2017_percapday,
      indir_patcost = (lostprod_percase + carerlost_percase) * par_incidence,
      total_patcost = dir_patcost + indir_patcost,
      total_cost = total_patcost + total_provcost
    )
  
  return(output)
}


radcure.model <- function(params) {
  output <- params %>%
    mutate(
      # Get populations to be G6PD tested and receive PQ in baseline - use to calculate new incidences in radical cure scenarios
      pop.tested.a =
        case_when(
          age_group == "infants" ~ (par_incidence * par_tsb_prop) * (1 - 0.2),
          age_group == "children" ~ (par_incidence * par_tsb_prop),
          age_group == "adults" ~ (1 - preg.lact) * (par_incidence * par_tsb_prop)
        ),
      PQ.pop.a = pop.tested.a * ((
        g6pdd * (1 - par_g6pd_sens) + (1 - g6pdd) * par_g6pd_spec
      )),
      PQ.pop.eff.a = pop.tested.a * ((1 - g6pdd) * par_g6pd_spec)
    ) %>%
    mutate_at(vars(pop.tested.a, PQ.pop.a, PQ.pop.eff.a), round, 0) %>%
    #### SUPERVISED
    mutate(
      cases.av_sup =
        case_when(
          pq_use == "N" ~ PQ.pop.eff.a * par_PQ_prop_relapses,
          pq_use == "Y" ~ PQ.pop.eff.a * (par_PQ_prop_relapses -
                                            (par_PQ_pres * par_unsup_eff))
        ),
      incidence_sup = par_incidence - cases.av_sup
    ) %>%
    mutate_at(vars(cases.av_sup, incidence_sup), round, 0) %>%
    mutate(
      # Now get the population parameters for calculating costs in the supervised scenario
      tsb_count_sup = incidence_sup * par_tsb_prop,
      pop.tested.b_sup =
        case_when(
          age_group == "infants" ~ tsb_count_sup * (1 - 0.2),
          age_group == "children" ~ tsb_count_sup,
          age_group == "adults" ~ (1 - preg.lact) * tsb_count_sup
        ),
      PQ.pop.b_sup = pop.tested.b_sup * ((
        g6pdd * (1 - par_g6pd_sens) + (1 - g6pdd) * par_g6pd_spec
      )),
      PQ.pop.eff.b_sup = pop.tested.b_sup * ((1 - g6pdd) * par_g6pd_spec)
    ) %>%
    mutate_at(vars(PQ.pop.b_sup, PQ.pop.eff.b_sup), ~round(., 0))
  
  # Cost of Screening, Supervision and Care
  output <- output %>%
    mutate(
      sup_cScreening = case_when(
        country == "Malaysia" ~ pop.tested.b_sup * par_g6pd_fst,
        country != "Malaysia" ~ pop.tested.b_sup *
          par_g6pd_rdt
      ),
      sup_cSupervision = PQ.pop.b_sup * par_supcost,
      sup_cCare = ((par_dxcost_percase + par_bloodstage_cost + par_cost_pervisit) *
                     tsb_count_sup
      ) +
        PQ.pop.b_sup * (par_PQ_cost * 2),
      # Total Provider Cost
      total_provcost_sup = sup_cScreening + sup_cSupervision + sup_cCare,
      # Direct Household Cost
      dirhh_cost_sup = (par_hhcost_percase * tsb_count_sup),
      # Indirect Household Cost
      indirhh_cost_sup = ((lostprod_percase + carerlost_percase) * incidence_sup),
      # Total Household Cost
      total_hhcost_sup = dirhh_cost_sup + indirhh_cost_sup,
      # Overall Cost
      overallcost_sup = total_provcost_sup + total_hhcost_sup
    )
  
  #### UNSUPERVISED
  output <- output %>%
    mutate(
      cases.av_unsup =
        case_when(
          pq_use == "N" ~ PQ.pop.eff.a * par_unsup_eff,
          pq_use == "Y" ~ PQ.pop.eff.a * (par_unsup_eff - (par_PQ_pres *
                                                             par_unsup_eff))
        ),
      incidence_unsup = par_incidence - cases.av_unsup
    ) %>%
    mutate_at(vars(cases.av_unsup, incidence_unsup), round, 0) %>%
    mutate(
      # Now get the population parameters for calculating costs in the unsupervised radical cure scenario
      tsb_count_unsup = incidence_unsup * par_tsb_prop,
      pop.tested.b_unsup =
        case_when(
          age_group == "infants" ~ tsb_count_unsup * (1 - 0.2),
          age_group == "children" ~ tsb_count_unsup,
          age_group == "adults" ~ (1 - preg.lact) * tsb_count_unsup
        ),
      PQ.pop.b_unsup = pop.tested.b_unsup * ((
        g6pdd * (1 - par_g6pd_sens) + (1 - g6pdd) * par_g6pd_spec
      )),
      PQ.pop.eff.b_unsup = pop.tested.b_unsup * ((1 - g6pdd) * par_g6pd_spec)
    ) %>%
    mutate_at(vars(PQ.pop.b_unsup, PQ.pop.eff.b_unsup), ~round(., 0))
  
  # Cost of Screening and Care
  output <- output %>%
    mutate(
      unsup_cScreening = case_when(
        country == "Malaysia" ~ pop.tested.b_unsup * par_g6pd_fst,
        country != "Malaysia" ~ pop.tested.b_unsup *
          par_g6pd_rdt
      ),
      unsup_cCare = ((par_dxcost_percase + par_bloodstage_cost + par_cost_pervisit) *
                       tsb_count_unsup
      ) +
        PQ.pop.b_unsup * (par_PQ_cost * 2),
      # Total Provider Cost
      total_provcost_unsup = unsup_cScreening + unsup_cCare,
      # Direct Household Cost
      dirhh_cost_unsup = (par_hhcost_percase * tsb_count_unsup),
      # Indirect Household Cost
      indirhh_cost_unsup = ((lostprod_percase + carerlost_percase) * incidence_unsup),
      # Total Household Cost
      total_hhcost_unsup = dirhh_cost_unsup + indirhh_cost_unsup,
      # Overall Cost
      overallcost_unsup = total_provcost_unsup + total_hhcost_unsup
    )
  
  return(output)
}