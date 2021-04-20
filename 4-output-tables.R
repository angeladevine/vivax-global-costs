#####################################################################################################################
#   R-script:     4-output-tables.R
#   Project:		  "Global economic costs due to vivax malaria and the potential impact of its radical cure: A modelling study" by A Devine et al.
#
#   Data used:	  (1) "baseline.Rdata"
#                 (2) "radcure.Rdata"
#                 (3) "psa.Rdata"
#                 (4) "mapdata.xlsx" from Battle et al. 2019 Lancet
#   Data created:	"Table 1.csv", "Table 2.csv", "Table 3.csv", "Table 4.csv", 
#                 "S1_Table.csv", "S3_Table.csv", "S4_Table.csv", "Extra.csv"
#
#   Purpose:  		Formatting output for manuscript tables and appendices
#
#   Date:			    14-April-2021
#   Author: 		  Niamh Meagher & Angela Devine
#####################################################################################################################

rm(list = ls())

load(paste0("./outputs/psa.RData"))
  rm(list = c("all.params", "bad", "base", "dist.pars", "output", "p1", 'param_dat', "radsup", "radunsup", "samp_dat",
              "simulation_res", "a", "age", "beta", "c", "carerdays0", "country", "gamma", "i", "normal", "sim", "v",
              "xvals", "yvals"))
load(paste0("./outputs/radcure.RData"))
  rm(list = c("baseline", "g6pddx_costs", "output", "output2"))
load(paste0("./outputs/baseline.RData"))
  rm(list = c("dayslost.dat", "diag_costs.dat", "gdp.dat", "household_costs.dat", "incidence.dat", "output2", "treatment_costs.dat",
              "visit_costs.dat"))
  
# Table 1
  # Demography by Region
    par_inf <- readxl::read_xlsx("inputs/mapdata.xlsx", range = "A1:U136", sheet = "pv_case_data_new")
    par_inf <- par_inf %>%
      select(Name, age_group, PAR) %>%
      rename("country" = Name, "count_PAR" = PAR) %>%
      filter(country != "North Korea") %>%
      mutate(count_PAR = count_PAR/3) # Divided PAR by 3 because it lists the same total population for infants, children and adults.
    tab1_1 <- baseline %>%
      select(country, age_group, region, mean_tsb_count, g6pdd, mean_incidence, preg.lact) %>%
      mutate(count_preg.lact = case_when(age_group == "adults" ~ mean_tsb_count*preg.lact,
                                         age_group != "adults" ~ 0),
             count_g6pdd_over1 = case_when(age_group != "infants" ~ mean_tsb_count*g6pdd,
                                           age_group == "infants" ~ mean_tsb_count*g6pdd*(1-0.2)),
             count_g6pd_tested = case_when(age_group == "infants" ~ mean_tsb_count*(1-0.2),
                                           age_group == "children" ~ mean_tsb_count,
                                           age_group == "adults" ~ (1-preg.lact)*mean_tsb_count),
             count_PQelig = count_g6pd_tested * ((g6pdd*(1-g6pd.sens) + (1-g6pdd)*g6pd.spec)))
    tab1_1 <- left_join(tab1_1, par_inf, by = c("country", "age_group"))
  # Summarise to get estimates by WHO region (with total)
    tab1 <- tab1_1 %>%
      group_by(region, age_group) %>%
      summarise_if(is.numeric, sum) %>%
      mutate_if(is.numeric, round) %>%
      mutate(mean_incidence_inf = case_when(age_group == "infants" ~ mean_incidence, age_group != "infants" ~ 0),
             mean_incidence_chil = case_when(age_group == "children" ~ mean_incidence, age_group != "children" ~ 0),
             mean_incidence_adults = case_when(age_group == "adults" ~ mean_incidence, age_group != "adults" ~ 0),
             mean_tsb_count_inf = case_when(age_group == "infants" ~ mean_tsb_count, age_group != "infants" ~ 0),
             mean_tsb_count_chil = case_when(age_group == "children" ~ mean_tsb_count, age_group != "children" ~ 0),
             mean_tsb_count_adults = case_when(age_group == "adults" ~ mean_tsb_count, age_group != "adults" ~ 0)) %>%
      select(region, age_group, count_PAR, mean_incidence, mean_incidence_inf, mean_incidence_chil, mean_incidence_adults,
             mean_tsb_count, mean_tsb_count_inf, mean_tsb_count_chil, mean_tsb_count_adults, count_preg.lact,
             count_g6pdd_over1, count_PQelig) %>%
      group_by(region) %>% summarise_if(is.numeric, sum) %>% ungroup() %>%
      bind_rows(summarise_all(., list(function(x) if(is.numeric(x)) sum(x) else "Total")))
  # Transpose for correct formatting
    tab1 <- as.data.frame(t(as.matrix(tab1)))
  # Rename columns, drop "region" row"
    colnames(tab1) <- tab1["region",]
    tab1 <- tab1[2:nrow(tab1),]
  # Rename rows
    row.names(tab1) <- c("PAR (from MAP)", "Total vivax malaria incidence",
                         "Infant incidence", "Children incidence", "Adults incidence", "Total number seeking treatment",
                         "Infants seeking treatment", "Children seeking treatment", "Adults seeking treatment",
                         "Pregnant or lactating adults", "Number G6PD deficient in treatment seeking population >1yr",
                         "Number eligible for primaquine in treatment seeking population")
    
# Table 2    
  # Country, Provider Costs, Direct Household Costs, Total Cost, 95% CrIs
    tab2 <- output_base[,1:7]
  # Rename columns
    colnames(tab2) <- c("Country", "Provider Costs", "Direct Household Costs", "Indirect Household Costs",
                       "Total Cost", "Lower 95% CrI", "Upper 95% CrI")
    
# Table 3
  # Column 1 - select base case costs and total
    tab3 <- baseline %>%
      select(mean_total_provcost, mean_total_patcost, mean_direct_patcost, mean_indirect_patcost, mean_total_cost) %>%
      summarise_all(., ~sum(.))
    # Add on the CrIs and rename columns for clarity and easier binding
      tab3 <- cbind(tab3, output_base[output_base$Country=="Total", c("CrI.Lower", "CrI.Upper")])
      tab3 <- tab3 %>%
        dplyr::rename_at(vars(colnames(tab3)), ~ c("provider_cost", "household_cost", "direct_household_cost", "indirect_household_cost",
                                                   "total_cost", "cri_lower", "cri_upper"))
      # Rename new row
        rownames(tab3) <- "Base.Global.Cost"
    
  # Column 2 - base case sensitivity analysis (excluding productivity lost in children)
    addrow <- baseline %>%
      select(mean_total_provcost, sa_mean_total_patcost, sa_mean_direct_patcost, sa_mean_indirect_patcost, sa_mean_total_cost) %>%
      summarise_all(., ~sum(.)) %>%
      add_column("CrI.Lower" = NA, "CrI.Upper" = NA) %>%
      dplyr::rename_at(vars(colnames(.)), ~ c("provider_cost", "household_cost", "direct_household_cost", "indirect_household_cost",
                                              "total_cost", "cri_lower", "cri_upper"))
    # Add on to table
      tab3 <- rbind(tab3, addrow)
      # Rename new row
        rownames(tab3)[2] <- "Base.SA"
    
  # Column 3 - results from supervised radical cure scenario
    addrow <- radicalcure_sup %>%
      select(sup.total_provcost, sup.totalhh_cost, sup.dirhh_cost, sup.indirhh_cost, sup.overallcost_mean) %>%
      summarise_all(., ~sum(.))
    # Add on the CrIs and rename columns for clarity and easier binding
      addrow <- cbind(addrow, output_radsup[output_radsup$Country=="Total", c("CrI.Lower", "CrI.Upper")])
      addrow <- addrow %>%
        dplyr::rename_at(vars(colnames(addrow)), ~ c("provider_cost", "household_cost", "direct_household_cost", "indirect_household_cost",
                                                     "total_cost", "cri_lower", "cri_upper"))
      # Add on to table, rename new row
        tab3 <- rbind(tab3, addrow)
          # Rename new row
            rownames(tab3)[3] <- "Sup.Rad.Cure.Cost"
    
  # Column 4 - one way SA on cost of supervision (6 days supervision)
    addrow <- radicalcure_sup %>%
      select(sup.total_provcost_sa1, sup.totalhh_cost, sup.dirhh_cost, sup.indirhh_cost, sup.overallcost_mean_sa1) %>%
      summarise_all(., ~sum(.)) %>%
      add_column("CrI.Lower" = NA, "CrI.Upper" = NA) %>%
      dplyr::rename_at(vars(colnames(.)), ~ c("provider_cost", "household_cost", "direct_household_cost", "indirect_household_cost",
                                              "total_cost", "cri_lower", "cri_upper"))
    # Add on to table
      tab3 <- rbind(tab3, addrow)
        # Rename new row
          rownames(tab3)[4] <- "Sup.Rad.Cure.SA.SupCost.6Vis"
    
  # Column 5 - one way SA on cost of supervision (1 day supervision)
    addrow <- radicalcure_sup %>%
      select(sup.total_provcost_sa2, sup.totalhh_cost, sup.dirhh_cost, sup.indirhh_cost, sup.overallcost_mean_sa2) %>%
      summarise_all(., ~sum(.)) %>%
      add_column("CrI.Lower" = NA, "CrI.Upper" = NA) %>%
      dplyr::rename_at(vars(colnames(.)), ~ c("provider_cost", "household_cost", "direct_household_cost", "indirect_household_cost",
                                              "total_cost", "cri_lower", "cri_upper"))
    # Add on to table
      tab3 <- rbind(tab3, addrow)
        # Rename new row
          rownames(tab3)[5] <- "Sup.Rad.Cure.SA.SupCost.1Vis"
    
  # Column 6 - one way SA on effectiveness of PQ (low proportion/less effective)
    addrow <- effect_sa1 %>%
      select(sup.total_provcost, sup.totalhh_cost, sup.dirhh_cost, sup.indirhh_cost, sup.overallcost_mean) %>%
      summarise_all(., ~sum(.)) %>%
      add_column("CrI.Lower" = NA, "CrI.Upper" = NA) %>%
      dplyr::rename_at(vars(colnames(.)), ~ c("provider_cost", "household_cost", "direct_household_cost", "indirect_household_cost",
                                              "total_cost", "cri_lower", "cri_upper"))
    # Add on to table
      tab3 <- rbind(tab3, addrow)
        # Rename new row
        rownames(tab3)[6] <- "Sup.Rad.Cure.SA.PQEff.Low"
    
  # Column 7 - one way SA on effectiveness of PQ (high proportion/more effective)
    addrow <- effect_sa2 %>%
      select(sup.total_provcost, sup.totalhh_cost, sup.dirhh_cost, sup.indirhh_cost, sup.overallcost_mean) %>%
      summarise_all(., ~sum(.)) %>%
      add_column("CrI.Lower" = NA, "CrI.Upper" = NA) %>%
      dplyr::rename_at(vars(colnames(.)), ~ c("provider_cost", "household_cost", "direct_household_cost", "indirect_household_cost",
                                              "total_cost", "cri_lower", "cri_upper"))
    # Add on to table
      tab3 <- rbind(tab3, addrow)
      # Rename new row
        rownames(tab3)[7] <- "Sup.Rad.Cure.SA.PQEff.High"
    
  # Column 8 - results from unsupervised radical cure scenario
    addrow <- radicalcure_unsup %>%
      select(unsup.total_provcost, unsup.totalhh_cost, unsup.dirhh_cost, unsup.indirhh_cost, unsup.overallcost_mean) %>%
      summarise_all(., ~sum(.))
    # Add on the CrIs and rename columns for clarity and easier binding
      addrow <- cbind(addrow, output_radunsup[output_radunsup$Country=="Total", c("CrI.Lower", "CrI.Upper")])
      addrow <- addrow %>%
        dplyr::rename_at(vars(colnames(addrow)), ~ c("provider_cost", "household_cost", "direct_household_cost", "indirect_household_cost",
                                                     "total_cost", "cri_lower", "cri_upper"))
      # Add on to table, rename new row
        tab3 <- rbind(tab3, addrow)
          # Rename new row
            rownames(tab3)[8] <- "Unsup.Rad.Cure.Cost"
    
  # Column 9 - one way SA on efficacy of PQ (lower proportion/less efficacious)
    addrow <- efficacy_sa2 %>%
      select(unsup.total_provcost, unsup.totalhh_cost, unsup.dirhh_cost, unsup.indirhh_cost, unsup.overallcost_mean) %>%
      summarise_all(., ~sum(.)) %>%
      add_column("CrI.Lower" = NA, "CrI.Upper" = NA) %>%
      dplyr::rename_at(vars(colnames(.)), ~ c("provider_cost", "household_cost", "direct_household_cost", "indirect_household_cost",
                                              "total_cost", "cri_lower", "cri_upper"))
      # Add on to table
        tab3 <- rbind(tab3, addrow)
          # Rename new row
            rownames(tab3)[9] <- "Unsup.Rad.Cure.SA.PQEff.Low"
    
  # Column 10 - one way SA on efficacy of PQ (higher proportion/more efficacious)
    addrow <- efficacy_sa1 %>%
      select(unsup.total_provcost, unsup.totalhh_cost, unsup.dirhh_cost, unsup.indirhh_cost, unsup.overallcost_mean) %>%
      summarise_all(., ~sum(.)) %>%
      add_column("CrI.Lower" = NA, "CrI.Upper" = NA) %>%
      dplyr::rename_at(vars(colnames(.)), ~ c("provider_cost", "household_cost", "direct_household_cost", "indirect_household_cost",
                                              "total_cost", "cri_lower", "cri_upper"))
    # Add on to table
      tab3 <- rbind(tab3, addrow)
        # Rename new row
          rownames(tab3)[10] <- "Unsup.Rad.Cure.SA.PQEff.High"
    
  # Transpose for correct formatting
    tab3 <- as.data.frame(t(as.matrix(tab3)))
    # Round all figures to nearest integer
      tab3[] <- mutate_all(tab3, round)
    

# Table 4
  # Country, Base PV Cases, Supervised PV Cases, Unsupervised PV Cases
    tab4 <- radicalcure_sup %>%
      select(c("country", "age_group", "mean_incidence", "mean_tsb_count"))
    # Add cases and % reduction for each supervised and unsupervised scenario
      tab4 <- cbind(tab4, radicalcure_sup[,"sup.incidence"], radicalcure_unsup[,"unsup.incidence"])
    # Group age-specific case numbers together for the table and add % reduction
      tab4 <- tab4 %>%
        group_by(country) %>%
        summarise_if(is.numeric, sum) %>%
        bind_rows(summarise_all(., list(function(x) if(is.numeric(x)) sum(x) else "Total"))) %>%
        mutate(sup.perc.reduct = ((mean_incidence-`radicalcure_sup[, "sup.incidence"]`)/mean_incidence),
               unsup.perc.reduct = ((mean_incidence-`radicalcure_unsup[, "unsup.incidence"]`)/mean_incidence)) %>%
        mutate_at(vars(sup.perc.reduct, unsup.perc.reduct), round, 4)
    # Rename columns
      tab4 <- tab4 %>%
        select(c(country, mean_incidence, mean_tsb_count, `radicalcure_sup[, "sup.incidence"]`, sup.perc.reduct,
                 `radicalcure_unsup[, "unsup.incidence"]`, unsup.perc.reduct)) %>%
        rename("Country" = country, "Base Incidence" = mean_incidence, "Base Treatment Seeking Count" = mean_tsb_count,
               "Supervised PQ Incidence" = `radicalcure_sup[, "sup.incidence"]`,
               "Supervised PQ: Reduction from Base Incidence (%)" = sup.perc.reduct,
               "Unsupervised PQ Incidence" = `radicalcure_unsup[, "unsup.incidence"]`,
               "Unsupervised PQ: Reduction from Base Incidence (%)" = unsup.perc.reduct)
    
# S1 TABLE
  # 1 - Country                            2 - WHO region                    3 - Age Group
  # 4,5,6 - Incidence (Mean, Low, High)    7,8,9 - TSB (Mean, Low, High)     10 - Patients Seeking Treatment
  # 11 - Prop G6PDd                        12 - Prop Pregnant/Lactating      13 - Proportion Total Confirmed Cases
  # 14 - Prop RDT Confirmed                15 - Days Lost/Case               16 - Carer Days Lost/Case
  # 17 - Bloodstage Treatment              18 - PQ Use                       19 - G6PD Diagnosis Cost
  # 20 - GDP/capita/day (2016)             21 - Total Cost of IP Visit       22 - Total Cost of OP Visit
  # 23 - Micro Dx Cost                     24 - RDT Dx Cost                  25 - PV Bloodstage Cure Cost/Case
  # 26 - Cost/PQ Treatment                 27 - Household Cost/Case          28 - Productivity Cost/Case            
  # 29 - Carer Productivity Lost/Case
      
    # Select columns 1 to 20 from base case dataframe
      app1 <- baseline %>%
        select(c("country", "region", "age_group", "mean_incidence", "low_incidence", "high_incidence",
                 "mean_tsb_prop", "low_tsb_prop", "high_tsb_prop", "mean_tsb_count", "g6pdd",
                 "preg.lact", "cnf_prop", "rdt_cnf_prop", "med_dayslost_percase", "med_carerdays_percase",
                 "bloodstage", "pq_use", "g6pd_dx_mean", "gdp2017_percapday", "ip_visit_cost", "op_visit_cost")) %>%
        rename(routine_g6pd = g6pd_dx_mean) %>%
        mutate(routine_g6pd = case_when(routine_g6pd == 0 ~ "N",
                                        routine_g6pd != 0 ~ "FST"))
    
    # Add columns 21 and 22 from saved values
      app1 <- app1 %>%
        mutate(micro_dx =
                 case_when(region == "AFRO" ~ afro_micro, region == "EMRO" ~ emro_micro, region == "PAHO" ~ paho_micro,
                           region == "SEARO" ~ searo_micro, region == "WPRO" ~ wpro_micro),
               rdt_dx =
                 case_when(region == "AFRO" ~ afro_rdt, region == "EMRO" ~ emro_rdt, region == "PAHO" ~ paho_rdt,
                           region == "SEARO" ~ searo_rdt, region == "WPRO" ~ wpro_rdt))
      
    # Add columns 23 from the base case dataframe 
      app1 <- cbind(app1, baseline[,"mean_bloodstage_cost"])

    # Add PQ cost if the country uses it
      app1 <- app1 %>% mutate(pq_cost = case_when(pq_use == "Y" ~ pq_lowdose_cost, pq_use == "N" ~ 0))
      
    # Add columns 25 to 27 from the base case dataframe
      app1 <- cbind(app1, baseline[,c("med_hhcost_percase", "mean_lostprod_percase", "mean_carerlost_percase")])
    
      # Sum up incidence values for each country
      incidences <- app1 %>%
        group_by(country) %>%
        summarise_at(c("mean_incidence", "low_incidence", "high_incidence", "mean_tsb_count"), sum)
     
    # Keep just the first row of the output data, remove age groups (and adults incidence values)
      app1 <- app1 %>%
        filter(age_group == "adults") %>%
        select(-c("age_group", "mean_incidence", "low_incidence", "high_incidence", "mean_tsb_count"))
      # Add in the summed incidence values
        app1 <- cbind(app1[,1:2], incidences[,2:4], app1[,3:5], incidences[5], app1[,6:length(app1)])
   
    # Rename columns     
      colnames(app1) <- c("Country", "WHO Region", "Incidence", "Low Incidence", "High Incidence",
                          "Prop. Treatment Seeking Behaviour (TSB)", "Low Prop. TSB", "High Prop. TSB",
                          "Number Seeking Treatment", "Prop. G6PD Deficient (G6PDd)", "Prop. Pregnant/Lactating",
                          "Prop. Total Confirmed Cases", "Prop. RDT Confirmed Cases", "Days Lost/Case",
                          "Carer Days Lost/Case", "Blood Stage Treatment", "PQ Use", "Routine G6PD Screening",
                          "GDP/capita/day (2017)", "Total Cost of IP Visit (TCIP)",
                          "Total Cost of OP Visit (TCOP)", "Malaria Dx Cost - Micro", "Malaria Dx Cost - RDT",
                          "P.v. Bloodstage Cure Cost/Case", "Cost/PQ Treatment", "Household Cost/Case",
                          "Productivity Cost/Case", "Carer Productivity Cost/Case")
    
# S6 Table
  # Base Case:        1. Country
  #                   2. Total Provider Cost/Case Seeking Treatment
  #                   3. SA: Excluding productivity losses in children
  # Supervised PQ:    1. Cost of G6PD screening
  #                   2. Cost of supervision
  #                   3. Cost of case management (Cost of visit/case presenting, Cost of malaria Dx/case presenting,
  #                                               Cost of bloodstage cure/case presenting, Cost of high dose PQ)
  #                   4. Total provider costs
  #                   5. Total direct household costs
  #                   6. Total indirect household costs
  #                   7. Total household costs
  #                   8. Total costs
  # Unsupervised PQ:  As above, excluding cost of supervision
    # Supervised scenario  
      app3 <- radicalcure_sup %>%
        select(country, mean_tsb_count, mean_total_provcost, cScreening, hd_cSupervision_base,
               cCare, sup.total_provcost, sup.dirhh_cost, sup.indirhh_cost, sup.totalhh_cost,
               sup.overallcost_mean) %>%
        group_by(country) %>%
        summarise_if(is.numeric, sum) %>%
        mutate(provcost_percase = mean_total_provcost/mean_tsb_count) %>%
        select(-c(mean_total_provcost, mean_tsb_count))
    # Unsupervised scenario
      unsup <- radicalcure_unsup %>%
        select(country, cScreening, cCare, unsup.total_provcost, unsup.dirhh_cost,
               unsup.indirhh_cost, unsup.totalhh_cost, unsup.overallcost_mean) %>%
        group_by(country) %>%
        summarise_if(is.numeric, sum)
    # Join together, round, and rename
      app3 <- cbind(app3[c(1,10)], output_base$SA.Estimate[1:44], app3[2:9], output_radsup$CrI.Lower[1:44],
                    output_radsup$CrI.Upper[1:44], unsup[,2:ncol(unsup)], output_radunsup$CrI.Lower[1:44],
                    output_radunsup$CrI.Upper[1:44])
      colnames(app3) <- c("Country", "Base - Provider Cost/Case", "Base SA - Excluded Productivity Losses in Children",
                          "SupPQ - Cost of G6PD Screening", "SupPQ - Cost of Supervision",
                          "SupPQ - Case Management Cost", "SupPQ - Total Provider Cost", "SupPQ - Direct Household Cost",
                          "SupPQ - Indirect Household Cost", "SupPQ - Total Household Cost", "SupPQ - Total Cost",
                          "SupPQ - Total Cost Lower 95% CrI", "SupPQ - Total Cost Upper 95% CrI",
                          "UnsupPQ - Cost of G6PD Screening", "UnsupPQ - Case Management Cost",
                          "UnsupPQ - Total Provider Cost", "UnsupPQ - Direct Household Cost",
                          "UnsupPQ - Indirect Household Cost", "UnsupPQ - Total Household Cost", "UnsupPQ - Total Cost",
                          "UnsupPQ - Total Cost Lower 95% CrI", "UnsupPQ - Total Cost Upper 95% CrI")
      app3 <- app3 %>%
        bind_rows(summarise_all(., list(function(x) if(is.numeric(x)) sum(x) else "Total"))) %>%
        mutate_at(vars(3:18), round) %>%
        mutate_at(2, round, 2)
      # Need to remove total for base provider cost per case (doesn't make sense to sum costs per case)
        app3[45,2] <- NA

# S7 Table
  # Country, Age-Group, Base PV Cases, Supervised PV Cases, Unsupervised PV Cases
    app4 <- radicalcure_sup %>%
      select(c("country", "age_group", "mean_incidence", "sup.incidence"))
    app4 <- cbind(app4, radicalcure_unsup$unsup.incidence, baseline$mean_tsb_count, radicalcure_sup$sup.mean_tsb_count,
                  radicalcure_unsup$unsup.mean_tsb_count)
        
  # Cases prescribed PQ, Case taking effective PQ - Base Case, Supervised PQ, Unsupervised PQ
    baseline <- baseline %>%
      mutate(cases_rxPQ =
              case_when(pq_use == "N" ~ 0,
                        age_group == "adults" & country != "Malaysia" & pq_use == "Y" ~
                          (1-preg.lact) * pq_prescribed_mean * mean_tsb_count,
                        age_group == "adults" & country == "Malaysia" ~
                          (g6pdd * (1-g6pd.sens) + (1-g6pdd) * g6pd.spec) * (1-preg.lact) * mean_tsb_count,
                        age_group == "children" & country != "Malaysia" & pq_use == "Y" ~
                          pq_prescribed_mean * mean_tsb_count,
                        age_group == "children" & country == "Malaysia" ~
                          (g6pdd * (1-g6pd.sens) + (1-g6pdd) * g6pd.spec) * mean_tsb_count,
                        age_group == "infants" & country != "Malaysia" & pq_use == "Y" ~
                          0.8 * pq_prescribed_mean * mean_tsb_count,
                        age_group == "infants" & country == "Malaysia" ~
                          (g6pdd * (1-g6pd.sens) + (1-g6pdd) * g6pd.spec) * 0.8 * mean_tsb_count),
            cases_effPQ = cases_rxPQ * unsupervised.eff_mean) %>%
          mutate_at(vars("cases_rxPQ", "cases_effPQ"), ~round(.))
    
    radicalcure_sup <- radicalcure_sup %>%
      mutate(cases_effPQ = PQ.pop.b_mean * mean_PQ.base) %>%
      mutate_at("cases_effPQ", ~round(.))
    
    radicalcure_unsup <- radicalcure_unsup %>%
      mutate(cases_effPQ = PQ.pop.b_mean * unsupervised.eff_mean) %>%
      mutate_at("cases_effPQ", ~round(.))
        
    app4 <- cbind(app4, baseline$cases_rxPQ, radicalcure_sup$PQ.pop.b_mean, radicalcure_unsup$PQ.pop.b_mean,
                  baseline$cases_effPQ, radicalcure_sup$cases_effPQ, radicalcure_unsup$cases_effPQ)
        
  # Provider costs, direct household costs, indirect household costs, total cost - Base Case, Supervised PQ, Unsupervised PQ
    app4 <- cbind(app4,
                  baseline$mean_total_provcost, radicalcure_sup$sup.total_provcost, radicalcure_unsup$unsup.total_provcost,
                  baseline$mean_direct_patcost, radicalcure_sup$sup.dirhh_cost, radicalcure_unsup$unsup.dirhh_cost,
                  baseline$mean_indirect_patcost, radicalcure_sup$sup.indirhh_cost, radicalcure_unsup$unsup.indirhh_cost,
                  baseline$mean_total_cost, radicalcure_sup$sup.overallcost_mean, radicalcure_unsup$unsup.overallcost_mean)
        
  # Round numeric values, add total row and rename columns
    app4 <- app4 %>% mutate_if(is.numeric, round) %>%
      bind_rows(summarise_all(., list(function(x) if(is.numeric(x)) sum(x) else "Total")))
        
    colnames(app4) <- c("Country", "Age Band", "Cases - Base", "Cases - SupPQ", "Cases - UnsupPQ",
                        "Cases Seeking Treatment - Base", "Cases Seeking Treatment - SupPQ",
                        "Cases Seeking Treatment - UnsupPQ", "Cases Rx PQ - Base", "Cases Rx PQ - SupPQ",
                        "Cases Rx PQ - UnsupPQ", "Cases taking eff. PQ - Base", "Cases taking eff. PQ - SupPQ",
                        "Cases taking eff. PQ - UnsupPQ",  "Provider Costs - Base", "Provider Costs - Sup",
                        "Provider Costs - Unsup", "Direct Household Costs - Base", "Direct Household Costs - Sup",
                        "Direct Household Costs - Unsup", "Indirect Household Costs - Base",
                        "Indirect Household Costs - Sup", "Indirect Household Costs - Unsup", "Total Costs - Base",
                        "Total Costs - Sup", "Total Costs - Unsup")
        
# Additional outputs reported in the text
  # Country, # G6PDd given PQ, Cost in supervised case for changing effectiveness of PQ, cases in unsupervised
  # PQ case if efficacy of unsupervised PQ is change
    extra <- radicalcure_sup %>%
      select(country, g6pdd, pop.tested.b_mean) %>%
      mutate(g6pdd_recPQ = (g6pdd*(1-g6pd.sens))*pop.tested.b_mean)
    extra <- cbind(extra, effect_sa1[,"sup.overallcost_mean"], effect_sa2[,"sup.overallcost_mean"],
                   efficacy_sa2[,"unsup.incidence"], efficacy_sa1[,"unsup.incidence"])
  # Select appropriate columns and summarise by country
    extra <- extra %>%
      select(country, g6pdd_recPQ, `effect_sa1[, "sup.overallcost_mean"]`, `effect_sa2[, "sup.overallcost_mean"]`,
             `efficacy_sa2[, "unsup.incidence"]`, `efficacy_sa1[, "unsup.incidence"]`) %>%
      group_by(country) %>%
      summarise_if(is.numeric, sum) %>%
      bind_rows(summarise_all(., list(function(x) if(is.numeric(x)) sum(x) else "Total"))) %>%
      mutate_if(is.numeric, round) %>%
      rename("Country" = country, "No. with G6PDd given PQ" = g6pdd_recPQ,
             "SupPQ - Cost if PQ.base is lowered" = `effect_sa1[, "sup.overallcost_mean"]`,
             "SupPQ - Cost if PQ.base is increased" = `effect_sa2[, "sup.overallcost_mean"]`,
             "UnsupPQ - Cases if unsupervised.eff is lowered" = `efficacy_sa2[, "unsup.incidence"]`,
             "UnsupPQ - Cases if unsupervised.eff is increased" =`efficacy_sa1[, "unsup.incidence"]`)
      
  

### OUTPUT ALL TABLES 
    write.csv(tab1,'outputs/Table 1.csv')
    write.csv(tab2,'outputs/Table 2.csv', row.names=FALSE)
    write.csv(tab3,'outputs/Table 3.csv', row.names=TRUE)
    write.csv(tab4,'outputs/Table 4.csv', row.names=FALSE)
    write.csv(app1,'outputs/S1-Table.csv', row.names=FALSE)
    write.csv(app3,'outputs/S3-Table.csv', row.names=FALSE)
    write.csv(app4,'outputs/S4-Table.csv', row.names=FALSE)
    write.csv(extra,'outputs/Extra.csv', row.names=FALSE)
