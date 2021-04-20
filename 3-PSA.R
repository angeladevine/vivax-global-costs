#####################################################################################################################
#   R-script:     3-PSA.R
#   Project:		  "Global economic costs due to vivax malaria and the potential impact of its radical cure: A modelling study" by A Devine et al.
#
#   Data used:	  (1) "baseline.Rdata"
#                 (2) "radcure.Rdata"
#   Data created:	"psa.Rdata", "PSA_CrIs.csv", "baseline_PSACrIs.csv", "RadicalCure_SupPQ_PSACrIs.csv" &
#                 "RadicalCure_UnsupPQ_PSACrIs.csv"
#
#   Purpose:  		Running the probabilistic sensitivity analysis (PSA) for the baseline and two radical cure scenarios
#
#   Date:			    14-April-2021
#   Author: 		  Niamh Meagher & Angela Devine
#####################################################################################################################

source('3a-PSA-parameters.R')
source('3b-PSA-distributions.R')
source('3c-PSA-models.R')
source('3d-PSA-simulations.R')

# Initialise data frame with the first row
output <-
  data.frame(
    "country" = simulation_res[[1]][["country"]][[1]],
    "age_group" = simulation_res[[1]][["age_group"]][[1]],
    "base_L" = quantile(simulation_res[[1]][["total_cost"]], probs = 0.025),
    "base_U" = quantile(simulation_res[[1]][["total_cost"]], probs = 0.975),
    "sup_L" = quantile(simulation_res[[1]][["overallcost_sup"]], probs = 0.025),
    "sup_U" = quantile(simulation_res[[1]][["overallcost_sup"]], probs = 0.975),
    "unsup_L" = quantile(simulation_res[[1]][["overallcost_unsup"]], probs = 0.025),
    "unsup_U" = quantile(simulation_res[[1]][["overallcost_unsup"]], probs = 0.975)
  )

# Append the output from the rest of the rows
for (i in 2:length(simulation_res)) {
  output <-
    rbind(
      output,
      data.frame(
        "country" = simulation_res[[i]][["country"]][[1]],
        "age_group" = simulation_res[[i]][["age_group"]][[1]],
        "base_L" = quantile(simulation_res[[i]][["total_cost"]], probs = 0.025),
        "base_U" = quantile(simulation_res[[i]][["total_cost"]], probs = 0.975),
        "sup_L" = quantile(simulation_res[[i]][["overallcost_sup"]], probs = 0.025),
        "sup_U" = quantile(simulation_res[[i]][["overallcost_sup"]], probs = 0.975),
        "unsup_L" = quantile(simulation_res[[i]][["overallcost_unsup"]], probs = 0.025),
        "unsup_U" = quantile(simulation_res[[i]][["overallcost_unsup"]], probs = 0.975)
      )
    )
}

# Aggregate by country, round and output as CSV
output <- output %>%
  select(-c(age_group)) %>%
  mutate_if(is.numeric, round) %>%
  group_by(country) %>%
  summarise_if(is.numeric, sum) %>%
  bind_rows(summarise_all(., list(function(x) if(is.numeric(x)) sum(x) else "Total"))) %>%
  rename(
    "Country" = country,
    "Baseline CrI Lower" = base_L,
    "Baseline CrI Upper" = base_U,
    "Sup Case CrI Lower" = sup_L,
    "Sup Case CrI Upper" = sup_U,
    "Unsup Case CrI Lower" = unsup_L,
    "Unsup Case CrI Upper" = unsup_U
  )
write.csv(output, 'outputs/PSA/PSA_CrIs.csv', row.names = FALSE)

# Add to previous output data
# Baseline
base <- read.csv('outputs/baseline.csv')
output_base <-
  cbind(base[, c(1:5)], output[c(2:3)], base[, c(8)]) %>%
  rename(
    "CrI.Lower" = "Baseline CrI Lower",
    "CrI.Upper" = "Baseline CrI Upper",
    "SA.Estimate" = "base[, c(8)]"
  )
write.csv(output_base, 'outputs/Baseline_PSACrIs.csv', row.names = FALSE)

# Supervised Radical Cure
radsup <- read.csv('outputs/RadicalCure_SupPQ.csv')
output_radsup <-
  cbind(radsup[, c(1:6)], output[c(4:5)], radsup[, c(7:8)]) %>%
  rename("CrI.Lower" = "Sup Case CrI Lower", "CrI.Upper" = "Sup Case CrI Upper")
write.csv(output_radsup,
          'outputs/RadicalCure_SupPQ_PSACrIs.csv',
          row.names = FALSE)

# Unsupervised Radical Cure
radunsup <- read.csv('outputs/RadicalCure_UnsupPQ.csv')
output_radunsup <- cbind(radunsup[, c(1:6)], output[c(6:7)]) %>%
  rename("CrI.Lower" = "Unsup Case CrI Lower", "CrI.Upper" = "Unsup Case CrI Upper")
write.csv(output_radunsup,
          'outputs/RadicalCure_UnsupPQ_PSACrIs.csv',
          row.names = FALSE)

# Save image for output tables
d <- Sys.Date()
save.image(file = "outputs/psa.Rdata")
