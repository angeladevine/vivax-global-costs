#####################################################################################################################
#   R-script:     3d-PSA-simulations.R
#   Project:		  "Global economic costs due to vivax malaria and the potential impact of its radical cure: A modelling study" by A Devine et al.
#
#   Purpose:  		Generate list of sampled parameter values for PSA
#
#   Date:			    14-April-2021
#   Author: 		  Niamh Meagher & Angela Devine
#####################################################################################################################

# # # # #
# SETTING UP
# # # # #

# Create file for PSA outputs
if (!file.exists("./outputs/PSA")) {
  dir.create("./outputs/PSA")
}

# Set number of simulations
sim <- 10000

# Set distributions for each parameter
normal <- "_incidence"
beta <-
  c(
    "_tsb_prop",
    "_PQ_pres",
    "_PQ_prop_relapses",
    "_unsup_eff",
    "_g6pd_sens",
    "_g6pd_spec"
  )
gamma <-
  c(
    "_cost_pervisit",
    "_dxcost_percase",
    "_bloodstage_cost",
    "_PQ_cost" ,
    "_hhcost_percase",
    "_supcost"
  )
# The G6PD screening parameter "_g6pd_fst" applies to Malaysia only: "g6pd_rdt" applied to all other countries
# Get the results for these separately (both are gamma distributions)

# Days lost to illness applies only to children and adults, not infants
# Obtain parameters just for adults and children
# Carer days lost - added small amounts to each lower bound so gamma distribution suits

# Set up distribution values data frame with identifying variables
dist.pars <- all.params %>% select(country, age_group, pq_use)






# # # # #
# OBTAINING DISTRIBUTIONS
# # # # #

# Generate the values for each of the specified parameters, depending on the specified base, lower and upper values
# Normal distribution
for (i in 1:nrow(dist.pars)) {
  for (v in normal) {
    dist.pars[i, paste0("mean", v)] <-
      findnorm(mean = all.params[i, paste0("mean", v)],
               L = all.params[i, paste0("low", v)],
               U = all.params[i, paste0("high", v)])$mean
    dist.pars[i, paste0("sd", v)] <-
      findnorm(mean = all.params[i, paste0("mean", v)],
               L = all.params[i, paste0("low", v)],
               U = all.params[i, paste0("high", v)])$sd
  }
}

# Beta distribution
for (i in 1:nrow(dist.pars)) {
  for (v in beta) {
    dist.pars[i, paste0("a", v)] <-
      findbeta2(tmean = all.params[i, paste0("mean", v)],
                l95 = all.params[i, paste0("low", v)],
                u95 = all.params[i, paste0("high", v)])$a
    dist.pars[i, paste0("b", v)] <-
      findbeta2(tmean = all.params[i, paste0("mean", v)],
                l95 = all.params[i, paste0("low", v)],
                u95 = all.params[i, paste0("high", v)])$b
  }
}

# Gamma distribution
for (i in 1:nrow(dist.pars)) {
  for (v in gamma) {
    dist.pars[i, paste0("shape", v)] <-
      findgamma2(tmean = all.params[i, paste0("mean", v)],
                 l95 = all.params[i, paste0("low", v)],
                 u95 = all.params[i, paste0("high", v)])$shape
    dist.pars[i, paste0("scale", v)] <-
      findgamma2(tmean = all.params[i, paste0("mean", v)],
                 l95 = all.params[i, paste0("low", v)],
                 u95 = all.params[i, paste0("high", v)])$scale
  }
}

# G6PD testing costs - different by country
# Gamma distribution
for (i in which(all.params$country == "Malaysia")) {
  dist.pars[i, "shape_g6pd_fst"] <-
    findgamma2(tmean = all.params[i, "mean_g6pd_fst"],
               l95 = all.params[i, "low_g6pd_fst"],
               u95 = all.params[i, "high_g6pd_fst"])$shape
  dist.pars[i, "scale_g6pd_fst"] <-
    findgamma2(tmean = all.params[i, "mean_g6pd_fst"],
               l95 = all.params[i, "low_g6pd_fst"],
               u95 = all.params[i, "high_g6pd_fst"])$scale
}
for (i in which(all.params$country != "Malaysia")) {
  dist.pars[i, "shape_g6pd_rdt"] <-
    findgamma2(tmean = all.params[i, "mean_g6pd_rdt"],
               l95 = all.params[i, "low_g6pd_rdt"],
               u95 = all.params[i, "high_g6pd_rdt"])$shape
  dist.pars[i, "scale_g6pd_rdt"] <-
    findgamma2(tmean = all.params[i, "mean_g6pd_rdt"],
               l95 = all.params[i, "low_g6pd_rdt"],
               u95 = all.params[i, "high_g6pd_rdt"])$scale
}

# Days lost to illness - only apply to adults and children (not infants)
# Gamma distribution
for (i in which(all.params$age_group != "infants")) {
  dist.pars[i, "shape_dayslost_percase"] <-
    findgamma2(tmean = all.params[i, "mean_dayslost_percase"],
               l95 = all.params[i, "low_dayslost_percase"],
               u95 = all.params[i, "high_dayslost_percase"])$shape
  dist.pars[i, "scale_dayslost_percase"] <-
    findgamma2(tmean = all.params[i, "mean_dayslost_percase"],
               l95 = all.params[i, "low_dayslost_percase"],
               u95 = all.params[i, "high_dayslost_percase"])$scale
}

# Carer days lost to illness - when the lower limit was 0, we replace with a very small value (0.01)
for (i in which(all.params$low_carerdays_percase != 0)) {
  dist.pars[i, "shape_carerdays_percase"] <-
    findgamma2(tmean = all.params[i, "mean_carerdays_percase"],
               l95 = all.params[i, "low_carerdays_percase"],
               u95 = all.params[i, "high_carerdays_percase"])$shape
  dist.pars[i, "scale_carerdays_percase"] <-
    findgamma2(tmean = all.params[i, "mean_carerdays_percase"],
               l95 = all.params[i, "low_carerdays_percase"],
               u95 = all.params[i, "high_carerdays_percase"])$scale
}
for (i in which(all.params$low_carerdays_percase.01 == 0.01)) {
  dist.pars[i, "shape_carerdays_percase.01"] <-
    findgamma2(tmean = all.params[i, "mean_carerdays_percase"],
               l95 = all.params[i, "low_carerdays_percase.01"],
               u95 = all.params[i, "high_carerdays_percase"])$shape
  dist.pars[i, "scale_carerdays_percase.01"] <-
    findgamma2(tmean = all.params[i, "mean_carerdays_percase"],
               l95 = all.params[i, "low_carerdays_percase.01"],
               u95 = all.params[i, "high_carerdays_percase"])$scale
}


# Plot the distributions 
# Need to obtain distributions for each country and age group:
country <- levels(factor(all.params$country))
age <- levels(factor(all.params$age_group))
# Also need to determine the countries which have lower limit carer days of 0 to replace with 0.01 for gamma distribution
carerdays0 <-
  all.params[which(all.params$low_carerdays_percase == 0), "country"]

for (c in country) {
  for (a in age) {
    # Create two empty dataframes:  1) for sampled values
    #                               2) for lower and upper bound estimates
    samp_dat <- NULL
    param_dat <- NULL
    
    # Normal distribution
    for (i in which(all.params$country == c &
                    all.params$age_group == a)) {
      for (v in normal) {
        # Generate the values to plot
        xvals <-
          seq(0.9 * all.params[i, paste0("low", v)], 1.1 * all.params[i, paste0("high", v)], length.out = 100)
        yvals <-
          dnorm(xvals, mean = dist.pars[i, paste0("mean", v)], sd = dist.pars[i, paste0("sd", v)])
        # Add sampled values to the samples dataframe for plotting
        samp_dat <-
          rbind(samp_dat,
                data.frame(
                  "x" = xvals,
                  "y" = yvals,
                  "dist" = "normal",
                  "par" = v
                ))
        # Add parameter values used to obtain distribution in the parameter dataframe
        param_dat <-
          rbind(param_dat, melt(all.params[i, c(paste0("mean", v), paste0("low", v), paste0("high", v))]) %>%
                  mutate(par = v))
      }
      # Beta distribution
      for (v in beta) {
        # Generate the values to plot
        xvals <-
          seq(0.9 * all.params[i, paste0("low", v)], 1.1 * all.params[i, paste0("high", v)], length.out = 100)
        yvals <-
          dbeta(xvals, shape1 = dist.pars[i, paste0("a", v)], shape2 = dist.pars[i, paste0("b", v)])
        # Add sampled values to the samples dataframe for plotting
        samp_dat <-
          rbind(samp_dat,
                data.frame(
                  "x" = xvals,
                  "y" = yvals,
                  "dist" = "beta",
                  "par" = v
                ))
        # Add parameter values used to obtain distribution in the parameter dataframe
        param_dat <-
          rbind(param_dat, melt(all.params[i, c(paste0("mean", v), paste0("low", v), paste0("high", v))]) %>%
                  mutate(par = v))
      }
      
      # Gamma distribution
      for (v in gamma) {
        # Generate the values to plot
        xvals <-
          seq(0.9 * all.params[i, paste0("low", v)], 1.1 * all.params[i, paste0("high", v)], length.out = 100)
        yvals <-
          dgamma(xvals, shape = dist.pars[i, paste0("shape", v)], scale = dist.pars[i, paste0("scale", v)])
        # Add sampled values to the samples dataframe for plotting
        samp_dat <-
          rbind(samp_dat,
                data.frame(
                  "x" = xvals,
                  "y" = yvals,
                  "dist" = "gamma",
                  "par" = v
                ))
        # Add parameter values used to obtain distribution in the parameter dataframe
        param_dat <-
          rbind(param_dat, melt(all.params[i, c(paste0("mean", v), paste0("low", v), paste0("high", v))]) %>%
                  mutate(par = v))
      }
      
      # G6PD testing costs - different for Malaysia only (gamma)
      if (c == "Malaysia") {
        xvals <-
          seq(0.9 * all.params[i, "low_g6pd_fst"], 1.1 * all.params[i, "high_g6pd_fst"], length.out = 100)
        yvals <-
          dgamma(xvals, shape = dist.pars[i, "shape_g6pd_fst"], scale = dist.pars[i, "scale_g6pd_fst"])
        # Add sampled values to the samples dataframe for plotting
        samp_dat <-
          rbind(
            samp_dat,
            data.frame(
              "x" = xvals,
              "y" = yvals,
              "dist" = "gamma",
              "par" = "_g6pd_fst"
            )
          )
        # Add parameter values used to obtain distribution in the parameter dataframe
        param_dat <-
          rbind(param_dat,
                melt(all.params[i, c("mean_g6pd_fst", "low_g6pd_fst", "high_g6pd_fst")]) %>%
                  mutate(par = "_g6pd_fst"))
      } else {
        xvals <-
          seq(0.9 * all.params[i, "low_g6pd_rdt"], 1.1 * all.params[i, "high_g6pd_rdt"], length.out = 100)
        yvals <-
          dgamma(xvals, shape = dist.pars[i, "shape_g6pd_rdt"], scale = dist.pars[i, "scale_g6pd_rdt"])
        # Add sampled values to the samples dataframe for plotting
        samp_dat <-
          rbind(
            samp_dat,
            data.frame(
              "x" = xvals,
              "y" = yvals,
              "dist" = "gamma",
              "par" = "_g6pd_rdt"
            )
          )
        # Add parameter values used to obtain distribution in the parameter dataframe
        param_dat <-
          rbind(param_dat,
                melt(all.params[i, c("mean_g6pd_rdt", "low_g6pd_rdt", "high_g6pd_rdt")]) %>%
                  mutate(par = "_g6pd_rdt"))
      }
      
      # Days lost to illness - only apply to children and adults (not infants)
      if (a != "infants") {
        xvals <-
          seq(0.9 * all.params[i, "low_dayslost_percase"], 1.1 * all.params[i, "high_dayslost_percase"], length.out = 100)
        yvals <-
          dgamma(xvals, shape = dist.pars[i, "shape_dayslost_percase"], scale = dist.pars[i, "scale_dayslost_percase"])
        # Add sampled values to the samples dataframe for plotting
        samp_dat <-
          rbind(
            samp_dat,
            data.frame(
              "x" = xvals,
              "y" = yvals,
              "dist" = "gamma",
              "par" = "_dayslost_percase"
            )
          )
        # Add parameter values used to obtain distribution in the parameter dataframe
        param_dat <-
          rbind(param_dat,
                melt(all.params[i, c("mean_dayslost_percase",
                                     "low_dayslost_percase",
                                     "high_dayslost_percase")]) %>% mutate(par = "_dayslost_percase"))
      }
      
      # Carer days lost to illness - lower limits replaced when 0
      if (c %in% carerdays0) {
        xvals <-
          seq(0.9 * all.params[i, "low_carerdays_percase.01"], 1.1 * all.params[i, "high_carerdays_percase"], length.out = 100)
        yvals <-
          dgamma(xvals, shape = dist.pars[i, "shape_carerdays_percase.01"],
                 scale = dist.pars[i, "scale_carerdays_percase.01"])
        # Add sampled values to the samples dataframe for plotting
        samp_dat <-
          rbind(
            samp_dat,
            data.frame(
              "x" = xvals,
              "y" = yvals,
              "dist" = "gamma",
              "par" = "_carerdays_percase.01"
            )
          )
        # Add parameter values used to obtain distribution in the parameter dataframe
        param_dat <-
          rbind(param_dat,
                melt(all.params[i, c(
                  "mean_carerdays_percase",
                  "low_carerdays_percase.01",
                  "high_carerdays_percase"
                )]) %>% mutate(par = "_carerdays_percase.01"))
      } else {
        xvals <-
          seq(0.9 * all.params[i, "low_carerdays_percase"], 1.1 * all.params[i, "high_carerdays_percase"], length.out = 100)
        yvals <-
          dgamma(xvals, shape = dist.pars[i, "shape_carerdays_percase"],
                 scale = dist.pars[i, "scale_carerdays_percase"])
        # Add sampled values to the samples dataframe for plotting
        samp_dat <-
          rbind(
            samp_dat,
            data.frame(
              "x" = xvals,
              "y" = yvals,
              "dist" = "gamma",
              "par" = "_carerdays_percase"
            )
          )
        # Add parameter values used to obtain distribution in the parameter dataframe
        param_dat <-
          rbind(param_dat,
                melt(all.params[i, c(
                  "mean_carerdays_percase",
                  "low_carerdays_percase",
                  "high_carerdays_percase"
                )]) %>% mutate(par = "_carerdays_percase"))
      }
      
      # Fix up naming of the parameter dataframe
      param_dat$var <- rep(c("M", "L", "U"))
      # Plot the distributions
      p1 <- ggplot(samp_dat, aes(x = x, y = y)) +
        geom_line() +
        geom_vline(data = param_dat, aes(xintercept = value, colour =
                                           var)) +
        facet_wrap( ~ par,
                    scales = "free",
                    nrow = 6,
                    ncol = 3) +
        theme_bw()
      
      # Save figure with counyry name and age group
      ggsave(
        filename = paste0("PSA_distributions_", c, "_", a, ".pdf"),
        path = "outputs/PSA",
        plot = p1,
        height = 11.69,
        width = 8.27,
        units = "in"
      )
      
    }
  }
}




# # # # #
# SIMULATIONS
# # # # #

# Set up an empty list - for each country/age group combination, there will be a data frame with
# a set of simulations (determined by "sim") of the relevant parameters.
simulation_res <- vector("list", 0)

# Set seed
set.seed(210720)

# Generate "sim" sampled parameter values for each distribution, using dist.pars
for (c in country) {
  for (a in age) {
    # Create the empty dataframe to put sampled values into
    samp_dat <- NULL
    
    for (i in which(all.params$country == c &
                    all.params$age_group == a)) {
      # Normal distribution
      for (v in normal) {
        # Add in identifying/consistent variables
        samp_dat$country <- rep(paste0(c), sim)
        samp_dat$age_group <- rep(paste0(a), sim)
        samp_dat$pq_use <-
          rep(all.params[which(all.params$country == paste0(c) &
                                 all.params$age_group == paste0(a)), "pq_use"], sim)
        samp_dat$gdp2017_percapday <-
          rep(all.params[which(all.params$country == paste0(c) &
                                 all.params$age_group == paste0(a)), "gdp2017_percapday"], sim)
        samp_dat$preg.lact <-
          rep(all.params[which(all.params$country == paste0(c) &
                                 all.params$age_group == paste0(a)), "preg.lact"], sim)
        samp_dat$g6pdd <-
          rep(all.params[which(all.params$country == paste0(c) &
                                 all.params$age_group == paste0(a)), "g6pdd"], sim)
        # Generate simulated values
        samp_dat <- cbind(samp_dat,
                          data.frame(rnorm(
                            sim, mean = dist.pars[i, paste0("mean", v)],
                            sd = dist.pars[i, paste0("sd", v)]
                          )))
        # Rename column with parameter name
        names(samp_dat)[ncol(samp_dat)] <- paste0(v)
        # Replace negative incidence values with values >= 0
        while (any(bad <-
                   (samp_dat[, ncol(samp_dat)] < 0))) {
          samp_dat[bad, ncol(samp_dat)] <-
            rnorm(1, mean = dist.pars[i, paste0("mean", v)], sd = dist.pars[i, paste0("sd", v)])
        }
      }
      # Beta distribution
      for (v in beta) {
        # Generate simulated values
        samp_dat <- cbind(samp_dat,
                          data.frame(rbeta(
                            sim, shape1 = dist.pars[i, paste0("a", v)],
                            shape2 = dist.pars[i, paste0("b", v)]
                          )))
        # Rename column wih parameter name
        names(samp_dat)[ncol(samp_dat)] <- paste0(v)
      }
      # Gamma distribution
      for (v in gamma) {
        # Generate simulated values
        samp_dat <- cbind(samp_dat,
                          data.frame(rgamma(
                            sim, shape = dist.pars[i, paste0("shape", v)],
                            scale = dist.pars[i, paste0("scale", v)]
                          )))
        # Rename column wih parameter name
        names(samp_dat)[ncol(samp_dat)] <- paste0(v)
      }
      # G6PD cost: FST for Malaysia, RDT everywhere else
      if (c == "Malaysia") {
        # Generate simulated values
        samp_dat <- cbind(samp_dat,
                          data.frame(rgamma(
                            sim, shape = dist.pars[i, "shape_g6pd_fst"],
                            scale = dist.pars[i, "scale_g6pd_fst"]
                          )))
        # Rename column wih parameter name
        names(samp_dat)[ncol(samp_dat)] <- "_g6pd_fst"
        # Add cost of RDT testing as NA
        samp_dat$par_g6pd_rdt <- NA
      } else {
        # Generate simulated values
        samp_dat <- cbind(samp_dat,
                          data.frame(rgamma(
                            sim, shape = dist.pars[i, "shape_g6pd_rdt"],
                            scale = dist.pars[i, "scale_g6pd_rdt"]
                          )))
        # Rename column wih parameter name
        names(samp_dat)[ncol(samp_dat)] <- "_g6pd_rdt"
        # Add cost of FST testing as NA
        samp_dat$par_g6pd_fst <- NA
      }
      # Days of productivity lost: applied to adults and children, not infants
      if (a != "infants") {
        # Generate simulated values
        samp_dat <- cbind(samp_dat,
                          data.frame(rgamma(
                            sim, shape = dist.pars[i, "shape_dayslost_percase"],
                            scale = dist.pars[i, "scale_dayslost_percase"]
                          )))
        # Rename column wih parameter name
        names(samp_dat)[ncol(samp_dat)] <-
          "_dayslost_percase"
      } else {
        samp_dat$par_dayslost_percase <- NA
      }
      # Carer days lost to illness:  lower limits replaced when 0
      if (c %in% carerdays0) {
        # Generate simulated values
        samp_dat <- cbind(samp_dat,
                          data.frame(rgamma(
                            sim, shape = dist.pars[i, "shape_carerdays_percase.01"],
                            scale = dist.pars[i, "scale_carerdays_percase.01"]
                          )))
        # Rename column wih parameter name
        names(samp_dat)[ncol(samp_dat)] <-
          "_carerdays_percase"
      } else {
        # Generate simulated values
        samp_dat <- cbind(samp_dat,
                          data.frame(rgamma(
                            sim, shape = dist.pars[i, "shape_carerdays_percase"],
                            scale = dist.pars[i, "scale_carerdays_percase"]
                          )))
        # Rename column wih parameter name
        names(samp_dat)[ncol(samp_dat)] <-
          "_carerdays_percase"
      }
    }
    # Remove _ from variable names
    samp_dat <-
      samp_dat %>% rename_at(vars(starts_with("_")), ~paste0("par", .))
    # Add simulated data to the list of all simulations
    simulation_res[[paste0(c, "_", a)]] <- samp_dat
  }
}

# Get the baseline total cost estimates for each country and age group combination
for (i in 1:length(simulation_res)) {
  simulation_res[[i]] <- baseline.model(simulation_res[[i]])
}

# Get the radical cure scenarios for each country and age group combination
for (i in 1:length(simulation_res)) {
  simulation_res[[i]] <- radcure.model(simulation_res[[i]])
}
