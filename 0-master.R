################################################################################
#   R-script:     0-master.R
#   Project:		  "Global economic costs due to vivax malaria and the potential impact of its radical cure: A modelling study" by A Devine et al. (PlosMed)
#
#   Purpose:  		Running all analyses involved in the global cost burden of P. vivax
#
#   Date:			    14-April-2021
#   Author: 		  Niamh Meagher & Angela Devine
################################################################################

# NOTE: All data used in analyses are found in the 'inputs' folder. Sources for these described in the publication (see S2 File in particular).

library("tidyverse")
library("readxl")
library("zoo")
library("reshape2")
library("here")          # set working directory

# create a folder for the outputs
if(!file.exists("./outputs")){
  dir.create("./outputs")
}

# to run the baseline analysis
source('1-baseline.R')

# to run the radical cure scenarios
source('2-radical-cure.R')

# to run the probabilistic sensitivity analysis
source('3-PSA.R') # previously PSA_Output.R

# to produce the tables
source('4-output-tables.R')

rm(list = ls())