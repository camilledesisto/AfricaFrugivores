###SIMULATIONS###
args <- commandArgs(trailingOnly = TRUE)
j <- args[1]
set.seed(j)

#wd_path <- "/Users/camilledesisto/Documents/GitHub/FaunalDegradationABG/"
wd_path <- '/hpc/group/dunsonlab/cd306/FaunalDegradationAGBNew'
getwd()
setwd(wd_path)
data_path <- 'Data/'
save_path <- 'Outputs/ImputedB4/'
source_path <- 'HelperScripts/'

#Load functions
source(file=paste0(source_path, "SimulationFunctions/bat_simulation_nr_imp.R"))
bat_simulation_draws_nr(bat_simulation_nr)

rm(list = ls(all.names = TRUE)) #clear all objects
gc()#free up/ check memory 