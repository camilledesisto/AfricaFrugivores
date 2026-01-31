###SIMULATIONS###
args <- commandArgs(trailingOnly = TRUE)
j <- args[1]
set.seed(j)

#wd_path <- "/Users/camilledesisto/Documents/GitHub/FaunalDegradationABGNew/"
wd_path <- '/hpc/group/dunsonlab/cd306/FaunalDegradationAGBNew'
getwd()
setwd(wd_path)
data_path <- 'Data/'
save_path <- 'Outputs/Observed/'
source_path <- 'HelperScripts/'

#Load functions
source(file=paste0(source_path, "SimulationFunctions/endangered_simulation_nr_obs.R"))
Endangered_simulation_draws_nr(Endangered_simulation_nr)

rm(list = ls(all.names = TRUE)) #clear all objects
gc()#free up/ check memory 