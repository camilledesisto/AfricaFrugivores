###SIMULATIONS###
args <- commandArgs(trailingOnly = TRUE)
j <- args[1]
set.seed(j)

#wd_path <- "/Users/camilledesisto/Documents/GitHub/FaunalDegradationABG/"
wd_path <- '/hpc/group/dunsonlab/cd306/FaunalDegradationAGB'
getwd()
setwd(wd_path)
data_path <- 'Data/'
save_path <- 'Outputs/Imputed75/'
source_path <- 'HelperScripts/'

#Load functions
source(file=paste0(source_path, "SimulationFunctions/ele_simulation_nr.R"))
ele_simulation_draws_nr(ele_simulation_nr)

rm(list = ls(all.names = TRUE)) #clear all objects
gc()#free up/ check memory 