# FaunalDegradationAGB

# All data are stored in the "Data" folder. 
# "HelperScripts" contain files used for pre-simulation data management and processing. The "SimulationFunctions" folder within "HelperScripts" contains separate functions to simulate the removal (with replacement) and re-calculation of plot AGB for each taxa for both the observed and imputed data, and with and without assuming compensation. 
# "RunSimulations" contains all simulation files (R files beginning with "Simulations") for each taxa for both the observed and imputed data, and with and without assuming compensation. These scripts run functions located in the "SimulationFunctions" folder under "HelperScripts". Simulation files were batch run in the Duke Computing Cluster. 
# All model outputs are stored in the "Outputs" folder. 
# "ImputedDataCompilation" compiles imputation outputs. 
# "Simulation Summary.Rmd" compiles and processes simulation results for the observed data and both imputation scenarios and produces visualizations of the results. 
# "StandVariables.Rmd" calculates differences in plot AGB, DBH, and wood density between dispersed and non-dispersed trees and produces visualizations of the results. 
# "DietaryRedundancy.Rmd" calculates niche breadth/ width and modularity (including c and z values) for the observed and imputed data. 

