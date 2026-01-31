# FaunalDegradationAGB

# "HelperScripts" contain files used for pre-simulation data management and processing. The "SimulationFunctions" folder within "HelperScripts" contains separate functions to simulate the removal (with replacement) and re-calculation of plot AGB for each taxa for both the observed and imputed data, and with and without assuming compensation. 
# "RunSimulations" contains all simulation files (R files beginning with "Simulations") for each taxa for both the observed and imputed data, and with and without assuming compensation. These scripts run functions located in the "SimulationFunctions" folder under "HelperScripts". Simulation files were batch run in the Duke Computing Cluster. 
# "ImputedDataCompilation" compiles imputation outputs. 
# "SimulationSummary.Rmd" compiles and processes simulation results for the observed data and both imputation scenarios and produces visualizations of the results. 
# "StandVariables2.Rmd" calculates differences in plot AGB, DBH, and wood density between dispersed and non-dispersed trees and produces visualizations of the results. 
# "DietaryRedundancy.Rmd" calculates niche breadth/ width and modularity (including c and z values) for the observed and imputed data. 
# "GeospatialAnalysis.Rmd" is the code to run models relating AGB change to plot variables.
# Anonymized data are available tdat_frug2_observed_ANON.csv and tdat_frug2_imputed_ANON.xslx
