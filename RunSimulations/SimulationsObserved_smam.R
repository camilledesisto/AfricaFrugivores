###SIMULATIONS###

#wd_path <- "/Users/camilledesisto/Documents/GitHub/FaunalDegradationABG/"
wd_path <- '/hpc/group/dunsonlab/cd306/FaunalDegradationAGB'
getwd()
setwd(wd_path)
data_path <- 'Data/'
save_path <- 'Outputs/Observed/'
source_path <- 'HelperScripts/'
  
#Load functions

smam_simulation <- function(plot){
  
  tdat_frug2b <- tdat_frug2 %>%
    filter(DispoCode == plot)
  
  output <- NULL
  
  for(i in c(25,50,100)){
    
    tdat_frug1 <- tdat_frug2b[sample(which(tdat_frug2b$smam2==1),replace=F, round(i*0.01*length(which(tdat_frug2b$smam2==1)))), ]
    
    tdat_frugb <- anti_join(tdat_frug2b, tdat_frug1)
    
    tdat_frugc <- tdat_frug2b[sample(which(tdat_frug2b$smam2==0),replace=T, (nrow(tdat_frug2b)-nrow(tdat_frugb))), ]
    
    tdat_frugd <- rbind(tdat_frugb, tdat_frugc)
    
    AGB <- sum(tdat_frugd$AGB)
    Plot <- plot
    Orig_AGB <- plot_agb$Plot_AGB[plot_agb$DispoCode==plot]
    AGB_Change <- AGB-Orig_AGB
    PercentRemoved <- i
    
    plot_agb_all <-AGB_Change
    
    output <- rbind(output, plot_agb_all)
  }
  
  all_simulation <- as.data.frame(output)
  all_simulation
  
}
  
  source(file=paste0(source_path, "SimDataManagementObserved.R"))
  
  plot_simulations <- sapply(rep(plot, each=100), FUN= smam_simulation) #Run the simulation
  Plot_simulation <- as.data.frame(abind(plot_simulations))
  Plot_simulation$Plot <- as.vector(rep(plot, each=300))
  Plot_simulation$PercentRemoved <- rep(c(25,50,100), times=26000)
  colnames(Plot_simulation)[1] <- "AGB_Change"
  write.csv(Plot_simulation, file=paste0(x=save_path, "smam/redundancy/output", ".csv"))
  
rm(list = ls(all.names = TRUE)) #clear all objects
gc()#free up/ check memory 
