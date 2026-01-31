
###IMPUTATION DATA MANAGEMENT###
  
#load packages
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("ggpubr")
#install.packages("abind")
library(dplyr);library(abind)

#read in data 
tdat_frug2_df <- read.csv(file=paste0(data_path,"tdat_frug2.csv"))#read in the interaction data
tdat_total_df <- read.csv(file=paste0(data_path,"agb-trees-complete.csv")) # read in the tree data
plot_subset <- read.csv(file=paste0(data_path,"model-data.csv"))# read in the plot data
plot_subset2 <- plot_subset %>% select(DispoCode)
plot_agb <- read.csv(file=paste0(data_path,"plot_agb.csv"))
animal_taxa <- read.csv(file=paste0(data_path,"animal_taxa.csv"))
animal_taxa <- animal_taxa %>% filter(Species != "Dyaphorophyia_castanea", Species != "Melaniparus_funereus")

#list of the unique plots
plot <- unique(plot_subset2$DispoCode)

#subset data to only include the relevant 260 plots
tdat_total <- tdat_total_df %>% filter(DispoCode %in% plot) 
tdat_frug2 <- tdat_frug2_df %>% filter(DispoCode %in% plot) 

#Organize data to account for dietary redundancy
tdat_frug2$DispersedSolo <- tdat_frug2$NDispersed
tdat_frug2$DispersedSolo[tdat_frug2$DispersedSolo!=1] <- 0

#Organize data for the taxa-level simulations
tdat_frug2 <- tdat_frug2 %>%
  mutate(ele = select(., Loxodonta_cyclotis) %>% rowSums(na.rm = TRUE))
tdat_frug2 <- tdat_frug2 %>%
  mutate(ape = select(., Gorilla_gorilla, Pan_troglodytes ) %>% rowSums(na.rm = TRUE))
tdat_frug2 <- tdat_frug2 %>%
  mutate(ceph = select(., c(animal_taxa$Species[animal_taxa$Taxa=="Ungulate"])) %>% rowSums(na.rm = TRUE))
tdat_frug2 <- tdat_frug2 %>%
  mutate(bat = select(., c(animal_taxa$Species[animal_taxa$Taxa=="Bats"]) ) %>% rowSums(na.rm = TRUE))
tdat_frug2 <- tdat_frug2 %>%
  mutate(monkey = select(., c(animal_taxa$Species[animal_taxa$Taxa=="Monkeys"])) %>% rowSums(na.rm = TRUE))
tdat_frug2 <- tdat_frug2 %>%
  mutate(carnivore = select(., c(animal_taxa$Species[animal_taxa$Taxa=="Carnivore"])) %>% rowSums(na.rm = TRUE))
tdat_frug2 <- tdat_frug2 %>%
  mutate(bird = select(.,c(animal_taxa$Species[animal_taxa$Taxa=="Bird"])) %>% rowSums(na.rm = TRUE))

tdat_frug2$ele[tdat_frug2$ele>=1] <- 1
tdat_frug2$ape[tdat_frug2$ape>=1] <- 1
tdat_frug2$ceph[tdat_frug2$ceph>=1] <- 1
tdat_frug2$monkey[tdat_frug2$monkey>=1] <- 1
tdat_frug2$carnivore[tdat_frug2$carnivore>=1] <- 1
tdat_frug2$bird[tdat_frug2$bird>=1] <- 1
tdat_frug2$bat[tdat_frug2$bat>=1] <- 1

tdat_frug2$NDispersed_Taxa <- tdat_frug2$ele +tdat_frug2$ape +tdat_frug2$ceph +tdat_frug2$bat +tdat_frug2$monkey +tdat_frug2$bird + tdat_frug2$carnivore

tdat_frug2$ele2 <- ifelse((tdat_frug2$NDispersed_Taxa==1 & tdat_frug2$ele==1) >0, 1, 0)
tdat_frug2$ape2 <- ifelse((tdat_frug2$NDispersed_Taxa==1 & tdat_frug2$ape==1) >0, 1, 0)
tdat_frug2$ceph2 <- ifelse((tdat_frug2$NDispersed_Taxa==1 & tdat_frug2$ceph==1) >0, 1, 0)
tdat_frug2$bat2 <- ifelse((tdat_frug2$NDispersed_Taxa==1 & tdat_frug2$bat==1) >0, 1, 0)
tdat_frug2$monkey2 <- ifelse((tdat_frug2$NDispersed_Taxa==1 & tdat_frug2$monkey==1) >0, 1, 0)
tdat_frug2$bird2 <- ifelse((tdat_frug2$NDispersed_Taxa==1 & tdat_frug2$bird==1) >0, 1, 0)
tdat_frug2$carnivore2 <- ifelse((tdat_frug2$NDispersed_Taxa==1 & tdat_frug2$carnivore==1) >0, 1, 0)
tdat_frug2$Endangered2 <- ifelse((tdat_frug2$NDispersed_Taxa==1 & tdat_frug2$Endangered==1) >0, 1, 0)

tdat_frug2 <- tdat_frug2 %>% select(-c(Atimastillas_flavigula:Tragelaphus_eurycerus))

write.csv(tdat_frug2, file=paste0(data_path,"tdat_frug2_observed.csv"))


