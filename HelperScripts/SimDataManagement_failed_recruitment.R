
###IMPUTATION DATA MANAGEMENT###
  
#load packages
library(dplyr);library(ggplot2);library(abind)

#read in data 
tdat_frug2_df <- read.csv("~/Documents/GitHub/FaunalDegradationAGBNew/Data/tdat_frug2_imputedB4.csv")#read in the interaction data, either expert or 1/75 scenario
tdat_total_df <- read.csv("~/Documents/GitHub/FaunalDegradationAGBNew/Data/agb-trees-complete.csv") # read in the tree data
plot_subset <- read.csv(file=paste0("~/Documents/GitHub/FaunalDegradationAGBNew/Data/model-data.csv"))# read in the plot data
plot_agb <- read.csv(file=paste0("~/Documents/GitHub/FaunalDegradationAGBNew/Data/plot_agb.csv"))
plot_subset2 <- plot_subset %>% select(DispoCode)
animal_taxa <- read.csv(file=paste0("~/Documents/GitHub/FaunalDegradationAGBNew/Data/animal_taxa.csv"))
animal_taxa <- animal_taxa %>% filter(Species != "Dyaphorophyia_castanea", Species != "Melaniparus_funereus")


#list of the unique plots
plot <- unique(plot_subset2$DispoCode)

#subset data to only include the relevant plots
tdat_total <- tdat_total_df %>% filter(DispoCode %in% plot) 
tdat_frug2 <- tdat_frug2_df %>% filter(DispoCode %in% plot) 


apply_rbinom <- function(col) {
  rbinom(length(col), 1, col)  # Assuming you want to apply rbinom with size 1 and using the column's values as probabilities
}

# Get the indices of the columns from Atimastillas_flavigula to Tragelaphus_eurycerus
start_col <- which(names(tdat_frug2) == "Atimastillas_flavigula")
end_col <- which(names(tdat_frug2) == "Tragelaphus_eurycerus")

# Columns you want to apply the function to
columns_to_apply <- start_col:end_col

# Apply the function to each column
tdat_frug2[, columns_to_apply] <- lapply(tdat_frug2[, columns_to_apply], apply_rbinom)

tdat_frug2<-  tdat_frug2 %>%
  mutate(NDispersed = select(., columns_to_apply) %>% rowSums(na.rm = TRUE))

#Organize data to account for dietary redundancy
tdat_frug2$DispersedSolo <- tdat_frug2$NDispersed
tdat_frug2$DispersedSolo[tdat_frug2$DispersedSolo!=1] <- 0

tdat_frug2$DispersedBinary <- tdat_frug2$NDispersed
tdat_frug2$DispersedBinary[tdat_frug2$DispersedBinary!=0] <- 1

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


tdat_frug2$Endangered <- tdat_frug2$Cercocebus_torquatus + tdat_frug2$Gorilla_gorilla + tdat_frug2$Loxodonta_cyclotis + tdat_frug2$Pan_troglodytes + tdat_frug2$Psittacus_erithacus


tdat_frug2$ele[tdat_frug2$ele>=1] <- 1
tdat_frug2$ape[tdat_frug2$ape>=1] <- 1
tdat_frug2$ceph[tdat_frug2$ceph>=1] <- 1
tdat_frug2$monkey[tdat_frug2$monkey>=1] <- 1
tdat_frug2$carnivore[tdat_frug2$carnivore>=1] <- 1
tdat_frug2$bird[tdat_frug2$bird>=1] <- 1
tdat_frug2$bat[tdat_frug2$bat>=1] <- 1
tdat_frug2$Endangered[tdat_frug2$Endangered>=1] <- 1

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

agb_simulation_nr_failed <- function(plot){
  
  tdat_frug2b <- tdat_frug2 %>%
    filter(DispoCode == plot)%>%
    select(DispoCode, AGB, DispersedBinary)
  
  output <- NULL
  
  for(i in c(25,50,100)){
    
    tdat_frug1 <- tdat_frug2b[sample(which(tdat_frug2b$DispersedBinary==1),replace=F, round(i*0.01*length(which(tdat_frug2b$DispersedBinary==1)))), ]
    
    tdat_frugd <- anti_join(tdat_frug2b, tdat_frug1)
    
    AGB <- sum(tdat_frugd$AGB)
    Plot <- plot
    Orig_AGB <- plot_agb$Plot_AGB[plot_agb$DispoCode==plot]
    AGB_Change <- AGB-Orig_AGB
   
     output <- rbind(
      output,
      data.frame(
        Plot           = plot,
        PercentRemoved = i,
        AGB_Change     = AGB_Change
      )
    )
  }
  
  all_simulation <- as.data.frame(output)
  all_simulation
}

library(future)
library(future.apply)

plots <- plot

plan(multisession, workers = 4)

sim_results <- future_lapply(
  plots,
  agb_simulation_nr_failed
)

names(sim_results) <- plots

sim_results <- dplyr::bind_rows(sim_results, .id = "PlotID")

sim_results2 <- data.frame(sim_results)

sim_results2$AGB_Change <- sim_results2$AGB_Change/1000


sim_df_agb <- sim_results2 %>%
  group_by(PercentRemoved) %>%
  summarise(
    Mean  = mean(AGB_Change, na.rm = TRUE),
    Lower = quantile(AGB_Change, 0.025, na.rm = TRUE),
    Upper = quantile(AGB_Change, 0.975, na.rm = TRUE),
    .groups = "drop"
  )



agb_fig <- ggplot(
  sim_df_agb,
  aes(
    x    = factor(PercentRemoved,
                  levels = c(100, 50, 25),      # order in plot
                  labels = c("100%", "50%", "25%")),
    y    = Mean,
    fill = factor(PercentRemoved)
  )
) +
  geom_bar(
    position = position_dodge(.9),
    stat = "identity"
  ) +
  geom_errorbar(
    position = position_dodge(.9),
    width = .25,
    aes(ymin = Lower, ymax = Upper)
  ) +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "longdash") +
  ylab("Mean AGB Change\n(Mg/ha)") +
  xlab("Scenario") +
  scale_fill_manual(
    name   = "Scenario",
    values = c(
      "100" = "skyblue3",
      "50"  = "lightskyblue1",
      "25"  = "lightskyblue"
    )
  ) +
  theme(legend.position = "none")

agb_fig


ggsave("agb_fig_failed_recruitment.png", 
       plot = agb_fig,
       width = 6, height = 6, dpi = 1000)


