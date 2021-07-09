# 07/09/21 Rachel Bash

# output from Page's statemod, for each reach,

#quantify the total upstream diversions, changes in reservoir storage, reservoir evaporation, and return flows

library(tidyverse)
library(readxl)

path <- "G:/My Drive/American_Whitewater_EEIS/StateMod Models/Extraction Scripts/Output/"
path_nodes <- "D:/projects/aw/data/Reaches/"



#=======================================================#

# YAMPA

 # upload Statemod data

y_tot_return <- read.csv(paste0(path, "Yampa_Total_Return.csv"))
y_tot_supply <- read.csv(paste0(path, "Yampa_Total_Supply.csv"))
y_dstorage <- read.csv(paste0(path, "Yampa_dStorage.csv"))
y_res_evap <- read.csv(paste0(path, "Yampa_Res_Evap.csv"))
y_sim_eom <- read.csv(paste0(path, "Yampa_Sim_EOM.csv"))
y_nat_flow <- read.csv(paste0(path, "Yampa_Natural_Flow.csv"))
y_sim_flow <- read.csv(paste0(path, "Yampa_Simulated_Flow.csv"))

 # tidy

y_tot_return_tidy <- y_tot_return %>%
  pivot_longer(
    cols = contains("RET"),
    names_to = "node",
    names_pattern = "X(.*)_SWSI_B_RET",
    values_to = "af")
y_tot_supply_tidy <- y_tot_supply %>%
  pivot_longer(
    cols = contains("DIV"),
    names_to = "node",
    names_pattern = "X(.*)_SWSI_B_DIV",
    values_to = "af")
y_dstorage_tidy <- y_dstorage %>%
  pivot_longer(
    cols = contains("DSTO"),
    names_to = "node",
    names_pattern = "X(.*)_SWSI_B_EOM_DSTO",
    values_to = "af")
y_res_evap_tidy <- y_res_evap %>%
  pivot_longer(
    cols = contains("EVA"),
    names_to = "node",
    names_pattern = "X(.*)_SWSI_B_EVA",
    values_to = "af")
y_sim_eom_tidy <- y_sim_eom %>%
  pivot_longer(
    cols = contains("EOM"),
    names_to = "node",
    names_pattern = "X(.*)_SWSI_B_EOM",
    values_to = "af")
y_nat_flow_tidy <- y_nat_flow %>%
  select(-contains(".1")) %>%
  pivot_longer(
    cols = contains("NAT"),
    names_to = "node",
    names_pattern = "X(.*)_NAT",
    values_to = "af")
y_sim_flow_tidy <- y_sim_flow %>%
  select(-contains(".1")) %>%
  pivot_longer(
    cols = contains("SIM"),
    names_to = "node",
    names_pattern = "X(.*)_SWSI_B_SIM",
    values_to = "af")

# upload node and location data
y1 <- read_excel(paste0(path_nodes, "nodes upstream of AW reaches.xlsx"), sheet = "Y River Park to Transit")
y2 <- read_excel(paste0(path_nodes, "nodes upstream of AW reaches.xlsx"), sheet = "Transit Center to Pump Station")
y3 <- read_excel(paste0(path_nodes, "nodes upstream of AW reaches.xlsx"), sheet = "Little Yampa Canyon")
y4 <- read_excel(paste0(path_nodes, "nodes upstream of AW reaches.xlsx"), sheet = "Cross Mtn Gorge to Deerlodge")
y5 <- read_excel(paste0(path_nodes, "nodes upstream of AW reaches.xlsx"), sheet = "Deerlodge park to echo park")

crosswalk <- read_excel(paste0(path_nodes, "nodes upstream of AW reaches.xlsx"), sheet = "Crosswalk") %>%
  mutate(Gage_ID = paste0(0,Gage_ID))

#=================================================================#
# Reach y1

nat_tmp <- y_nat_flow_tidy %>%
  filter(node == y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y1"]) %>%
  rename(nat_flow_af = af)
sim_tmp <- y_sim_flow_tidy %>%
  filter(node == y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y1"]) %>%
  rename(sim_flow_af = af)
return_tmp <- y_tot_return_tidy %>%
  filter(node %in% y1$WDID) %>%
  group_by(Date) %>%
  summarise(return_af = sum(af))
supply_tmp <- y_tot_supply_tidy %>%
  filter(node %in% y1$WDID) %>%
  group_by(Date) %>%
  summarise(supply_af = sum(af))
dstorage_tmp <- y_dstorage_tidy %>%
  filter(node %in% y1$WDID) %>%
  group_by(Date) %>%
  summarise(dstorage_af = sum(af))
evap_tmp <- y_res_evap_tidy %>%
  filter(node %in% y1$WDID) %>%
  group_by(Date) %>%
  summarise(evap_af = sum(af))
sim_res_tmp <- y_sim_eom_tidy %>%
  filter(node %in% y1$WDID) %>%
  group_by(Date) %>%
  summarise(sim_res_af = sum(af))

y1_wb_components <- nat_tmp %>% 
  full_join(sim_tmp, by = c("Date", "node")) %>%
  full_join(return_tmp, by = "Date") %>%
  full_join(supply_tmp, by = "Date") %>%
  full_join(dstorage_tmp, by = "Date") %>%
  full_join(evap_tmp, by = "Date") %>%
  full_join(sim_res_tmp, by = "Date") %>%
  mutate(Date = paste0(Date, "-01"),
         Date = as.Date(Date))

#=================================================================#
# Reach y2

nat_tmp <- y_nat_flow_tidy %>%
  filter(node == y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y2"]) %>%
  rename(nat_flow_af = af)
sim_tmp <- y_sim_flow_tidy %>%
  filter(node == y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y2"]) %>%
  rename(sim_flow_af = af)
return_tmp <- y_tot_return_tidy %>%
  filter(node %in% y2$WDID) %>%
  group_by(Date) %>%
  summarise(return_af = sum(af))
supply_tmp <- y_tot_supply_tidy %>%
  filter(node %in% y2$WDID) %>%
  group_by(Date) %>%
  summarise(supply_af = sum(af))
dstorage_tmp <- y_dstorage_tidy %>%
  filter(node %in% y2$WDID) %>%
  group_by(Date) %>%
  summarise(dstorage_af = sum(af))
evap_tmp <- y_res_evap_tidy %>%
  filter(node %in% y2$WDID) %>%
  group_by(Date) %>%
  summarise(evap_af = sum(af))
sim_res_tmp <- y_sim_eom_tidy %>%
  filter(node %in% y2$WDID) %>%
  group_by(Date) %>%
  summarise(sim_res_af = sum(af))

y2_wb_components <- nat_tmp %>% 
  full_join(sim_tmp, by = c("Date", "node")) %>%
  full_join(return_tmp, by = "Date") %>%
  full_join(supply_tmp, by = "Date") %>%
  full_join(dstorage_tmp, by = "Date") %>%
  full_join(evap_tmp, by = "Date") %>%
  full_join(sim_res_tmp, by = "Date") %>%
  mutate(Date = paste0(Date, "-01"),
         Date = as.Date(Date))

#=================================================================#
# Reach y3

nat_tmp <- y_nat_flow_tidy %>%
  filter(node == y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y3"]) %>%
  rename(nat_flow_af = af)
sim_tmp <- y_sim_flow_tidy %>%
  filter(node == y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y3"]) %>%
  rename(sim_flow_af = af)
return_tmp <- y_tot_return_tidy %>%
  filter(node %in% y3$WDID) %>%
  group_by(Date) %>%
  summarise(return_af = sum(af))
supply_tmp <- y_tot_supply_tidy %>%
  filter(node %in% y3$WDID) %>%
  group_by(Date) %>%
  summarise(supply_af = sum(af))
dstorage_tmp <- y_dstorage_tidy %>%
  filter(node %in% y3$WDID) %>%
  group_by(Date) %>%
  summarise(dstorage_af = sum(af))
evap_tmp <- y_res_evap_tidy %>%
  filter(node %in% y3$WDID) %>%
  group_by(Date) %>%
  summarise(evap_af = sum(af))
sim_res_tmp <- y_sim_eom_tidy %>%
  filter(node %in% y3$WDID) %>%
  group_by(Date) %>%
  summarise(sim_res_af = sum(af))

y3_wb_components <- nat_tmp %>% 
  full_join(sim_tmp, by = c("Date", "node")) %>%
  full_join(return_tmp, by = "Date") %>%
  full_join(supply_tmp, by = "Date") %>%
  full_join(dstorage_tmp, by = "Date") %>%
  full_join(evap_tmp, by = "Date") %>%
  full_join(sim_res_tmp, by = "Date") %>%
  mutate(Date = paste0(Date, "-01"),
         Date = as.Date(Date))

#=================================================================#
# Reach y4

nat_tmp <- y_nat_flow_tidy %>%
  filter(node == y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y4"]) %>%
  rename(nat_flow_af = af)
sim_tmp <- y_sim_flow_tidy %>%
  filter(node == y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y4"]) %>%
  rename(sim_flow_af = af)
return_tmp <- y_tot_return_tidy %>%
  filter(node %in% y4$WDID) %>%
  group_by(Date) %>%
  summarise(return_af = sum(af))
supply_tmp <- y_tot_supply_tidy %>%
  filter(node %in% y4$WDID) %>%
  group_by(Date) %>%
  summarise(supply_af = sum(af))
dstorage_tmp <- y_dstorage_tidy %>%
  filter(node %in% y4$WDID) %>%
  group_by(Date) %>%
  summarise(dstorage_af = sum(af))
evap_tmp <- y_res_evap_tidy %>%
  filter(node %in% y4$WDID) %>%
  group_by(Date) %>%
  summarise(evap_af = sum(af))
sim_res_tmp <- y_sim_eom_tidy %>%
  filter(node %in% y4$WDID) %>%
  group_by(Date) %>%
  summarise(sim_res_af = sum(af))

y4_wb_components <- nat_tmp %>% 
  full_join(sim_tmp, by = c("Date", "node")) %>%
  full_join(return_tmp, by = "Date") %>%
  full_join(supply_tmp, by = "Date") %>%
  full_join(dstorage_tmp, by = "Date") %>%
  full_join(evap_tmp, by = "Date") %>%
  full_join(sim_res_tmp, by = "Date") %>%
  mutate(Date = paste0(Date, "-01"),
         Date = as.Date(Date))

#=================================================================#
# Reach y5

nat_tmp <- y_nat_flow_tidy %>%
  filter(node == y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y5"]) %>%
  rename(nat_flow_af = af)
sim_tmp <- y_sim_flow_tidy %>%
  filter(node == y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y5"]) %>%
  rename(sim_flow_af = af)
return_tmp <- y_tot_return_tidy %>%
  filter(node %in% y5$WDID) %>%
  group_by(Date) %>%
  summarise(return_af = sum(af))
supply_tmp <- y_tot_supply_tidy %>%
  filter(node %in% y5$WDID) %>%
  group_by(Date) %>%
  summarise(supply_af = sum(af))
dstorage_tmp <- y_dstorage_tidy %>%
  filter(node %in% y5$WDID) %>%
  group_by(Date) %>%
  summarise(dstorage_af = sum(af))
evap_tmp <- y_res_evap_tidy %>%
  filter(node %in% y5$WDID) %>%
  group_by(Date) %>%
  summarise(evap_af = sum(af))
sim_res_tmp <- y_sim_eom_tidy %>%
  filter(node %in% y5$WDID) %>%
  group_by(Date) %>%
  summarise(sim_res_af = sum(af))

y5_wb_components <- nat_tmp %>% 
  full_join(sim_tmp, by = c("Date", "node")) %>%
  full_join(return_tmp, by = "Date") %>%
  full_join(supply_tmp, by = "Date") %>%
  full_join(dstorage_tmp, by = "Date") %>%
  full_join(evap_tmp, by = "Date") %>%
  full_join(sim_res_tmp, by = "Date") %>%
  mutate(Date = paste0(Date, "-01"),
         Date = as.Date(Date))


# save RDS files
saveRDS(y1_wb_components, "./output/y1_wb_components.rds")
saveRDS(y2_wb_components, "./output/y2_wb_components.rds")
saveRDS(y3_wb_components, "./output/y3_wb_components.rds")
saveRDS(y4_wb_components, "./output/y4_wb_components.rds")
saveRDS(y5_wb_components, "./output/y5_wb_components.rds")


#notes


#dstorage and res_evap are for the 12 reservoirs in the model
  
  #y1 = yampa river park to transit center (most upstream)
  #y2 = transit center to pump station
  #y3 = little yampa canyon
  #y4 = cross mountain gorge
  #y5 = deerlodge park to echo canyon

#Scrap

#y_crosswalk <- crosswalk %>%
#  filter(River == "Yampa")
#
#for(y in seq_along(reach_list))
#  
#  for (y in y_crosswalk$R_ID)
#    
#    print(y_crosswalk$R_ID[y])
#reach <- y_crosswalk$R_ID[y]
#nodes <- reach
#
#reach_list <- list(y1, y2, y3, y4, y5)

