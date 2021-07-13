# 07/09/21 Rachel Bash

# output from Page's statemod, for each reach,

#quantify the total upstream diversions, changes in reservoir storage, reservoir evaporation, and return flows

library(tidyverse)
library(readxl)
library(lubridate)

source("misc/find_google_drive.R")

path <- paste0(drive_dir, "StateMod Models/Extraction Scripts/Output/")
path_nodes <- "./data/nodes upstream of AW reaches.xlsx"





# YAMPA -------------------------------------------------------------------

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
y1 <- read_excel(path_nodes, sheet = "Y River Park to Transit")
y2 <- read_excel(path_nodes, sheet = "Transit Center to Pump Station")
y3 <- read_excel(path_nodes, sheet = "Little Yampa Canyon")
y4 <- read_excel(path_nodes, sheet = "Cross Mtn Gorge to Deerlodge")
y5 <- read_excel(path_nodes, sheet = "Deerlodge park to echo park")

crosswalk <- read_excel(path_nodes, sheet = "Crosswalk") %>%
  mutate(Gage_ID = paste0(0,Gage_ID))

# Reach y1 ================================

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



# Adam meeting working ======================
y1_tot_supply_tidy <- y_tot_supply_tidy %>%
  filter(node %in% y1$WDID) %>%
  mutate(Date = paste0(Date, "-01"), Date = as.Date(Date), year = year(Date)) %>%
  group_by(node, year) %>%
  summarise(tot_supply = sum(af))
y1_tot_supply <- y1_tot_supply_tidy %>%
  group_by(node) %>%
  summarise(avg_tot_supply = mean(tot_supply))

ggplot(y1_tot_supply, aes(x = avg_tot_supply)) +
  geom_histogram()



y1_wb <- y1_wb_components %>%
  filter(Date > "1974-01-01") %>%
  mutate(nat_sim = nat_flow_af - sim_flow_af) %>%
  mutate(balance = nat_flow_af - supply_af - dstorage_af - evap_af + return_af)

y1_wb_annual <- y1_wb %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarise(nat_flow = sum(nat_flow_af), sim_flow = sum(sim_flow_af), 
            return = sum(return_af), supply = sum(supply_af), 
            dstorage = sum(dstorage_af), evap = sum(evap_af), 
            sim_res = sum(sim_res_af), nat_sim = sum(nat_sim), 
            balance_annual = sum(balance))

# Reach y2 ================================

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

# Reach y3 ================================

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

# Reach y4 ================================

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

# Reach y5 ================================

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

# Next steps ==========================
#impact: add supply and returns columns together then sum over the year then average across years



# save RDS files =============================
saveRDS(y1_wb_components, "./output/y1_wb_components.rds")
saveRDS(y2_wb_components, "./output/y2_wb_components.rds")
saveRDS(y3_wb_components, "./output/y3_wb_components.rds")
saveRDS(y4_wb_components, "./output/y4_wb_components.rds")
saveRDS(y5_wb_components, "./output/y5_wb_components.rds")





# ROARING FORK -------------------------------------------------------------------

# upload Statemod data

c_tot_return <- read.csv(paste0(path, "Colorado_Total_Return.csv"))
c_tot_supply <- read.csv(paste0(path, "Colorado_Total_Supply.csv"))
c_dstorage <- read.csv(paste0(path, "Colorado_dStorage.csv"))
c_res_evap <- read.csv(paste0(path, "Colorado_Res_Evap.csv"))
c_sim_eom <- read.csv(paste0(path, "Colorado_Sim_EOM.csv"))
c_nat_flow <- read.csv(paste0(path, "Colorado_Natural_Flow.csv"))
c_sim_flow <- read.csv(paste0(path, "Colorado_Simulated_Flow.csv"))

# tidy

c_tot_return_tidy <- c_tot_return %>%
  pivot_longer(
    cols = contains("RET"),
    names_to = "node",
    names_pattern = "X(.*)_SWSI_B_RET",
    values_to = "af")
c_tot_supplc_tidy <- c_tot_supply %>%
  pivot_longer(
    cols = contains("DIV"),
    names_to = "node",
    names_pattern = "X(.*)_SWSI_B_DIV",
    values_to = "af")
c_dstorage_tidy <- c_dstorage %>%
  pivot_longer(
    cols = contains("DSTO"),
    names_to = "node",
    names_pattern = "X(.*)_SWSI_B_EOM_DSTO",
    values_to = "af")
c_res_evap_tidy <- c_res_evap %>%
  pivot_longer(
    cols = contains("EVA"),
    names_to = "node",
    names_pattern = "X(.*)_SWSI_B_EVA",
    values_to = "af")
c_sim_eom_tidy <- c_sim_eom %>%
  pivot_longer(
    cols = contains("EOM"),
    names_to = "node",
    names_pattern = "X(.*)_SWSI_B_EOM",
    values_to = "af")
c_nat_flow_tidy <- c_nat_flow %>%
  select(-contains(".1")) %>%
  pivot_longer(
    cols = contains("NAT"),
    names_to = "node",
    names_pattern = "X(.*)_NAT",
    values_to = "af")
c_sim_flow_tidy <- c_sim_flow %>%
  select(-contains(".1")) %>%
  pivot_longer(
    cols = contains("SIM"),
    names_to = "node",
    names_pattern = "X(.*)_SWSI_B_SIM",
    values_to = "af")

# upload node and location data
rf1 <- read_excel(path_nodes, sheet = "Weller Lake")
rf2 <- read_excel(path_nodes, sheet = "Slaughterhouse")
rf3 <- read_excel(path_nodes, sheet = "Upper Woody Creek Bridge")
rf4 <- read_excel(path_nodes, sheet = "Lower Woody Creek Bridge")
rf5 <- read_excel(path_nodes, sheet = "Basalt to Carbondale")
rf6 <- read_excel(path_nodes, sheet = "Black Bridge to Veltus")
crosswalk <- read_excel(path_nodes, sheet = "Crosswalk") %>%
  mutate(Gage_ID = paste0(0,Gage_ID))
c_crosswalk <- crosswalk %>%
    filter(River == "Roaring Fork")
c_crosswalk <- crosswalk %>%
  filter(River == "Crystal")


# Reach rf1 ================================

nat_tmp <- c_nat_flow_tidy %>%
  filter(node == c_crosswalk$Gage_ID[c_crosswalk$R_ID == "rf1"]) %>% 
  rename(nat_flow_af = af)
sim_tmp <- c_sim_flow_tidy %>%
  filter(node == c_crosswalk$Gage_ID[c_crosswalk$R_ID == "rf1"]) %>%
  rename(sim_flow_af = af)
return_tmp <- c_tot_return_tidy %>%
  filter(node %in% rf1$WDID) %>%
  group_by(Date) %>%
  summarise(return_af = sum(af))
supplc_tmp <- c_tot_supplc_tidy %>%
  filter(node %in% rf1$WDID) %>%
  group_by(Date) %>%
  summarise(supplc_af = sum(af))
dstorage_tmp <- c_dstorage_tidy %>%
  filter(node %in% rf1$WDID) %>%
  group_by(Date) %>%
  summarise(dstorage_af = sum(af))
evap_tmp <- c_res_evap_tidy %>%
  filter(node %in% rf1$WDID) %>%
  group_by(Date) %>%
  summarise(evap_af = sum(af))
sim_res_tmp <- c_sim_eom_tidy %>%
  filter(node %in% rf1$WDID) %>%
  group_by(Date) %>%
  summarise(sim_res_af = sum(af))

rf1_wb_components <- nat_tmp %>% 
  full_join(sim_tmp, by = c("Date", "node")) %>%
  full_join(return_tmp, by = "Date") %>%
  full_join(supplc_tmp, by = "Date") %>%
  full_join(dstorage_tmp, by = "Date") %>%
  full_join(evap_tmp, by = "Date") %>%
  full_join(sim_res_tmp, by = "Date") %>%
  mutate(Date = paste0(Date, "-01"),
         Date = as.Date(Date))

# Reach rf2 ================================

nat_tmp <- c_nat_flow_tidy %>%
  filter(node == c_crosswalk$Gage_ID[c_crosswalk$R_ID == "rf2"]) %>%
  rename(nat_flow_af = af)
sim_tmp <- c_sim_flow_tidy %>%
  filter(node == c_crosswalk$Gage_ID[c_crosswalk$R_ID == "rf2"]) %>%
  rename(sim_flow_af = af)
return_tmp <- c_tot_return_tidy %>%
  filter(node %in% rf2$WDID) %>%
  group_by(Date) %>%
  summarise(return_af = sum(af))
supplc_tmp <- c_tot_supplc_tidy %>%
  filter(node %in% rf2$WDID) %>%
  group_by(Date) %>%
  summarise(supplc_af = sum(af))
dstorage_tmp <- c_dstorage_tidy %>%
  filter(node %in% rf2$WDID) %>%
  group_by(Date) %>%
  summarise(dstorage_af = sum(af))
evap_tmp <- c_res_evap_tidy %>%
  filter(node %in% rf2$WDID) %>%
  group_by(Date) %>%
  summarise(evap_af = sum(af))
sim_res_tmp <- c_sim_eom_tidy %>%
  filter(node %in% rf2$WDID) %>%
  group_by(Date) %>%
  summarise(sim_res_af = sum(af))

rf2_wb_components <- nat_tmp %>% 
  full_join(sim_tmp, by = c("Date", "node")) %>%
  full_join(return_tmp, by = "Date") %>%
  full_join(supplc_tmp, by = "Date") %>%
  full_join(dstorage_tmp, by = "Date") %>%
  full_join(evap_tmp, by = "Date") %>%
  full_join(sim_res_tmp, by = "Date") %>%
  mutate(Date = paste0(Date, "-01"),
         Date = as.Date(Date))

# Reach rf3 ================================

nat_tmp <- c_nat_flow_tidy %>%
  filter(node == c_crosswalk$Gage_ID[c_crosswalk$R_ID == "rf3"]) %>%
  rename(nat_flow_af = af)
sim_tmp <- c_sim_flow_tidy %>%
  filter(node == c_crosswalk$Gage_ID[c_crosswalk$R_ID == "rf3"]) %>%
  rename(sim_flow_af = af)
return_tmp <- c_tot_return_tidy %>%
  filter(node %in% rf3$WDID) %>%
  group_by(Date) %>%
  summarise(return_af = sum(af))
supplc_tmp <- c_tot_supplc_tidy %>%
  filter(node %in% rf3$WDID) %>%
  group_by(Date) %>%
  summarise(supplc_af = sum(af))
dstorage_tmp <- c_dstorage_tidy %>%
  filter(node %in% rf3$WDID) %>%
  group_by(Date) %>%
  summarise(dstorage_af = sum(af))
evap_tmp <- c_res_evap_tidy %>%
  filter(node %in% rf3$WDID) %>%
  group_by(Date) %>%
  summarise(evap_af = sum(af))
sim_res_tmp <- c_sim_eom_tidy %>%
  filter(node %in% rf3$WDID) %>%
  group_by(Date) %>%
  summarise(sim_res_af = sum(af))

rf3_wb_components <- nat_tmp %>% 
  full_join(sim_tmp, by = c("Date", "node")) %>%
  full_join(return_tmp, by = "Date") %>%
  full_join(supplc_tmp, by = "Date") %>%
  full_join(dstorage_tmp, by = "Date") %>%
  full_join(evap_tmp, by = "Date") %>%
  full_join(sim_res_tmp, by = "Date") %>%
  mutate(Date = paste0(Date, "-01"),
         Date = as.Date(Date))

# Reach rf4 ================================

nat_tmp <- c_nat_flow_tidy %>%
  filter(node == c_crosswalk$Gage_ID[c_crosswalk$R_ID == "rf4"]) %>%
  rename(nat_flow_af = af)
sim_tmp <- c_sim_flow_tidy %>%
  filter(node == c_crosswalk$Gage_ID[c_crosswalk$R_ID == "rf4"]) %>%
  rename(sim_flow_af = af)
return_tmp <- c_tot_return_tidy %>%
  filter(node %in% rf4$WDID) %>%
  group_by(Date) %>%
  summarise(return_af = sum(af))
supplc_tmp <- c_tot_supplc_tidy %>%
  filter(node %in% rf4$WDID) %>%
  group_by(Date) %>%
  summarise(supplc_af = sum(af))
dstorage_tmp <- c_dstorage_tidy %>%
  filter(node %in% rf4$WDID) %>%
  group_by(Date) %>%
  summarise(dstorage_af = sum(af))
evap_tmp <- c_res_evap_tidy %>%
  filter(node %in% rf4$WDID) %>%
  group_by(Date) %>%
  summarise(evap_af = sum(af))
sim_res_tmp <- c_sim_eom_tidy %>%
  filter(node %in% rf4$WDID) %>%
  group_by(Date) %>%
  summarise(sim_res_af = sum(af))

rf4_wb_components <- nat_tmp %>% 
  full_join(sim_tmp, by = c("Date", "node")) %>%
  full_join(return_tmp, by = "Date") %>%
  full_join(supplc_tmp, by = "Date") %>%
  full_join(dstorage_tmp, by = "Date") %>%
  full_join(evap_tmp, by = "Date") %>%
  full_join(sim_res_tmp, by = "Date") %>%
  mutate(Date = paste0(Date, "-01"),
         Date = as.Date(Date))

# Reach rf5 ================================

nat_tmp <- c_nat_flow_tidy %>%
  filter(node == c_crosswalk$Gage_ID[c_crosswalk$R_ID == "rf5"]) %>%
  rename(nat_flow_af = af)
sim_tmp <- c_sim_flow_tidy %>%
  filter(node == c_crosswalk$Gage_ID[c_crosswalk$R_ID == "rf5"]) %>%
  rename(sim_flow_af = af)
return_tmp <- c_tot_return_tidy %>%
  filter(node %in% rf5$WDID) %>%
  group_by(Date) %>%
  summarise(return_af = sum(af))
supplc_tmp <- c_tot_supplc_tidy %>%
  filter(node %in% rf5$WDID) %>%
  group_by(Date) %>%
  summarise(supplc_af = sum(af))
dstorage_tmp <- c_dstorage_tidy %>%
  filter(node %in% rf5$WDID) %>%
  group_by(Date) %>%
  summarise(dstorage_af = sum(af))
evap_tmp <- c_res_evap_tidy %>%
  filter(node %in% rf5$WDID) %>%
  group_by(Date) %>%
  summarise(evap_af = sum(af))
sim_res_tmp <- c_sim_eom_tidy %>%
  filter(node %in% rf5$WDID) %>%
  group_by(Date) %>%
  summarise(sim_res_af = sum(af))

rf5_wb_components <- nat_tmp %>% 
  full_join(sim_tmp, by = c("Date", "node")) %>%
  full_join(return_tmp, by = "Date") %>%
  full_join(supplc_tmp, by = "Date") %>%
  full_join(dstorage_tmp, by = "Date") %>%
  full_join(evap_tmp, by = "Date") %>%
  full_join(sim_res_tmp, by = "Date") %>%
  mutate(Date = paste0(Date, "-01"),
         Date = as.Date(Date))

# Reach rf6 ================================

nat_tmp <- c_nat_flow_tidy %>%
  filter(node == c_crosswalk$Gage_ID[c_crosswalk$R_ID == "rf6"]) %>%
  rename(nat_flow_af = af)
sim_tmp <- c_sim_flow_tidy %>%
  filter(node == c_crosswalk$Gage_ID[c_crosswalk$R_ID == "rf6"]) %>%
  rename(sim_flow_af = af)
return_tmp <- c_tot_return_tidy %>%
  filter(node %in% rf6$WDID) %>%
  group_by(Date) %>%
  summarise(return_af = sum(af))
supplc_tmp <- c_tot_supplc_tidy %>%
  filter(node %in% rf6$WDID) %>%
  group_by(Date) %>%
  summarise(supplc_af = sum(af))
dstorage_tmp <- c_dstorage_tidy %>%
  filter(node %in% rf6$WDID) %>%
  group_by(Date) %>%
  summarise(dstorage_af = sum(af))
evap_tmp <- c_res_evap_tidy %>%
  filter(node %in% rf6$WDID) %>%
  group_by(Date) %>%
  summarise(evap_af = sum(af))
sim_res_tmp <- c_sim_eom_tidy %>%
  filter(node %in% rf6$WDID) %>%
  group_by(Date) %>%
  summarise(sim_res_af = sum(af))

rf6_wb_components <- nat_tmp %>% 
  full_join(sim_tmp, by = c("Date", "node")) %>%
  full_join(return_tmp, by = "Date") %>%
  full_join(supplc_tmp, by = "Date") %>%
  full_join(dstorage_tmp, by = "Date") %>%
  full_join(evap_tmp, by = "Date") %>%
  full_join(sim_res_tmp, by = "Date") %>%
  mutate(Date = paste0(Date, "-01"),
         Date = as.Date(Date))


# save RDS files =============================
saveRDS(rf1_wb_components, "./output/rf1_wb_components.rds")
saveRDS(rf2_wb_components, "./output/rf2_wb_components.rds")
saveRDS(rf3_wb_components, "./output/rf3_wb_components.rds")
saveRDS(rf4_wb_components, "./output/rf4_wb_components.rds")
saveRDS(rf5_wb_components, "./output/rf5_wb_components.rds")
saveRDS(rf6_wb_components, "./output/rf6_wb_components.rds")

# Notes -------------------------------------------------------------------


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

