# 07/09/21 Rachel Bash

# output from Page's statemod, for each reach,

#quantify the total upstream diversions, changes in reservoir storage, reservoir evaporation, and return flows

# Set Up --------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(nationalparkcolors)

options(scipen = 100)
source("misc/find_google_drive.R")

pal1 <- park_palette("Saguaro", 6)
path <- paste0(drive_dir, "StateMod Models/Extraction Scripts/Output/")
path_nodes <- "./data/nodes upstream of AW reaches.xlsx"

crosswalk <- read_excel(path_nodes, sheet = "Crosswalk") %>%
  mutate(Gage_ID = paste0(0,Gage_ID))





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


y_crosswalk <- crosswalk %>%
  filter(River == "Yampa")

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

ggplot(y1_wb_annual) +
  geom_point(aes(x = balance_annual, y = sim_flow))

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
supply_return <- y_tot_supply_tidy %>% 
  rename(supply = af) %>% 
  full_join(y_tot_return_tidy) %>%
  rename(return = af) %>%
  mutate(impact = supply - return)

storage_evap <- y_dstorage_tidy %>%
  rename(dstorage = af) %>%
  full_join(y_res_evap_tidy, by = c("Date", "node")) %>%
  rename(evap = af) %>%
  mutate(impact = dstorage + evap)

impact <- full_join(supply_return, storage_evap, by = c("Date", "node", "impact")) %>%
  mutate(Date = paste0(Date, "-01"), Date = as.Date(Date), year = year(Date), month = month(Date)) %>%
  filter(month > 4 & month < 10) %>%
  group_by(node, year) %>%
  summarise(tot_impact = sum(impact)) %>%
  group_by(node) %>%
  summarise(avg_tot_impact = mean(tot_impact))

ggplot() +
  geom_histogram(data = impact, aes(x = avg_tot_impact))


y_mapping_impact <- full_join(y5, impact, by = c("ID" = "node")) %>%
  mutate(abs_impact = abs(avg_tot_impact), #make absolute column
         sign = ifelse(avg_tot_impact > 0, 1, ifelse(avg_tot_impact == 0, 0, -1)))
write.csv(mapping_impact, "./output/mapping_impact_all.csv", row.names = FALSE)

# problems with importing csv into arcgis, so attempt to make it shp here
# upload existing shp used to see projection info

library(sf)
yampa_nodes <- st_read("../data/Yampa Structures/At Deerlodge.shp")
our_crs <- st_crs(yampa_nodes)
y_mapping_impact_sf <- st_as_sf(y_mapping_impact, coords = c("UTMX", "UTMY"), crs = our_crs)
st_write(mapping_impact_sf, "./output/mapping_impact_summer.shp")



# get top 5 impacters
y5_top5 <- mapping_impact_sf %>%
  arrange(desc(avg_tot_impact)) %>%
  slice_head(n=5) %>%
  mutate(reach = "y1")
y1_top5 <- mapping_impact_sf %>% filter(WDID %in% y1$WDID) %>%
  arrange(desc(avg_tot_impact)) %>%
  slice_head(n=5) %>%
  mutate(reach = "y2")
y2_top5 <- mapping_impact_sf %>% filter(WDID %in% y2$WDID) %>%
  arrange(desc(avg_tot_impact)) %>%
  slice_head(n=5) %>%
  mutate(reach = "y3")
y3_top5 <- mapping_impact_sf %>% filter(WDID %in% y3$WDID) %>%
  arrange(desc(avg_tot_impact)) %>%
  slice_head(n=5) %>%
  mutate(reach = "y4")
y4_top5 <- mapping_impact_sf %>% filter(WDID %in% y4$WDID) %>%
  arrange(desc(avg_tot_impact)) %>%
  slice_head(n=5) %>%
  mutate(reach = "y5")
all_top5 <- rbind(y1_top5, y2_top5, y3_top5, y4_top5, y5_top5)
write.csv(all_top5, "./output/all_top5.csv", row.names = FALSE)
st_write(all_top5, "./output/all_top5.shp")

#save as individual st's
st_write(y1_top5, "./output/y1_top5.shp")
st_write(y2_top5, "./output/y2_top5.shp")
st_write(y3_top5, "./output/y3_top5.shp")
st_write(y4_top5, "./output/y4_top5.shp")
st_write(y5_top5, "./output/y5_top5.shp")

# save RDS files =============================
saveRDS(y1_wb_components, "./output/y1_wb_components.rds")
saveRDS(y2_wb_components, "./output/y2_wb_components.rds")
saveRDS(y3_wb_components, "./output/y3_wb_components.rds")
saveRDS(y4_wb_components, "./output/y4_wb_components.rds")
saveRDS(y5_wb_components, "./output/y5_wb_components.rds")


# water balance components

y5_wb <- y5_wb_components %>%
  filter(Date > "1974-01-01") %>%
  mutate(nat_sim = nat_flow_af - sim_flow_af) %>%
  mutate(balance = nat_flow_af - supply_af - dstorage_af - evap_af + return_af)

y5_wb_annual <- y5_wb %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarise(nat_flow = sum(nat_flow_af), sim_flow = sum(sim_flow_af), 
            return = sum(return_af), supply = sum(supply_af), 
            dstorage = sum(dstorage_af), evap = sum(evap_af), 
            sim_res = sum(sim_res_af), nat_sim = sum(nat_sim), 
            balance_annual = sum(balance))

ggplot(y5_wb_annual) +
  geom_point(aes(x = balance_annual, y = sim_flow))

y5_wb_month <- y5_wb %>%
  mutate(month = month(Date)) %>%
  group_by(month) %>%
  summarise(nat_flow = sum(nat_flow_af), sim_flow = sum(sim_flow_af, na.rm = TRUE), 
            return = sum(return_af, na.rm = TRUE), supply = sum(supply_af, na.rm = TRUE), 
            dstorage = sum(dstorage_af, na.rm = TRUE), evap = sum(evap_af, na.rm = TRUE), 
            sim_res = sum(sim_res_af, na.rm = TRUE), nat_sim = sum(nat_sim, na.rm = TRUE), 
            balance_annual = sum(balance, na.rm = TRUE)) %>%
  pivot_longer(-month, names_to = "flow_type", values_to = "af")

ggplot(y5_wb_month) +
  geom_line(aes(x = month, y = af, color = flow_type)) +
  theme_bw()

# Plotting ----------------------------------------------------------------------

# Plot y1 =============================
# reconstruct nat flow for each gage in reach; only use the columns that have data in them
y1_wb <- y1_wb_components %>%
  filter(Date > "1974-10-01") %>%
  mutate(recon_nat_flow = sim_flow_af + supply_af - return_af + dstorage_af + evap_af,
         diff = nat_flow_af - recon_nat_flow)

# plot all wb components
p1<- y1_wb %>% 
  select(-sim_res_af) %>%
  filter(Date > "2000-01-01") %>%
  pivot_longer(cols = contains("af"), names_to = "flow_type", values_to = "af") %>%
  ggplot(.) +
  geom_line(aes(x = Date, y = af, color = flow_type), size = 0.9) +
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Year", y = "Total flow (cfs)", color = "Flow Type", 
       title = paste0("Reach y1 - ", y_crosswalk$Reach_Name[y_crosswalk$R_ID == "y1"], " USGS ",
                      y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y1"]))
p1

# plot difference between natural and reconstructed  
p2<- y1_wb %>%
  filter(Date > "2000-01-01") %>%
  ggplot(.) + 
  geom_line(aes(x = Date, y = diff)) + 
  theme_classic()  +
  labs(x = "Year", y = "Natural flow - Reconstructed Natural flow (cfs)", 
       title = paste0("Reach y1 - ", y_crosswalk$Reach_Name[y_crosswalk$R_ID == "y1"], " USGS ",
                      y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y1"]))
p2

# plot of nat flow and reconstructed natural flow overlapping
p3<- y1_wb %>% 
  select(Date, nat_flow_af, recon_nat_flow) %>%
  filter(Date > "2000-01-01") %>%
  pivot_longer(-Date, names_to = "flow_type", values_to = "af") %>%
  ggplot(.) + 
  geom_line(aes(x = Date, y = af, color = flow_type), size = 0.9) + 
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Year", y = "Total flow (cfs)", color = "Flow Type", 
       title = paste0("Reach y1 - ", y_crosswalk$Reach_Name[y_crosswalk$R_ID == "y1"], " USGS ",
                      y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y1"]))
p3

# plot of average monthly natural and reconstructed natural 
p4<- y1_wb %>% select(Date, nat_flow_af, recon_nat_flow) %>%
  pivot_longer(-Date, names_to = "flow_type", values_to = "af") %>%
  mutate(month = month(Date)) %>%
  group_by(month, flow_type) %>%
  summarise(avg_monthly_flow = mean(af)) %>%
  ggplot(.) +
  geom_line(aes(x = month, y = avg_monthly_flow, color = flow_type), size = 0.9)+ 
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Month", y = "Average Monthly Flow (cfs)", color = "Flow Type", 
       title = paste0("Reach y1 - ", y_crosswalk$Reach_Name[y_crosswalk$R_ID == "y1"], " USGS ",
                      y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y1"]))
p4

ggsave(p3, filename = "./output/plots/y1_nat_reconstructed_flow.png", bg = "transparent")
ggsave(p4, filename = "./output/plots/y1_monthly_avg.png", bg = "transparent")



# Plot y2 =============================
# reconstruct nat flow for each gage in reach; only use the columns that have data in them
y2_wb <- y2_wb_components %>%
  filter(Date > "1974-10-01") %>%
  mutate(recon_nat_flow = sim_flow_af + supply_af - return_af + dstorage_af + evap_af,
         diff = nat_flow_af - recon_nat_flow)

# plot all wb components
p1<- y2_wb %>% 
  select(-sim_res_af) %>%
  filter(Date > "2000-01-01") %>%
  pivot_longer(cols = contains("af"), names_to = "flow_type", values_to = "af") %>%
  ggplot(.) +
  geom_line(aes(x = Date, y = af, color = flow_type), size = 0.9) +
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Year", y = "Total flow (cfs)", color = "Flow Type", 
       title = paste0("Reach y2 - ", y_crosswalk$Reach_Name[y_crosswalk$R_ID == "y2"], " USGS ",
                      y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y2"]))
p1

# plot difference between natural and reconstructed  
p2<- y2_wb %>%
  filter(Date > "2000-01-01") %>%
  ggplot(.) + 
  geom_line(aes(x = Date, y = diff)) + 
  theme_classic()  +
  labs(x = "Year", y = "Natural flow - Reconstructed Natural flow (cfs)", 
       title = paste0("Reach y2 - ", y_crosswalk$Reach_Name[y_crosswalk$R_ID == "y2"], " USGS ",
                      y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y2"]))
p2

# plot of nat flow and reconstructed natural flow overlapping
p3<- y2_wb %>% 
  select(Date, nat_flow_af, recon_nat_flow) %>%
  filter(Date > "2000-01-01") %>%
  pivot_longer(-Date, names_to = "flow_type", values_to = "af") %>%
  ggplot(.) + 
  geom_line(aes(x = Date, y = af, color = flow_type), size = 0.9) + 
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Year", y = "Total flow (cfs)", color = "Flow Type", 
       title = paste0("Reach y2 - ", y_crosswalk$Reach_Name[y_crosswalk$R_ID == "y2"], " USGS ",
                      y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y2"]))
p3

# plot of average monthly natural and reconstructed natural 
p4<- y2_wb %>% select(Date, nat_flow_af, recon_nat_flow) %>%
  pivot_longer(-Date, names_to = "flow_type", values_to = "af") %>%
  mutate(month = month(Date)) %>%
  group_by(month, flow_type) %>%
  summarise(avg_monthly_flow = mean(af)) %>%
  ggplot(.) +
  geom_line(aes(x = month, y = avg_monthly_flow, color = flow_type), size = 0.9)+ 
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Month", y = "Average Monthly Flow (cfs)", color = "Flow Type", 
       title = paste0("Reach y2 - ", y_crosswalk$Reach_Name[y_crosswalk$R_ID == "y2"], " USGS ",
                      y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y2"]))
p4

ggsave(p3, filename = "./output/plots/y2_nat_reconstructed_flow.png", bg = "transparent")
ggsave(p4, filename = "./output/plots/y2_monthly_avg.png", bg = "transparent")


# Plot y3 =============================
# reconstruct nat flow for each gage in reach; only use the columns that have data in them
y3_wb <- y3_wb_components %>%
  filter(Date > "1974-10-01") %>%
  mutate(recon_nat_flow = sim_flow_af + supply_af - return_af + dstorage_af + evap_af,
         diff = nat_flow_af - recon_nat_flow)

# plot all wb components
p1<- y3_wb %>% 
  select(-sim_res_af) %>%
  filter(Date > "2000-01-01") %>%
  pivot_longer(cols = contains("af"), names_to = "flow_type", values_to = "af") %>%
  ggplot(.) +
  geom_line(aes(x = Date, y = af, color = flow_type), size = 0.9) +
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Year", y = "Total flow (cfs)", color = "Flow Type", 
       title = paste0("Reach y3 - ", y_crosswalk$Reach_Name[y_crosswalk$R_ID == "y3"], " USGS ",
                      y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y3"]))
p1

# plot difference between natural and reconstructed  
p2<- y3_wb %>%
  filter(Date > "2000-01-01") %>%
  ggplot(.) + 
  geom_line(aes(x = Date, y = diff)) + 
  theme_classic()  +
  labs(x = "Year", y = "Natural flow - Reconstructed Natural flow (cfs)", 
       title = paste0("Reach y3 - ", y_crosswalk$Reach_Name[y_crosswalk$R_ID == "y3"], " USGS ",
                      y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y3"]))
p2

# plot of nat flow and reconstructed natural flow overlapping
p3<- y3_wb %>% 
  select(Date, nat_flow_af, recon_nat_flow) %>%
  filter(Date > "2000-01-01") %>%
  pivot_longer(-Date, names_to = "flow_type", values_to = "af") %>%
  ggplot(.) + 
  geom_line(aes(x = Date, y = af, color = flow_type), size = 0.9) + 
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Year", y = "Total flow (cfs)", color = "Flow Type", 
       title = paste0("Reach y3 - ", y_crosswalk$Reach_Name[y_crosswalk$R_ID == "y3"], " USGS ",
                      y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y3"]))
p3

# plot of average monthly natural and reconstructed natural 
p4<- y3_wb %>% select(Date, nat_flow_af, recon_nat_flow) %>%
  pivot_longer(-Date, names_to = "flow_type", values_to = "af") %>%
  mutate(month = month(Date)) %>%
  group_by(month, flow_type) %>%
  summarise(avg_monthly_flow = mean(af)) %>%
  ggplot(.) +
  geom_line(aes(x = month, y = avg_monthly_flow, color = flow_type), size = 0.9)+ 
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Month", y = "Average Monthly Flow (cfs)", color = "Flow Type", 
       title = paste0("Reach y3 - ", y_crosswalk$Reach_Name[y_crosswalk$R_ID == "y3"], " USGS ",
                      y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y3"]))
p4

ggsave(p3, filename = "./output/plots/y3_nat_reconstructed_flow.png", bg = "transparent")
ggsave(p4, filename = "./output/plots/y3_monthly_avg.png", bg = "transparent")


# Plot y4 =============================
# reconstruct nat flow for each gage in reach; only use the columns that have data in them
y4_wb <- y4_wb_components %>%
  filter(Date > "1974-10-01") %>%
  mutate(recon_nat_flow = sim_flow_af + supply_af - return_af + dstorage_af + evap_af,
         diff = nat_flow_af - recon_nat_flow)

# plot all wb components
p1<- y4_wb %>% 
  select(-sim_res_af) %>%
  filter(Date > "2000-01-01") %>%
  pivot_longer(cols = contains("af"), names_to = "flow_type", values_to = "af") %>%
  ggplot(.) +
  geom_line(aes(x = Date, y = af, color = flow_type), size = 0.9) +
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Year", y = "Total flow (cfs)", color = "Flow Type", 
       title = paste0("Reach y4 - ", y_crosswalk$Reach_Name[y_crosswalk$R_ID == "y4"], " USGS ",
                      y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y4"]))
p1

# plot difference between natural and reconstructed  
p2<- y4_wb %>%
  filter(Date > "2000-01-01") %>%
  ggplot(.) + 
  geom_line(aes(x = Date, y = diff)) + 
  theme_classic()  +
  labs(x = "Year", y = "Natural flow - Reconstructed Natural flow (cfs)", 
       title = paste0("Reach y4 - ", y_crosswalk$Reach_Name[y_crosswalk$R_ID == "y4"], " USGS ",
                      y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y4"]))
p2

# plot of nat flow and reconstructed natural flow overlapping
p3<- y4_wb %>% 
  select(Date, nat_flow_af, recon_nat_flow) %>%
  filter(Date > "2000-01-01") %>%
  pivot_longer(-Date, names_to = "flow_type", values_to = "af") %>%
  ggplot(.) + 
  geom_line(aes(x = Date, y = af, color = flow_type), size = 0.9) + 
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Year", y = "Total flow (cfs)", color = "Flow Type", 
       title = paste0("Reach y4 - ", y_crosswalk$Reach_Name[y_crosswalk$R_ID == "y4"], " USGS ",
                      y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y4"]))
p3

# plot of average monthly natural and reconstructed natural 
p4<- y4_wb %>% select(Date, nat_flow_af, recon_nat_flow) %>%
  pivot_longer(-Date, names_to = "flow_type", values_to = "af") %>%
  mutate(month = month(Date)) %>%
  group_by(month, flow_type) %>%
  summarise(avg_monthly_flow = mean(af)) %>%
  ggplot(.) +
  geom_line(aes(x = month, y = avg_monthly_flow, color = flow_type), size = 0.9)+ 
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Month", y = "Average Monthly Flow (cfs)", color = "Flow Type", 
       title = paste0("Reach y4 - ", y_crosswalk$Reach_Name[y_crosswalk$R_ID == "y4"], " USGS ",
                      y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y4"]))
p4

ggsave(p3, filename = "./output/plots/y4_nat_reconstructed_flow.png", bg = "transparent")
ggsave(p4, filename = "./output/plots/y4_monthly_avg.png", bg = "transparent")


# Plot y5 =============================
# reconstruct nat flow for each gage in reach; only use the columns that have data in them
y5_wb <- y5_wb_components %>%
  filter(Date > "1974-10-01") %>%
  mutate(recon_nat_flow = sim_flow_af + supply_af - return_af + dstorage_af + evap_af,
         diff = nat_flow_af - recon_nat_flow)

# plot all wb components
p1<- y5_wb %>% 
  select(-sim_res_af) %>%
  filter(Date > "2000-01-01") %>%
  pivot_longer(cols = contains("af"), names_to = "flow_type", values_to = "af") %>%
  ggplot(.) +
  geom_line(aes(x = Date, y = af, color = flow_type), size = 0.9) +
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Year", y = "Total flow (cfs)", color = "Flow Type", 
       title = paste0("Reach y5 - ", y_crosswalk$Reach_Name[y_crosswalk$R_ID == "y5"], " USGS ",
                      y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y5"]))
p1

# plot difference between natural and reconstructed  
p2<- y5_wb %>%
  filter(Date > "2000-01-01") %>%
  ggplot(.) + 
  geom_line(aes(x = Date, y = diff)) + 
  theme_classic()  +
  labs(x = "Year", y = "Natural flow - Reconstructed Natural flow (cfs)", 
       title = paste0("Reach y5 - ", y_crosswalk$Reach_Name[y_crosswalk$R_ID == "y5"], " USGS ",
                      y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y5"]))
p2

# plot of nat flow and reconstructed natural flow overlapping
p3<- y5_wb %>% 
  select(Date, nat_flow_af, recon_nat_flow) %>%
  filter(Date > "2000-01-01") %>%
  pivot_longer(-Date, names_to = "flow_type", values_to = "af") %>%
  ggplot(.) + 
  geom_line(aes(x = Date, y = af, color = flow_type), size = 0.9) + 
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Year", y = "Total flow (cfs)", color = "Flow Type", 
       title = paste0("Reach y5 - ", y_crosswalk$Reach_Name[y_crosswalk$R_ID == "y5"], " USGS ",
                      y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y5"]))
p3

# plot of average monthly natural and reconstructed natural 
p4<- y5_wb %>% select(Date, nat_flow_af, recon_nat_flow) %>%
  pivot_longer(-Date, names_to = "flow_type", values_to = "af") %>%
  mutate(month = month(Date)) %>%
  group_by(month, flow_type) %>%
  summarise(avg_monthly_flow = mean(af)) %>%
  ggplot(.) +
  geom_line(aes(x = month, y = avg_monthly_flow, color = flow_type), size = 0.9)+ 
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Month", y = "Average Monthly Flow (cfs)", color = "Flow Type", 
       title = paste0("Reach y5 - ", y_crosswalk$Reach_Name[y_crosswalk$R_ID == "y5"], " USGS ",
                      y_crosswalk$Gage_ID[y_crosswalk$R_ID == "y5"]))
p4

ggsave(p3, filename = "./output/plots/y5_nat_reconstructed_flow.png", bg = "transparent")
ggsave(p4, filename = "./output/plots/y5_monthly_avg.png", bg = "transparent")


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
c_tot_supply_tidy <- c_tot_supply %>%
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
#c_crosswalk <- crosswalk %>%
#  filter(River == "Crystal")


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
supply_tmp <- c_tot_supply_tidy %>%
  filter(node %in% rf1$WDID) %>%
  group_by(Date) %>%
  summarise(supply_af = sum(af))
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

#Only 4 nodes in this section. None appear in return, dstorage, evap, res, or sim_res.
#Only 1 reservoir in entire area of interest - ruedi reservoir. 
#3 nodes, all "minimum flow" nodes, do not appear because they do not impact (take away or return water)

rf1_wb_components <- nat_tmp %>% 
  full_join(sim_tmp, by = c("Date", "node")) %>%
  full_join(return_tmp, by = "Date") %>%
  full_join(supply_tmp, by = "Date") %>%
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
supply_tmp <- c_tot_supply_tidy %>%
  filter(node %in% rf2$WDID) %>%
  group_by(Date) %>%
  summarise(supply_af = sum(af))
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
  full_join(supply_tmp, by = "Date") %>%
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
supply_tmp <- c_tot_supply_tidy %>%
  filter(node %in% rf3$WDID) %>%
  group_by(Date) %>%
  summarise(supply_af = sum(af))
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
  full_join(supply_tmp, by = "Date") %>%
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
supply_tmp <- c_tot_supply_tidy %>%
  filter(node %in% rf4$WDID) %>%
  group_by(Date) %>%
  summarise(supply_af = sum(af))
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
  full_join(supply_tmp, by = "Date") %>%
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
supply_tmp <- c_tot_supply_tidy %>%
  filter(node %in% rf5$WDID) %>%
  group_by(Date) %>%
  summarise(supply_af = sum(af))
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
  full_join(supply_tmp, by = "Date") %>%
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
supply_tmp <- c_tot_supply_tidy %>%
  filter(node %in% rf6$WDID) %>%
  group_by(Date) %>%
  summarise(supply_af = sum(af))
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
  full_join(supply_tmp, by = "Date") %>%
  full_join(dstorage_tmp, by = "Date") %>%
  full_join(evap_tmp, by = "Date") %>%
  full_join(sim_res_tmp, by = "Date") %>%
  mutate(Date = paste0(Date, "-01"),
         Date = as.Date(Date))




# Map impact ===================


#impact: add supply and returns columns together then sum over the year then average across years
supply_return <- c_tot_supply_tidy %>% 
  rename(supply = af) %>% 
  full_join(c_tot_return_tidy) %>%
  rename(return = af) %>%
  mutate(impact = supply - return)

storage_evap <- c_dstorage_tidy %>%
  rename(dstorage = af) %>%
  full_join(c_res_evap_tidy, by = c("Date", "node")) %>%
  rename(evap = af) %>%
  mutate(impact = dstorage + evap)

avg_seasonal_flow <- c_nat_flow_tidy %>%
  mutate(Date = paste0(Date, "-01"), Date = as.Date(Date), year = year(Date), month = month(Date)) %>%
  filter(month > 4 & month < 10) %>%
  group_by(node) %>%
  summarise(avg_flow = mean(af))
#don't know how to crosswalk into the impact df

impact <- full_join(supply_return, storage_evap, by = c("Date", "node", "impact")) %>%
  mutate(Date = paste0(Date, "-01"), Date = as.Date(Date), year = year(Date), month = month(Date)) %>%
  filter(month > 4 & month < 10) %>%
  group_by(node, year) %>%
  summarise(tot_impact = sum(impact)) %>%
  group_by(node) %>%
  summarise(avg_tot_impact = mean(tot_impact))

ggplot() +
  geom_histogram(data = impact, aes(x = avg_tot_impact))
#72 total nodes with impact values, despite there being 118 total nodes in last reach of RF

mapping_impact <- full_join(rf6, impact, by = c("ID" = "node")) %>%
  mutate(abs_impact = abs(avg_tot_impact), #make absolute column
         sign = ifelse(avg_tot_impact > 0, 1, ifelse(avg_tot_impact == 0, 0, -1)))
write.csv(mapping_impact, "./output/mapping_impact_all.csv", row.names = FALSE)

# problems with importing csv into arcgis, so attempt to make it shp here
# upload existing shp used to see projection info

library(sf)
rf_nodes <- st_read("../data/RoaringFork_watershed_delineation/globalwatershed.shp")
mapping_impact_sf <- st_as_sf(mapping_impact, coords = c("UTMX", "UTMY"), crs = our_crs)
st_write(mapping_impact_sf, "./output/mapping_impact_rf/mapping_impact_summer_rf.shp")


# skipping this step since rf1 only has 4 nodes beneath it anyway
# get top 5 impacters
rf1_top5 <- mapping_impact_sf %>%
  arrange(desc(avg_tot_impact)) %>%
  slice_head(n=5) %>%
  mutate(reach = "rf1")
rf2_top5 <- mapping_impact_sf %>% filter(WDID %in% rf2$WDID) %>%
  arrange(desc(avg_tot_impact)) %>%
  slice_head(n=5) %>%
  mutate(reach = "rf2")
rf3_top5 <- mapping_impact_sf %>% filter(WDID %in% rf3$WDID) %>%
  arrange(desc(avg_tot_impact)) %>%
  slice_head(n=5) %>%
  mutate(reach = "rf3")
rf4_top5 <- mapping_impact_sf %>% filter(WDID %in% rf4$WDID) %>%
  arrange(desc(avg_tot_impact)) %>%
  slice_head(n=5) %>%
  mutate(reach = "rf4")
rf5_top5 <- mapping_impact_sf %>% filter(WDID %in% rf5$WDID) %>%
  arrange(desc(avg_tot_impact)) %>%
  slice_head(n=5) %>%
  mutate(reach = "rf5")
rf6_top5 <- mapping_impact_sf %>% filter(WDID %in% rf6$WDID) %>%
  arrange(desc(avg_tot_impact)) %>%
  slice_head(n=5) %>%
  mutate(reach = "rf6")

all_top5 <- rbind(rf1_top5, rf2_top5, rf3_top5, rf4_top5, rf5_top5, rf6_top5)
write.csv(all_top5, "./output/all_top5.csv", row.names = FALSE)


#save as individual st's
st_write(rf1_top5, "./output/rf1_top5.shp")
st_write(rf2_top5, "./output/rf2_top5.shp")
st_write(rf3_top5, "./output/rf3_top5.shp")
st_write(rf4_top5, "./output/rf4_top5.shp")
st_write(rf5_top5, "./output/rf5_top5.shp")
st_write(rf6_top5, "./output/rf6_top5.shp")
st_write(all_top5, "./output/all_top5.shp")


# save RDS files =============================
saveRDS(rf1_wb_components, "./output/rf1_wb_components.rds")
saveRDS(rf2_wb_components, "./output/rf2_wb_components.rds")
saveRDS(rf3_wb_components, "./output/rf3_wb_components.rds")
saveRDS(rf4_wb_components, "./output/rf4_wb_components.rds")
saveRDS(rf5_wb_components, "./output/rf5_wb_components.rds")
saveRDS(rf6_wb_components, "./output/rf6_wb_components.rds")






# Plotting ---------------------------------------------------------------

# Plot rf1 =============================
# reconstruct nat flow for each gage in reach; only use the columns that have data in them
rf1_wb <- rf1_wb_components %>%
  filter(Date > "1974-10-01") %>%
  mutate(recon_nat_flow = sim_flow_af + supply_af
         #- return_af + dstorage_af + evap_af
         ,
         diff = nat_flow_af - recon_nat_flow)

# plot all wb components
p1<- rf1_wb %>% 
  select(-sim_res_af) %>%
  filter(Date > "2000-01-01") %>%
  pivot_longer(cols = contains("af"), names_to = "flow_type", values_to = "af") %>%
  ggplot(.) +
  geom_line(aes(x = Date, y = af, color = flow_type), size = 0.9) +
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Year", y = "Total flow (cfs)", color = "Flow Type", 
       title = "Reach rf1 - Weller Lake, USGS 09073300")

# plot difference between natural and reconstructed  
p2<- rf1_wb %>%
  filter(Date > "2000-01-01") %>%
  ggplot(.) + 
  geom_line(aes(x = Date, y = diff)) + 
  theme_classic()  +
  labs(x = "Year", y = "Natural flow - Reconstructed Natural flow (cfs)", 
       title = "Reach rf1 - Weller Lake, USGS 09073300")

# plot of nat flow and reconstructed natural flow overlapping
p3<- rf1_wb %>% 
  select(Date, nat_flow_af, recon_nat_flow) %>%
  filter(Date > "2000-01-01") %>%
  pivot_longer(-Date, names_to = "flow_type", values_to = "af") %>%
  ggplot(.) + 
  geom_line(aes(x = Date, y = af, color = flow_type), size = 0.9) + 
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Year", y = "Total flow (cfs)", color = "Flow Type", 
       title = "Reach rf1 - Weller Lake, USGS 09073300")

# plot of average monthly natural and reconstructed natural 
p4<- rf1_wb %>% select(Date, nat_flow_af, recon_nat_flow) %>%
  pivot_longer(-Date, names_to = "flow_type", values_to = "af") %>%
  mutate(month = month(Date)) %>%
  group_by(month, flow_type) %>%
  summarise(avg_monthly_flow = mean(af)) %>%
  ggplot(.) +
  geom_line(aes(x = month, y = avg_monthly_flow, color = flow_type), size = 0.9)+ 
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Month", y = "Average Monthly Flow (cfs)", color = "Flow Type", 
       title = "Reach rf1 - Weller Lake, USGS 09073300")

ggsave(p3, filename = "./output/plots/rf1_nat_reconstructed_flow.png", bg = "transparent")
ggsave(p4, filename = "./output/plots/rf1_monthly_avg.png", bg = "transparent")


# Plot rf2 =============================
# reconstruct nat flow for each gage in reach; only use the columns that have data in them
rf2_wb <- rf2_wb_components %>%
  filter(Date > "1974-10-01") %>%
  mutate(recon_nat_flow = sim_flow_af + supply_af - return_af
         #- return_af + dstorage_af + evap_af
         ,
         diff = nat_flow_af - recon_nat_flow)

# plot all wb components
rf2_wb %>% 
  select(-sim_res_af) %>%
  filter(Date > "2000-01-01") %>%
  pivot_longer(cols = contains("af"), names_to = "flow_type", values_to = "af") %>%
  ggplot(.) +
  geom_line(aes(x = Date, y = af, color = flow_type), size = 0.9) +
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Year", y = "Total flow (cfs)", color = "Flow Type", 
       title = "Reach rf2 - Slaughterhouse, USGS 09076300")

# plot difference between natural and reconstructed  
rf2_wb %>%
  filter(Date > "2000-01-01") %>%
  ggplot(.) + 
  geom_line(aes(x = Date, y = diff)) + 
  theme_classic()  +
  labs(x = "Year", y = "Natural flow - Reconstructed Natural flow (cfs)", 
       title = "Reach rf2 - Slaughterhouse, USGS 09076300")

# plot of nat flow and reconstructed natural flow overlapping
p3<- rf2_wb %>% 
  select(Date, nat_flow_af, recon_nat_flow) %>%
  filter(Date > "2000-01-01") %>%
  pivot_longer(-Date, names_to = "flow_type", values_to = "af") %>%
  ggplot(.) + 
  geom_line(aes(x = Date, y = af, color = flow_type), size = 0.9) + 
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Year", y = "Total flow (cfs)", color = "Flow Type", 
       title = "Reach rf2 - Slaughterhouse, USGS 09076300")

# plot of average monthly natural and reconstructed natural 
p4<- rf2_wb %>% select(Date, nat_flow_af, recon_nat_flow) %>%
  pivot_longer(-Date, names_to = "flow_type", values_to = "af") %>%
  mutate(month = month(Date)) %>%
  group_by(month, flow_type) %>%
  summarise(avg_monthly_flow = mean(af)) %>%
  ggplot(.) +
  geom_line(aes(x = month, y = avg_monthly_flow, color = flow_type), size = 0.9)+ 
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Month", y = "Average Monthly Flow (cfs)", color = "Flow Type", 
       title = "Reach rf2 - Slaughterhouse, USGS 09076300")

ggsave(p3, filename = "./output/plots/rf2_nat_reconstructed_flow.png", bg = "transparent")
ggsave(p4, filename = "./output/plots/rf2_monthly_avg.png", bg = "transparent")



# Plot rf3 =============================
# reconstruct nat flow for each gage in reach; only use the columns that have data in them
rf3_wb <- rf3_wb_components %>%
  filter(Date > "1974-10-01") %>%
  mutate(recon_nat_flow = sim_flow_af + supply_af - return_af
         #- return_af + dstorage_af + evap_af
         ,
         diff = nat_flow_af - recon_nat_flow)

# plot all wb components
rf3_wb %>% 
  select(-sim_res_af) %>%
  filter(Date > "2000-01-01") %>%
  pivot_longer(cols = contains("af"), names_to = "flow_type", values_to = "af") %>%
  ggplot(.) +
  geom_line(aes(x = Date, y = af, color = flow_type), size = 0.9) +
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Year", y = "Total flow (cfs)", color = "Flow Type", 
       title = "Reach rf3 - Upper Woody Creek, USGS 09076300")

# plot difference between natural and reconstructed  
rf3_wb %>%
  filter(Date > "2000-01-01") %>%
  ggplot(.) + 
  geom_line(aes(x = Date, y = diff)) + 
  theme_classic()  +
  labs(x = "Year", y = "Natural flow - Reconstructed Natural flow (cfs)", 
       title = "Reach rf3 - Upper Woody Creek, USGS 09076300")

# plot of nat flow and reconstructed natural flow overlapping
p3<- rf3_wb %>% 
  select(Date, nat_flow_af, recon_nat_flow) %>%
  filter(Date > "2000-01-01") %>%
  pivot_longer(-Date, names_to = "flow_type", values_to = "af") %>%
  ggplot(.) + 
  geom_line(aes(x = Date, y = af, color = flow_type), size = 0.9) + 
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Year", y = "Total flow (cfs)", color = "Flow Type", 
       title = "Reach rf3 - Upper Woody Creek, USGS 09076300")

# plot of average monthly natural and reconstructed natural 
p4<- rf3_wb %>% select(Date, nat_flow_af, recon_nat_flow) %>%
  pivot_longer(-Date, names_to = "flow_type", values_to = "af") %>%
  mutate(month = month(Date)) %>%
  group_by(month, flow_type) %>%
  summarise(avg_monthly_flow = mean(af)) %>%
  ggplot(.) +
  geom_line(aes(x = month, y = avg_monthly_flow, color = flow_type), size = 0.9)+ 
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Month", y = "Average Monthly Flow (cfs)", color = "Flow Type", 
       title = "Reach rf3 - Upper Woody Creek, USGS 09076300")

ggsave(p3, filename = "./output/plots/rf3_nat_reconstructed_flow.png", bg = "transparent")
ggsave(p4, filename = "./output/plots/rf3_monthly_avg.png", bg = "transparent")




# Plot rf4 =============================
# reconstruct nat flow for each gage in reach; only use the columns that have data in them
rf4_wb <- rf4_wb_components %>%
  filter(Date > "1974-10-01") %>%
  mutate(recon_nat_flow = sim_flow_af + supply_af - return_af
         #- return_af + dstorage_af + evap_af
         ,
         diff = nat_flow_af - recon_nat_flow)

# plot all wb components
rf4_wb %>% 
  select(-sim_res_af) %>%
  filter(Date > "2000-01-01") %>%
  pivot_longer(cols = contains("af"), names_to = "flow_type", values_to = "af") %>%
  ggplot(.) +
  geom_line(aes(x = Date, y = af, color = flow_type), size = 0.9) +
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Year", y = "Total flow (cfs)", color = "Flow Type", 
       title = "Reach rf4 - Lower Woody Creek, USGS 09076300")

# plot difference between natural and reconstructed  
rf4_wb %>%
  filter(Date > "2000-01-01") %>%
  ggplot(.) + 
  geom_line(aes(x = Date, y = diff)) + 
  theme_classic()  +
  labs(x = "Year", y = "Natural flow - Reconstructed Natural flow (cfs)", 
       title = "Reach rf4 - Lower Woody Creek, USGS 09076300")

# plot of nat flow and reconstructed natural flow overlapping
p3<- rf4_wb %>% 
  select(Date, nat_flow_af, recon_nat_flow) %>%
  filter(Date > "2000-01-01") %>%
  pivot_longer(-Date, names_to = "flow_type", values_to = "af") %>%
  ggplot(.) + 
  geom_line(aes(x = Date, y = af, color = flow_type), size = 0.9) + 
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Year", y = "Total flow (cfs)", color = "Flow Type", 
       title = "Reach rf4 - Lower Woody Creek, USGS 09076300")

# plot of average monthly natural and reconstructed natural 
p4<- rf4_wb %>% select(Date, nat_flow_af, recon_nat_flow) %>%
  pivot_longer(-Date, names_to = "flow_type", values_to = "af") %>%
  mutate(month = month(Date)) %>%
  group_by(month, flow_type) %>%
  summarise(avg_monthly_flow = mean(af)) %>%
  ggplot(.) +
  geom_line(aes(x = month, y = avg_monthly_flow, color = flow_type), size = 0.9)+ 
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Month", y = "Average Monthly Flow (cfs)", color = "Flow Type", 
       title = "Reach rf4 - Lower Woody Creek, USGS 09076300")

ggsave(p3, filename = "./output/plots/rf4_nat_reconstructed_flow.png", bg = "transparent")
ggsave(p4, filename = "./output/plots/rf4_monthly_avg.png", bg = "transparent")




# Plot rf5 =============================
# reconstruct nat flow for each gage in reach; only use the columns that have data in them
rf5_wb <- rf5_wb_components %>%
  filter(Date > "1974-10-01") %>%
  mutate(recon_nat_flow = sim_flow_af + supply_af - return_af
         + dstorage_af + evap_af
         ,
         diff = nat_flow_af - recon_nat_flow)

# plot all wb components
rf5_wb %>% 
  select(-sim_res_af) %>%
  filter(Date > "2000-01-01") %>%
  pivot_longer(cols = contains("af"), names_to = "flow_type", values_to = "af") %>%
  ggplot(.) +
  geom_line(aes(x = Date, y = af, color = flow_type), size = 0.9) +
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Year", y = "Total flow (cfs)", color = "Flow Type", 
       title = "Reach rf5 - Basalt to Carbondale, USGS 09081000")

# plot difference between natural and reconstructed  
rf5_wb %>%
  filter(Date > "2000-01-01") %>%
  ggplot(.) + 
  geom_line(aes(x = Date, y = diff)) + 
  theme_classic()  +
  labs(x = "Year", y = "Natural flow - Reconstructed Natural flow (cfs)", 
       title = "Reach rf5 - Basalt to Carbondale, USGS 09081000")

# plot of nat flow and reconstructed natural flow overlapping
p3<- rf5_wb %>% 
  select(Date, nat_flow_af, recon_nat_flow) %>%
  filter(Date > "2000-01-01") %>%
  pivot_longer(-Date, names_to = "flow_type", values_to = "af") %>%
  ggplot(.) + 
  geom_line(aes(x = Date, y = af, color = flow_type), size = 0.9) + 
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Year", y = "Total flow (cfs)", color = "Flow Type", 
       title = "Reach rf5 - Basalt to Carbondale, USGS 09081000")

# plot of average monthly natural and reconstructed natural 
p4<- rf5_wb %>% select(Date, nat_flow_af, recon_nat_flow) %>%
  pivot_longer(-Date, names_to = "flow_type", values_to = "af") %>%
  mutate(month = month(Date)) %>%
  group_by(month, flow_type) %>%
  summarise(avg_monthly_flow = mean(af)) %>%
  ggplot(.) +
  geom_line(aes(x = month, y = avg_monthly_flow, color = flow_type), size = 0.9)+ 
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Month", y = "Average Monthly Flow (cfs)", color = "Flow Type", 
       title = "Reach rf5 - Basalt to Carbondale, USGS 09081000")


ggsave(p3, filename = "./output/plots/rf5_nat_reconstructed_flow.png", bg = "transparent")
ggsave(p4, filename = "./output/plots/rf5_monthly_avg.png", bg = "transparent")



# Plot rf6 =============================
# reconstruct nat flow for each gage in reach; only use the columns that have data in them
rf6_wb <- rf6_wb_components %>%
  filter(Date > "1974-10-01") %>%
  mutate(recon_nat_flow = sim_flow_af + supply_af - return_af
         + dstorage_af + evap_af
         ,
         diff = nat_flow_af - recon_nat_flow)

# plot all wb components
rf6_wb %>% 
  select(-sim_res_af) %>%
  filter(Date > "2000-01-01") %>%
  pivot_longer(cols = contains("af"), names_to = "flow_type", values_to = "af") %>%
  ggplot(.) +
  geom_line(aes(x = Date, y = af, color = flow_type), size = 0.9) +
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Year", y = "Total flow (cfs)", color = "Flow Type", 
       title = "Reach rf6 - Black Bridge to Veltus, USGS 09085000")

# plot difference between natural and reconstructed  
rf6_wb %>%
  filter(Date > "2000-01-01") %>%
  ggplot(.) + 
  geom_line(aes(x = Date, y = diff)) + 
  theme_classic()  +
  labs(x = "Year", y = "Natural flow - Reconstructed Natural flow (cfs)", 
       title = "Reach rf6 - Black Bridge to Veltus, USGS 09085000")

# plot of nat flow and reconstructed natural flow overlapping
p3<- rf6_wb %>% 
  select(Date, nat_flow_af, recon_nat_flow) %>%
  filter(Date > "2000-01-01") %>%
  pivot_longer(-Date, names_to = "flow_type", values_to = "af") %>%
  ggplot(.) + 
  geom_line(aes(x = Date, y = af, color = flow_type), size = 0.9) + 
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Year", y = "Total flow (cfs)", color = "Flow Type", 
       title = "Reach rf6 - Black Bridge to Veltus, USGS 09085000")

# plot of average monthly natural and reconstructed natural 
p4<- rf6_wb %>% select(Date, nat_flow_af, recon_nat_flow) %>%
  pivot_longer(-Date, names_to = "flow_type", values_to = "af") %>%
  mutate(month = month(Date)) %>%
  group_by(month, flow_type) %>%
  summarise(avg_monthly_flow = mean(af)) %>%
  ggplot(.) +
  geom_line(aes(x = month, y = avg_monthly_flow, color = flow_type), size = 0.9)+ 
  theme_classic() + scale_color_manual(values = pal1) +
  labs(x = "Month", y = "Average Monthly Flow (cfs)", color = "Flow Type", 
       title = "Reach rf6 - Black Bridge to Veltus, USGS 09085000")


ggsave(p3, filename = "./output/plots/rf6_nat_reconstructed_flow.png", bg = "transparent")
ggsave(p4, filename = "./output/plots/rf6_monthly_avg.png", bg = "transparent")

# Notes -------------------------------------------------------------------


#dstorage and res_evap are for the 12 reservoirs in the model
  
  #y1 = yampa river park to transit center (most upstream)
  #y2 = transit center to pump station
  #y3 = little yampa canyon
  #y4 = cross mountain gorge
  #y5 = deerlodge park to echo canyon


# positive dstorage means storage in reservoir is increasing. Good for reservoir, bad for streamflow

# impact from reservoirs = dstorage + res_evap  

