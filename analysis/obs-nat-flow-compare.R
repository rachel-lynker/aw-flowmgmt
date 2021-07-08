remove(list = ls()) # clear all workspace variables
cat("\014")         # clear command line

library(tidyverse)
library(lubridate)
library(lfstat)
source("misc/find_google_drive.R")

# This script will compare observed and naturalized flows at key whitewater indicator gauges
# along the Yampa, Roaring Fork, and Crystal Rivers.

# Import observed and naturalized flow data, generated from StateMod output (monthly)
# reformat dataframes
# left-join observed and natural flows by gauge.id and date
# =========================================================================================

# COLORADO RIVER BASIN
# ----------------------------------------------------------------------------------------
col_obs <- read.csv(paste0(drive_dir, "Gage Data/","Colorado_Gages_ObsModelData.csv")) %>%
  pivot_longer(!Date, names_to = "gauge.id", values_to = "flow.obs") %>%
  mutate(gauge.id = sub("X","",gauge.id)) %>%
  rename(date = Date) %>%
  mutate(date = as.Date(paste0(date, "-01"))) %>%
  group_by(gauge.id) %>%
  arrange(desc(date), .by_group = T)
  
col_nat <- read.csv(paste0(drive_dir, "Gage Data/","Colorado_Gages_NatFlow.csv")) %>%
  pivot_longer(!Date, names_to = "gauge.id", values_to = "flow.nat") %>%
  mutate(gauge.id = sub("X","",gauge.id)) %>%
  rename(date = Date) %>%
  mutate(date = as.Date(paste0(date, "-01"))) %>%
  group_by(gauge.id) %>%
  arrange(desc(date), .by_group = T)

col_flow <- col_nat %>%
  left_join(col_obs, by = c("date", "gauge.id"))

# YAMPA RIVER BASIN
# ----------------------------------------------------------------------------------------
yam_obs <- read.csv(paste0(drive_dir, "Gage Data/","Yampa_Gages_ObsModelData.csv")) %>%
  pivot_longer(!Date, names_to = "gauge.id", values_to = "flow.obs") %>%
  mutate(gauge.id = sub("X","",gauge.id)) %>%
  rename(date = Date) %>%
  mutate(date = as.Date(paste0(date, "-01"))) %>%
  group_by(gauge.id) %>%
  arrange(desc(date), .by_group = T)

yam_nat <- read.csv(paste0(drive_dir, "Gage Data/","Yampa_Gages_NatFlow.csv")) %>%
  pivot_longer(!Date, names_to = "gauge.id", values_to = "flow.nat") %>%
  mutate(gauge.id = sub("X","",gauge.id)) %>%
  rename(date = Date) %>%
  mutate(date = as.Date(paste0(date, "-01"))) %>%
  group_by(gauge.id) %>%
  arrange(desc(date), .by_group = T)

yam_flow <- yam_nat %>%
  left_join(yam_obs, by = c("date", "gauge.id"))

# Load list of key gauges
# =========================================================================================
gauges <- read.csv(paste0(drive_dir,"key_gauges_flow_thresholds.csv")) %>%
  
  # convert gauge ID to character with leading zero
  rename(gauge.id = Gauge.ID) %>%
  mutate(gauge.id = paste0("0",as.character(gauge.id)))

# Analyze differences at available gauges
# =========================================================================================
key_gauge_flow <- gauges %>%
  select(gauge.id, Gauge.name) %>%
  distinct() %>%
  inner_join(rbind(col_flow, yam_flow), by = "gauge.id")

df <- filter(key_gauge_flow, gauge.id == "09085000") %>%
  mutate(year = year(date)) %>%
  filter(year == 2002) %>%
  rename(`Natural Flow` = flow.nat,
         `Observed (managed) Flow` = flow.obs) %>%
  select(date, `Natural Flow`, `Observed (managed) Flow`) %>%
  pivot_longer(!date, names_to = "Classification", values_to = "flow") %>%
  mutate(Classification = as.factor(Classification))
  
plot_example <- ggplot(df, aes(x = date, y = flow, col = Classification)) +
  geom_line(size = 4) +
  ylab("Flow (AF/month)") +
  xlab("") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold")) +
  theme(legend.position="top") +
  theme(legend.text=element_text(size=12))

print(plot_example)

# American Whitewater hydrologic year type - based on Natural Flow estimates
year_types <- key_gauge_flow %>%
  mutate(na.obs = is.na(flow.nat)) %>%
  mutate(year = year(date)) %>%
  group_by(gauge.id, year) %>%
  summarize(annual.sum = if_else(sum(na.obs)<2,sum(flow.nat),NA_real_)) %>%
  group_by(gauge.id) %>%
  mutate(year_class = case_when(annual.sum < quantile(annual.sum, 0.25) ~ "dry",
                                annual.sum > quantile(annual.sum, 0.25) & annual.sum < quantile(annual.sum, 0.50) ~ "dry typical",
                                annual.sum > quantile(annual.sum, 0.50) & annual.sum < quantile(annual.sum, 0.75) ~ "wet typical",
                                annual.sum > quantile(annual.sum, 0.75) ~ "wet"))

# Management impact - difference between natural and observed flows
impact <- key_gauge_flow %>%
  mutate(diff_prop = (flow.nat - flow.obs)/flow.nat) %>%
  mutate(month = month(date)) %>%
  mutate(year = year(date)) %>%
  left_join(year_types, by = c("year", "gauge.id")) %>%
  group_by(gauge.id, Gauge.name, month, year_class) %>%
  summarize(impact = mean(diff_prop, na.rm = T)) %>%
  mutate(name = case_when(gauge.id == "09081600" ~ "Crystal nr Redstone",
                          gauge.id == "09085000" ~ "Roaring Fork nr Glenwood",
                          gauge.id == "09239500" ~ "Yampa at Deerlodge",
                          gauge.id == "09247600" ~ "Yampa at Steamboat",
                          gauge.id == "09251000" ~ "Yampa blw Craig",
                          gauge.id == "09260050" ~ "Yampa nr Maybell",)) %>%
  rename(`Year Type` = year_class) %>%
  mutate(year = 2000) %>%
  mutate(Date = as.Date(paste0(year,"-",month,"-01")))

p1 <- ggplot(impact, aes(x = Date, y = -impact * 100, col = `Year Type`)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap(~name, labeller = labeller(groupwrap = label_wrap_gen(10))) + 
  ylab("Management Impact, % of natural flow") +
  scale_x_date(date_labels = "%b") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 12)) +
  xlab("")

print(p1)
                                
                                



