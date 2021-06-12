remove(list = ls()) # clear all workspace variables
cat("\014")         # clear command line

library(tidyverse)
library(lubridate)
library(lfstat)
library(dataRetrieval)
source("misc/find_google_drive.R")

ss_siteno = "09085000"

# Get USGS flow observations for a specific gauge
dly_flow <- readNWISdv(siteNumbers = ss_siteno,
                  parameterCd = c("00060")) %>%
  rename(flow.cms = X_00060_00003) %>%
  filter(Date > as.Date("1920-01-01"))

# calculate monthly average flow
mnthly_flow <- dly_flow %>%
  mutate(month = month(Date),
         year = year(Date)) %>%
  group_by(year, month) %>%
  summarize(flow.cms = mean(flow.cms)) %>%
  mutate(Date = as.Date(paste0(year,"-",month,"-01")))

# year-type classifications 
year_types <- mnthly_flow %>%
  mutate(na.obs = is.na(flow.cms)) %>%
  group_by(year) %>%
  summarize(annual.sum = if_else(sum(na.obs)<2,sum(flow.cms),NA_real_)) %>%
  mutate(year_class = case_when(annual.sum < quantile(annual.sum, 0.25) ~ "dry",
                                annual.sum > quantile(annual.sum, 0.25) & annual.sum < quantile(annual.sum, 0.50) ~ "dry typical",
                                annual.sum > quantile(annual.sum, 0.50) & annual.sum < quantile(annual.sum, 0.75) ~ "wet typical",
                                annual.sum > quantile(annual.sum, 0.75) ~ "wet"))

# wet-typical year, monthly data
wet_typical_mnthly <- mnthly_flow %>%
  mutate(year = year(Date)) %>%
  left_join(year_types, by = "year") %>%
  filter(year_class == "wet typical") %>%
  group_by(month) %>%
  summarize(flow.cms = mean(flow.cms))

# wet-typical year, from daily data
wet_typical_dly <- dly_flow %>%
  mutate(year = year(Date)) %>%
  left_join(year_types, by = "year") %>%
  filter(year_class == "wet typical") %>%
  mutate(doy = yday(Date)) %>%
  group_by(doy) %>%
  summarize(flow.cms = mean(flow.cms))

p1 <- ggplot(wet_typical_dly, aes(x = doy, y = flow.cms)) +
  geom_line()

print(p1)

# monthly-to-daily diaggregation of wet-typical year via Acharya and Ryu, 2014 ASCE
flow.ts <- filter(wet_typical_mnthly, month == 12) %>%
  rbind(wet_typical_mnthly) %>%
  rbind(filter(wet_typical_mnthly, month == 1)) %>%
  mutate(year = 2000) %>%
  rename(flow.mo.ts = flow.cms) %>%
  select(year, month, flow.mo.ts)
flow.ts$year[1] = 1999
flow.ts$year[length(flow.ts$year)] = 2001

flow.ss <- dly_flow %>%
  rename(flow.dy.ss = flow.cms) %>%
  mutate(year = year(Date),
         month = month(Date)) %>%
  group_by(month) %>%
  mutate(flow.mo.ss = mean(flow.dy.ss)) %>%
  select(Date, year, month, flow.dy.ss, flow.mo.ss)
  
flow.dly.est <- data.frame()

for (i in 2:(length(wet_typical_mnthly$month)-1)) {

  # extract 3-month flow window
  window <- flow.ts %>%
    ungroup() %>%
    slice((i-1):(i+1))
  
  if (window$month[2] == 1) {
    window <- window %>%
      mutate(month.shift = lead(month, default = 3))
  } else if (window$month[2] == 12) {
    window <- window %>%
      mutate(month.shift = lag(month, default = 10))
  } else {
    window <- window %>%
      mutate(month.shift = month)
  }
  
  # find 3-month period most similar
  srch_df <- flow.ss %>%
    group_by(year, month) %>%
    summarize(flow.mo.ss = mean(flow.dy.ss)) %>%
    ungroup()
  
  if (window$month[2] == 1) {
    srch_df <- srch_df %>%
      mutate(month.shift = lead(month),
             year.shift = lead(year))
  } else if (window$month[2] == 12) {
    srch_df <- srch_df %>%
      mutate(month.shift = lag(month),
             year.shift = lag(year))
  } else {
    srch_df <- srch_df %>%
      mutate(month.shift = month,
             year.shift = year)
  }
  
  srch_df <- srch_df %>%
    filter(month.shift %in% window$month.shift) %>%
    group_by(year.shift) %>%
    filter(length(month) == 3) %>%
    group_by(year.shift) %>%
    mutate(rmse = sqrt(mean(abs(flow.mo.ss - window$flow.mo.ts)^2))) %>%
    ungroup() %>%
    filter(rmse == min(rmse))
  
  yr_month_match <- as.Date(paste0(srch_df$year[2],"-",window$month[2],"-01"))
  
  # extract daily data for the best fitting year/month sequence
  dly_dat <- flow.ss %>%
    filter(Date >= yr_month_match - months(1) & Date < yr_month_match + months(2))
  
  # calclulate Streamfow Index
  si <- dly_dat %>%
    mutate(month = month(Date)) %>%
    group_by(month) %>%
    mutate(flow.mo.ss = mean(flow.dy.ss)) %>%
    mutate(si = flow.dy.ss/flow.mo.ss) %>%
    ungroup()
  
  tmp_est <- si %>%
    left_join(window, by = "month") %>%
    mutate(flow.est = si * flow.mo.ts) %>%
    mutate(dom = mday(Date)) %>%
    mutate(Date.est = as.Date(paste0(year.y,"-",month,"-",dom))) %>%
    filter(month == window$month[2]) %>%
    select(Date.est, flow.est) %>%
    rename(Date = Date.est)
  
  flow.dly.est <- flow.dly.est %>%
    rbind(tmp_est)
  
  k = 1
  
    
}


# compare disaggregated wet-typical hydrograph against daily average wet-typical hydrograph 