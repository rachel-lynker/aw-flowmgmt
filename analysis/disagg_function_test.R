remove(list = ls()) # clear all workspace variables
cat("\014")         # clear command line

library(tidyverse)
library(lubridate)
library(lfstat)
library(dataRetrieval)
library(zoo)
source('analysis/flow_disaggregation_functions.R')

# Roraing Fork near Emma, CO
ss_siteno = "09081600"

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
  summarize(flow.cms = mean(flow.cms), .groups = 'drop') %>%
  mutate(Date = as.Date(paste0(year,"-",month,"-01"))) %>%
  select(Date, flow.cms)

# clean daily flow series
dly_flow <- dly_flow %>%
  select(Date, flow.cms)



d = seq(as.Date("1960/01/01"), as.Date("2020/01/01"), "years")
bd.compare <- data.frame()
for (y in d) {
  
  print(paste0("evaluating ", as.Date(y)))
  
  t_start = as.Date(y) - months(1)
  t_end = as.Date(y) + years(1)
  
  m <- mnthly_flow %>%
    filter(Date >= t_start & Date <= t_end)
  
  d <- dly_flow %>%
    filter(Date < t_start | Date > t_end)
  
  truth <- dly_flow %>%
    filter(Date >= t_start | Date <= t_end)
  
  # call disaggregation function
  estimate.analog <- analog_disagg(m, d) %>% rename(analog = flow.est)
  estimate.block <- block_disagg(m) %>% rename(block = flow.est)
  estimate.linear <- linear_disagg(m) %>% rename(linear = flow.est)
  
  estimate <- estimate.analog %>%
    left_join(estimate.block, by = "Date") %>%
    left_join(estimate.linear, by = "Date") %>%
    pivot_longer(!Date, names_to = "method", values_to = "flow")
  
  # calculate boatable days in estimate and truch series'
  # @ Crystal - Redstone to Penny Hot Springs
  low_flow = 500
  high_flow = 3000
  
  bd.truth <- truth %>%
    mutate(year = year(Date)) %>%
    filter(year == year(as.Date(y))) %>%
    mutate(bd = if_else(flow.cms >= low_flow & flow.cms <= high_flow, 1, 0)) %>%
    group_by(year) %>%
    summarize(truth = sum(bd), .groups = "drop")
  
  bd.estimate <- estimate %>%
    mutate(year = year(Date)) %>%
    filter(year == year(as.Date(y))) %>%
    mutate(bd = if_else(flow >= low_flow & flow <= high_flow, 1, 0)) %>%
    group_by(year, method) %>%
    summarize(estimate = sum(bd), .groups = "drop")
  
  tmp.compare <- bd.truth %>%
    left_join(bd.estimate, by = "year") %>%
    select(year, method, estimate, truth)
  
  bd.compare <- bd.compare %>%
    rbind(tmp.compare)
    
}


p1 <- ggplot(bd.compare, aes(x = truth, y = estimate, col = method)) +
  geom_point() +
  geom_abline(slope = 1)

print(p1)



