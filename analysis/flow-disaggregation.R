remove(list = ls()) # clear all workspace variables
cat("\014")         # clear command line

library(tidyverse)
library(lubridate)
library(lfstat)
library(dataRetrieval)
library(zoo)
source("misc/find_google_drive.R")

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

flow.ts <- flow.ts %>%
  mutate(Date = as.Date(paste0(year,"-",month,"-01"))) %>%
  select(Date, year, month, flow.mo.ts)

test <- select(flow.ts, Date, flow.mo.ts) %>%
  rename(Date = names(.)[1],
         flow.mo.ts = names(.)[2]) %>%
  mutate(year = year(Date),
         month = month(Date)) %>%
  select(Date, year, month, flow.mo.ts)

flow.ss <- dly_flow %>%
  rename(flow.dy.ss = flow.cms) %>%
  mutate(year = year(Date),
         month = month(Date)) %>%
  select(Date, year, month, flow.dy.ss)
  
# Analog disaggregation scheme
# =========================================================================================
shift.months <- function(dat, mon_i) {
  
  if (mon_i == 1) {
    dat <- dat %>%
      mutate(month.shift = lead(month, default = tail(dat$month, 1)+1),
             year.shift = lead(year, default = if_else(tail(dat$month, 1) == 12,
                                                       tail(dat$year, 1)+1,
                                                       tail(dat$year, 1)
                                                       )
                               )
             )
  } else if (mon_i == 12) {
    dat <- dat %>%
      mutate(month.shift = lag(month, default = head(dat$month, 1)-1),
             year.shift = lag(year, default = if_else(head(dat$month, 1) == 1,
                                                       head(dat$year, 1)-1,
                                                       head(dat$year, 1)
                                                      )
                              )
            )
  } else {
    dat <- dat %>%
      mutate(month.shift = month,
             year.shift = year)
  }
  
  return(dat)
  
}


flow.dly.est.analog <- data.frame()
for (i in 2:(length(flow.ts$month)-1)) {

  # extract 3-month flow window from flow.ts
  # ========================================================
  window <- flow.ts %>%
    ungroup() %>%
    slice((i-1):(i+1))
  
  window <- shift.months(window, window$month[2])

  # find 3-month period in flow.ss that is most similar to flow.ts window
  # Best fit minimizes RMSE
  # ========================================================
  srch_df <- flow.ss %>%
    group_by(year, month) %>%
    summarize(flow.mo.ss = mean(flow.dy.ss)) %>%
    ungroup()
  
  srch_df <- shift.months(srch_df, window$month[2])
  
  srch_df <- srch_df %>%
    filter(month.shift %in% window$month.shift) %>%
    group_by(year.shift) %>%
    filter(length(month) == 3) %>%
    group_by(year.shift) %>%
    mutate(rmse = sqrt(mean(abs(flow.mo.ss - window$flow.mo.ts)^2))) %>%
    ungroup() %>%
    filter(rmse == min(rmse, na.rm = T))
  
  yr_month_match <- as.Date(paste0(srch_df$year[2],"-",window$month[2],"-01"))
  print(yr_month_match)
  
  # extract daily data for the best fitting year/month sequence
  dly_dat <- flow.ss %>%
    filter(Date >= yr_month_match - months(1) & Date < yr_month_match + months(2))
  
  # calclulate Streamfow Index from flow.ss
  # ========================================================
  si <- dly_dat %>%
    mutate(month = month(Date)) %>%
    group_by(month) %>%
    mutate(flow.mo.ss = mean(flow.dy.ss)) %>%
    mutate(si = flow.dy.ss/flow.mo.ss) %>%
    ungroup()
  
  # Disaggregate monthly flow to daily flow for month i
  # =========================================================
  tmp_est <- si %>%
    left_join(window, by = "month") %>%
    mutate(flow.est = si * flow.mo.ts) %>%
    mutate(dom = mday(Date.x)) %>%
    mutate(Date.est = as.Date(paste0(year.y,"-",month,"-",dom))) %>%
    filter(month == window$month[2]) %>%
    select(Date.est, flow.est) %>%
    rename(Date = Date.est)
  
  # Bind data together
  # =========================================================
  flow.dly.est.analog <- flow.dly.est.analog %>%
    rbind(tmp_est)
    
}

# Tetris dissgregation scheme
# =========================================================================================
flow.dly.est.tetris <- data.frame(approx(x = flow.ts$Date, y = flow.ts$flow.mo.ts,
                     xout = seq(min(flow.ts$Date),max(flow.ts$Date) + months(1) - days(1), by = "1 day"), 
                     rule = 2, method = "constant", ties = mean)) %>%
  rename(Date = x, flow.est = y)

# Linear disaggregation scheme
# =========================================================================================
flow.dly.est.interp <- data.frame(approx(x = flow.ts$Date + days(15), y = flow.ts$flow.mo.ts,
                           xout = seq(min(flow.ts$Date), max(flow.ts$Date) + months(1) - days(1), by = "1 day"), 
                           rule = 2, method = "linear", ties = mean)) %>%
  rename(Date = x, flow.est = y)

# compare disaggregated wet-typical hydrograph against daily average wet-typical hydrograph 
# =========================================================================================

# @ Crystal - Redstone to Penny Hot Springs
low_flow = 500
high_flow = 3000

# compute the number of boatable days for each wet-typical year type
# observed
bd.wettypical.obs <- flow.ss %>%
  filter(year %in% filter(year_types, year_class == "wet typical")$year) %>%
  mutate(bd = if_else(flow.dy.ss >= low_flow & flow.dy.ss <= high_flow, 1, 0)) %>%
  group_by(year) %>%
  summarize(boat.days = sum(bd))

# analog method on monthly averages
bd.wettypical.analog <- flow.dly.est.analog %>%
  mutate(bd = if_else(flow.est >= low_flow & flow.est <= high_flow, 1, 0)) %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarize(boat.days = sum(bd))

# tetris method on monthy averages
bd.wettypical.tetris <- flow.dly.est.tetris %>%
  mutate(bd = if_else(flow.est >= low_flow & flow.est <= high_flow, 1, 0)) %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarize(boat.days = sum(bd))

# interpolation method on monthly averages
bd.wettypical.interp <- flow.dly.est.interp %>%
  mutate(bd = if_else(flow.est >= low_flow & flow.est <= high_flow, 1, 0)) %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarize(boat.days = sum(bd))

# speghetti plot of all wet-typical annual hydrographs
df <- flow.ss %>%
  filter(year %in% filter(year_types, year_class == "wet typical")$year) %>%
  mutate(doy = yday(Date)) %>%
  mutate(year = as.factor(year(Date)))

df_mo <- flow.ss %>%
  filter(year %in% filter(year_types, year_class == "wet typical")$year) %>%
  mutate(month = month(Date)) %>%
  group_by(month) %>%
  summarize(flow.mo.ss = mean(flow.dy.ss)) %>%
  ungroup() %>%
  mutate(year = 2000) %>%
  mutate(Date = as.Date(paste0(year,"-",month,"-15"))) %>%
  mutate(doy = yday(Date))

p1 <- ggplot(df, aes(x = doy, y = flow.dy.ss, col = year)) +
  geom_line() +
  geom_point(data = df_mo, aes(x = doy, y = flow.mo.ss), col = "black", size = 4) +
  ylab("Flow (cfs)") +
  xlab("Day of Year") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme(legend.text=element_text(size=12))

print(p1)

# compare disaggregation methods - line plots
df <- flow.dly.est.analog %>%
  rename(`historical analogs` = flow.est) %>%
  left_join(rename(flow.dly.est.interp, `linear interpolation` = flow.est), by = "Date") %>%
  left_join(rename(flow.dly.est.tetris, `constant monthly flow` = flow.est), by = "Date") %>%
  pivot_longer(!Date, names_to = "method", values_to = "flow.est") %>%
  mutate(doy = yday(Date)) %>%
  filter(method == "historical analogs")

p2 <- ggplot(df, aes(x = doy, y = flow.est, col = method)) +
  geom_line() +
  geom_point(data = df_mo, aes(x = doy, y = flow.mo.ss), col = "black", size = 4) +
  ylab("Flow (cfs)") +
  xlab("Day of Year") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme(legend.text=element_text(size=12)) +
  theme(legend.title=element_blank())


print(p2)

# box plot of boatable days
bp <- ggplot(bd.wettypical.obs, aes(x = 0, y = boat.days)) +
  geom_boxplot() +
  geom_jitter() +
  ylab("# of Boatable Days") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

print(bp)
