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

# ======================================================================================
analog_disagg <- function(flow.ts, flow.ss) {
  #' Historical analogs monthly flow disaggregation technique
  #'
  #' @description 
  #' This function disaggregated a timeseries of monthly flow data
  #' to timeseries of daily flow data using the historical analog technique
  #' outlined in Acharya and Ryu, 2014, Journal of Hydrologic Engineering 
  #'
  #' @param flow.ts dataframe. First column is datatime object (Date), second column is 
  #' monthly average flow rate (flow.mo.ts)
  #' 
  #' @param flow.ss dataframe. First column is datetime object (Date), second column is
  #' daily average flow rate (flow.dy.ss). This should be a multi-year, long-term record. 
  #' 
  #' @usage analog_dissag(flow.ts, flow.ss)
  #' 
  #' @references Acharya and Ryu (2014), Simle Method for Streamflow Disaggregation
  #'   Journal of Hydrologic Engineering, 10.1061/(ASCE)HE.1943-5584.0000818
  
  
  # restructure input dataframe
  # TODO: add check to make sure dataframe variables are correctly formated
  flow.ts <- flow.ts %>%
    rename(Date = names(.)[1],
           flow.mo.ts = names(.)[2]) %>%
    mutate(year = year(Date),
           month = month(Date)) %>%
    select(Date, year, month, flow.mo.ts)
  
  flow.ss <- flow.ss %>%
    rename(Date = names(.)[1],
           flow.dy.ss = names(.)[2]) %>%
    mutate(year = year(Date),
           month = month(Date)) %>%
    select(Date, year, month, flow.dy.ss)
    
  # initialize dataframe to contain disaggregated flow results
  flow.dly.est.analog <- data.frame()
  
  #' Loop through months in the flow.ts record. Begin loop on the second month
  #' and end loop with the second to last month on record. This ensures that the 
  #' three month window is always fully populated.
  for (i in 2:(length(flow.ts$month)-1)) {
    
    #' extract 3-month flow window from flow.ts
    # ========================================================
    window <- flow.ts %>%
      slice((i-1):(i+1))
    
    window <- shift.months(window, window$month[2])
    
    #' Find the 3-month along period in flow.ss that is most similar to the 
    #' three month window of flow.ts. Similarity is objectively evaluated
    #' with Root-Mean-Squared-Error (RMSE) statistic. The most similar
    #' analog period produces the lowest RMSE.
    #' 
    #' Note: Only common months sequences are compared. For example, a three
    #' month flow.ts window of June-July-August is compared against all complete
    #' June-July-August sequences in the historical records. 
    # ========================================================
    
    # build a monthly record from the daily record
    srch_df <- flow.ss %>%
      group_by(year, month) %>%
      summarize(flow.mo.ss = mean(flow.dy.ss), .groups = 'drop')
    
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
  
  return(flow.dly.est.analog)
}

block_disagg <- function(flow.ts) {
  
  # restructure input dataframe
  # TODO: add check to make sure dataframe variables are correctly formated
  flow.ts <- flow.ts %>%
    rename(Date = names(.)[1],
           flow.mo.ts = names(.)[2]) %>%
    mutate(year = year(Date),
           month = month(Date)) %>%
    select(Date, year, month, flow.mo.ts)
  
  flow.dly.est.block <- data.frame(approx(x = flow.ts$Date, y = flow.ts$flow.mo.ts,
                                           xout = seq(min(flow.ts$Date),max(flow.ts$Date) + months(1) - days(1), by = "1 day"), 
                                           rule = 2, method = "constant", ties = mean)) %>%
    rename(Date = x, flow.est = y)
  
  return(flow.dly.est.block)
  
}

linear_disagg <- function(flow.ts) {
  
  # restructure input dataframe
  # TODO: add check to make sure dataframe variables are correctly formated
  flow.ts <- flow.ts %>%
    rename(Date = names(.)[1],
           flow.mo.ts = names(.)[2]) %>%
    mutate(year = year(Date),
           month = month(Date)) %>%
    select(Date, year, month, flow.mo.ts)
  
  flow.dly.est.interp <- data.frame(approx(x = flow.ts$Date + days(15), y = flow.ts$flow.mo.ts,
                                           xout = seq(min(flow.ts$Date), max(flow.ts$Date) + months(1) - days(1), by = "1 day"), 
                                           rule = 2, method = "linear", ties = mean)) %>%
    rename(Date = x, flow.est = y)
  
  return(flow.dly.est.interp)
}










