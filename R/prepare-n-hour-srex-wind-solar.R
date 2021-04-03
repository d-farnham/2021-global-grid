# make the solar files into 2-hour files
prepare_n_hour_srex_demand_wind_solar = function(hour_multiple){
  
# load the data
SREXs_capacity <- read.csv("data/capacity_factors_wind_solar/SREXs_capacity.csv")

  
# only keep years 2016-2018
SREXs_capacity = SREXs_capacity %>% dplyr::filter(Year %in% c(2015:2018))

  
for(ssrex in 1:26){
# solar  
tmp_CFs = SREXs_capacity %>% dplyr::select(Year, Month, Day, Hour, tidyselect::contains(paste0("SREX", ssrex, "."))) %>%
                             setNames(c('year', 'month', 'day', 'hour', 'solar_capacity'))  
  
nn_hour_solar = tmp_CFs %>% dplyr::mutate(date_time = as.POSIXct(paste0(year, '-', month, '-', day, ' ', hour, ':00'), tz = 'UTC')) %>%
  dplyr::mutate(new_date_time = round_date(date_time, unit = paste0(hour_multiple,' hours'))) %>%
  dplyr::group_by(new_date_time) %>%
  dplyr::summarise(solar_capacity = mean(solar_capacity)) %>%
  dplyr::mutate(year = lubridate::year(new_date_time),
                month = lubridate::month(new_date_time),
                day = lubridate::day(new_date_time),
                hour = lubridate::hour(new_date_time - 3600) + 1) %>% # this makes the hours start at hour_multiple and end at 24
  dplyr::select(year, month, day, hour, solar_capacity) %>%
  dplyr::ungroup()


second_row = data.frame(year = 'year', month = 'month', day = 'day', hour = 'hour', solarCF = 'solarCF')

first_row = c(noquote('BEGIN_DATA'), rep(NA, ncol(nn_hour_solar) -1))  

new_solar = rbind(first_row,
                  names(nn_hour_solar),
                  nn_hour_solar)
  
word_numbers <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

write.table(new_solar,
            paste0('MEM/Input_Data/',word_numbers[hour_multiple],'_hour_solar_srex',ssrex,'.csv'), 
            quote = FALSE,
            sep = ",",
            row.names = FALSE,
            col.names = FALSE,
            na="")
  
print(ssrex)
}

rm(list = ls()[!(ls() %in% c('hour_multiple'))])

# make the wind files into 2-hour files
# load the data
SREXw_capacity <- read.csv("data/capacity_factors_wind_solar/SREXw_capacity.csv")


# only keep years 2016-2018
SREXw_capacity = SREXw_capacity %>% dplyr::filter(Year %in% c(2015:2018))

for(ssrex in 1:26){
  # wind  
  tmp_CFw = SREXw_capacity %>% dplyr::select(Year, Month, Day, Hour, tidyselect::contains(paste0("SREX", ssrex, "."))) %>%
    setNames(c('year', 'month', 'day', 'hour', 'wind_capacity'))  
  
  nn_hour_wind = tmp_CFw %>% dplyr::mutate(date_time = as.POSIXct(paste0(year, '-', month, '-', day, ' ', hour, ':00'), tz = 'UTC')) %>%
    dplyr::mutate(new_date_time = round_date(date_time, unit = paste0(hour_multiple,' hours'))) %>%
    dplyr::group_by(new_date_time) %>%
    dplyr::summarise(wind_capacity = mean(wind_capacity)) %>%
    dplyr::mutate(year = lubridate::year(new_date_time),
                  month = lubridate::month(new_date_time),
                  day = lubridate::day(new_date_time),
                  hour = lubridate::hour(new_date_time - 3600) + 1) %>% # this makes the hours start at hour_multiple and end at 24
    dplyr::select(year, month, day, hour, wind_capacity) %>%
    dplyr::ungroup()
  
  
  second_row = data.frame(year = 'year', month = 'month', day = 'day', hour = 'hour', windCF = 'windCF')
  
  first_row = c(noquote('BEGIN_DATA'), rep(NA, ncol(nn_hour_wind) -1))  
  
  new_wind = rbind(first_row,
                    names(nn_hour_wind),
                    nn_hour_wind)
  
  word_numbers <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  
  write.table(new_wind,
              paste0('MEM/Input_Data/',word_numbers[hour_multiple],'_hour_wind_srex',ssrex,'.csv'), 
              quote = FALSE,
              sep = ",",
              row.names = FALSE,
              col.names = FALSE,
              na="")
  
  print(ssrex)
}
rm(list = ls())
}
