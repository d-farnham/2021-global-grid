# make the unnormalized demand files into 2-hour files
prepare_n_hour_srex_demand = function(prototype_demand_series_file,
                                      prototype_SREX_ID,
                                      hour_multiple){
  

# load in the srex region time offsets
time_offsets = read_csv('data/srex_region_time_offsets.csv') %>%
                 setNames(c('srex_index', 'srex_short_name', 'srex_long_name', 'offset_from_ENA', 'S_hemisphere'))
                

time_offsets = time_offsets %>% dplyr::mutate(prototype_offset = offset_from_ENA[srex_short_name == prototype_SREX_ID],
                                              offset_from_prototype = offset_from_ENA - prototype_offset) %>%
                                dplyr::select(-offset_from_ENA, -prototype_offset, -srex_long_name)


TWh_to_kWh = 1e6

kWh_to_mean_kW = 1 / (366 * 24)

# load the srex demand regions mean demand
SREX_demand_weight <- read.csv("data/SREX_demand_weightings.csv",
                               skip = 0,
                               header = TRUE) %>% setNames(c('srex', 'mean_demand_TWh')) %>%
                                                  dplyr::mutate(mean_demand_kW = mean_demand_TWh * TWh_to_kWh * kWh_to_mean_kW)



time_offsets_and_demand_weights = merge(time_offsets,
                                        SREX_demand_weight,
                                        by.x = 'srex_index',
                                        by.y = 'srex') %>%
                                   dplyr::mutate(demand_ratio_to_prototype = mean_demand_kW/mean_demand_kW[srex_short_name == prototype_SREX_ID])

prototype_mean_kW = time_offsets_and_demand_weights$mean_demand_kW[time_offsets_and_demand_weights$srex_short_name == prototype_SREX_ID]

MW_to_kW = 1e3

# load the prototype demand time-series and make the mean match the mean of the prototype SREX ID
prototype_one_hour_demand = read_csv(prototype_demand_series_file) %>%
                              setNames(c('date_time', 'raw_demand_MW', 'cleaned_demand_MW')) %>%
                              dplyr::select(-raw_demand_MW) %>%
                              dplyr::mutate(cleaned_demand_kW = cleaned_demand_MW * prototype_mean_kW * MW_to_kW / mean(cleaned_demand_MW))

for(ssrex in 1:26){
# apply time offsets and re-scaling to srex demands and then average to nn hours
tmp_time_offset_rescale = time_offsets_and_demand_weights %>% dplyr::filter(srex_index == ssrex)
  
nn_hour_demand = prototype_one_hour_demand %>% dplyr::mutate(demand = cleaned_demand_kW * tmp_time_offset_rescale$demand_ratio_to_prototype,
                                                             date_time = date_time + 3600 * tmp_time_offset_rescale$offset_from_prototype - 3600 * 24 * 183 * tmp_time_offset_rescale$S_hemisphere) %>%
                                               dplyr::mutate(new_date_time = round_date(date_time, unit = paste0(hour_multiple,' hours'))) %>%
                                               dplyr::group_by(new_date_time) %>%
                                               dplyr::summarise(demand = round(mean(demand), 0)) %>%
                                               dplyr::mutate(year = lubridate::year(new_date_time),
                                                             month = lubridate::month(new_date_time),
                                                             day = lubridate::day(new_date_time),
                                                             hour = lubridate::hour(new_date_time - 3600) + 1) %>% # this makes the hours start at hour_multiple and end at 24
                                               dplyr::select(year, month, day, hour, demand) %>%
                                               dplyr::ungroup()


second_row = data.frame(year = 'year', month = 'month', day = 'day', hour = 'hour', demand = 'demand')

first_row = c(noquote('BEGIN_DATA'), rep(NA, ncol(nn_hour_demand) -1))  

new_demand = rbind(first_row,
                   names(nn_hour_demand),
                   nn_hour_demand)
  

word_numbers <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

write.table(new_demand,
            paste0('MEM/Input_Data/',word_numbers[hour_multiple],'_hour_',prototype_SREX_ID,'_prototype_demand_unnormalized_srex',ssrex,'.csv'), 
            quote = FALSE,
            sep = ",",
            row.names = FALSE,
            col.names = FALSE,
            na="")
  
print(ssrex)
}

rm(list=ls())
}
