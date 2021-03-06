MEM_case_input_setup_separate_regional_grids = function(
year,
delta_t,
save_file_name,
data_path,
output_path,
demand_regions,
wind_regions,
solar_regions,
curtailment_regions,
lost_load_regions,
storage_regions,
storage_efficiency,
storage_decay_rate,
storage_charging_time,
solar_fixed_cost,
wind_fixed_cost,
storage_fixed_cost,
lost_load_var_cost
){
  
# load the template for these jobs
case_input_template = read_csv('data/MEM_case_input_template.csv',
                                col_names = FALSE)  
  
# fill in the case info and year
case_input = case_input_template %>% dplyr::mutate(X2 = ifelse(X1 == 'year_start', year, X2),
                                                   X2 = ifelse(X1 == 'year_end', year, X2),
                                                   X2 = ifelse(X1 == 'delta_t', delta_t, X2),
                                                   X2 = ifelse(X1 == 'hour_end', 24, X2),
                                                   X2 = ifelse(X1 == 'case_name', paste0('regional_grids_',year), X2),
                                                   X2 = ifelse(X1 == 'data_path', data_path, X2),
                                                   X2 = ifelse(X1 == 'output_path', output_path, X2))

word_numbers <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")


# now populate the demand for each region
for(rregion in demand_regions){
region_inputs_temp = 
  data.table(X1 = c(paste0('node_',rregion, '_demand')),
             X2 = c('demand'),
             X3 = c(paste0('node_', rregion)),
             X4 = c(''),
             X5 = c(paste0(word_numbers[delta_t],'_hour_ENA_prototype_demand_unnormalized_srex',rregion,'.csv')),
             X8 = c(''),
             X9 = c(''))

# now tack this onto the end of the case_input file
case_input = bind_rows(case_input, region_inputs_temp)
}

# fill a row with empty entries
case_input[nrow(case_input) + 1,] = ''

# now populate the curtaiilmentfor each region
for(rregion in curtailment_regions){
  region_inputs_temp = 
    data.table(X1 = c(paste0('node_',rregion, '_curtailment')),
               X2 = c('curtailment'),
               X3 = c(paste0('node_', rregion)),
               X4 = c(''),
               X5 = c(''),
               X8 = c(''),
               X9 = c(''))
  
  # now tack this onto the end of the case_input file
  case_input = bind_rows(case_input, region_inputs_temp)
}

# fill a row with empty entries
case_input[nrow(case_input) + 1,] = ''

# now populate the lost_load for each region
for(rregion in lost_load_regions){
  region_inputs_temp = 
    data.table(X1 = c(paste0('node_',rregion, '_lost_load')),
               X2 = c('lost_load'),
               X3 = c(''),
               X4 = c(paste0('node_', rregion)),
               X5 = c(''),
               X8 = c(''),
               X9 = c(as.character(lost_load_var_cost)))
  
  # now tack this onto the end of the case_input file
  case_input = bind_rows(case_input, region_inputs_temp)
}

# fill a row with empty entries
case_input[nrow(case_input) + 1,] = ''


# now populate the wind for each region
for(rregion in wind_regions){
  region_inputs_temp = 
    data.table(X1 = c(paste0('node_',rregion, c('_wind'))),
               X2 = c('fixed_generator'),
               X3 = c(''),
               X4 = c(paste0('node_', rregion)),
               X5 = c(paste0(word_numbers[delta_t],'_hour_', c('wind'),'_srex',rregion,'.csv')),
               X8 = c(as.character(wind_fixed_cost)),
               X9 = c(''))
  
  # now tack this onto the end of the case_input file
  case_input = bind_rows(case_input, region_inputs_temp)
}

# fill a row with empty entries
case_input[nrow(case_input) + 1,] = ''

# now populate the winsolard for each region
for(rregion in solar_regions){
  region_inputs_temp = 
    data.table(X1 = c(paste0('node_',rregion, c('_solar'))),
               X2 = c('fixed_generator'),
               X3 = c(''),
               X4 = c(paste0('node_', rregion)),
               X5 = c(paste0(word_numbers[delta_t],'_hour_', c('solar'),'_srex',rregion,'.csv')),
               X8 = c(as.character(solar_fixed_cost)),
               X9 = c(''))
  
  # now tack this onto the end of the case_input file
  case_input = bind_rows(case_input, region_inputs_temp)
}

# fill a row with empty entries
case_input[nrow(case_input) + 1,] = ''

# now populate the winsolard for each region
for(rregion in storage_regions){
  region_inputs_temp = 
    data.table(X1 = c(paste0('node_',rregion, c('_storage'))),
               X2 = c('storage'),
               X3 = c(paste0('node_', rregion)),
               X4 = c(paste0('node_', rregion)),
               X5 = c(''),
               X8 = c(as.character(storage_fixed_cost)),
               X9 = c(''),
               X10 = c(as.character(storage_charging_time)),
               X11 = c(as.character(storage_efficiency)),
               X12 = c(as.character(storage_decay_rate)))
  
  # now tack this onto the end of the case_input file
  case_input = bind_rows(case_input, region_inputs_temp)
}

# fill a row with empty entries
case_input[nrow(case_input) + 1,] = ''

# now add the row that ends the input sheet
case_input[nrow(case_input) + 1,c('X1')] = 'END_CASE_DATA'


write.table(case_input, 
            file = save_file_name, 
            quote = FALSE,
            sep = ",",
            row.names = FALSE,
            na="")


rm(list = ls())
}
                         