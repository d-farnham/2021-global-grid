extract_trans_dispatch_outputs = function(year = 2018,
                                          gg_output_file = 'connected_five_storage/global_grid_'){

  
# let's load the results from the globally connected grid
output_path = list.files(paste0('MEM/Output_Data/global_grid/',gg_output_file,year),
                         pattern = '.xlsx',
                         recursive = TRUE,
                         full.names = TRUE) 
  

# extract the transmission levels
trans_dispatch_gg0 = read_excel(path = output_path, 
                                sheet = "time results",
                                .name_repair = 'minimal') %>% 
  dplyr::select(contains(match = 'trans')) %>% 
  dplyr::select(contains(match = 'dispatch')) %>%
  dplyr::mutate(time_index = 1:n()) %>%
  reshape2::melt(id.vars = 'time_index',
                 variable.name = 'Name',
                 value.name = 'dispatch') %>%
  dplyr::mutate(Node_A = str_split_fixed(Name, '_', 5)[,2],
                Node_B = str_split_fixed(Name, '_', 5)[,4],
                IN = grepl(" in ", Name),
                REVERSE = grepl(" reverse ", Name)) %>%
  dplyr::filter(!IN) %>% # exclude the power that goes into transmission and leave transmissoin that comes out
  dplyr::mutate(Tech = 'transmission') %>%
  dplyr::select(time_index, Node_A, Node_B, Tech, REVERSE, dispatch) %>%
  dplyr::mutate(dispatch = ifelse(REVERSE, -dispatch, dispatch)) %>%
  dplyr::group_by(time_index, Node_A, Node_B, Tech) %>%
  dplyr::summarise(dispatch = sum(dispatch))

# extract the number of time periods and hours per timestep
time_parameters_gg = read_excel(path = output_path, 
                                sheet = "case input",
                                .name_repair = 'minimal') %>%
  setNames(c('Index', 'Variable', 'Value')) %>%
  dplyr::filter(Variable == 'year_start' |
                Variable == 'month_start'  |
                Variable == 'day_start' |
                Variable == 'hour_start'  |
                Variable == 'num_time_periods' |
                Variable == 'delta_t') %>%
  dplyr::select(Variable, Value) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  dplyr::mutate(case = 'Global grid')

time_transform = data.frame(time_index = 1:time_parameters_gg$num_time_periods,
                            date_time = seq.POSIXt(from = as.POSIXlt(paste0(time_parameters_gg$year_start,'-',
                                                                            time_parameters_gg$month_start,'-',
                                                                            time_parameters_gg$day_start,' ',
                                                                            '00:00'),
                                                                     tz = 'UTC') + 3600 * as.numeric(time_parameters_gg$delta_t) / 2,
                                                   by = 3600 * as.numeric(time_parameters_gg$delta_t),
                                                   length.out = as.numeric(time_parameters_gg$num_time_periods)))


trans_dispatch_gg = trans_dispatch_gg0 %>% merge(time_transform,
                                                 by = 'time_index') %>%
                                           dplyr::select(date_time, Node_A, Node_B, dispatch)


return(trans_dispatch_gg)}