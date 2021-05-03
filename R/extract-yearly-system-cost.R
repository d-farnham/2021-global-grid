extract_system_cost = function(year = 2018,
                               gg_output_file = 'connected_five_storage/global_grid_',
                               rg_output_file = 'separate_part_'){
  
# let's load the results from the globally connected grid
output_path = list.files(paste0('MEM/Output_Data/global_grid/',gg_output_file,year),
                         pattern = '.xlsx',
                         recursive = TRUE,
                         full.names = TRUE) 
  
  
# extract the number of time periods and hours per timestep
time_parameters_gg = read_excel(path = output_path, 
                                sheet = "case input",
                                .name_repair = 'minimal') %>%
    setNames(c('Index', 'Variable', 'Value')) %>%
    dplyr::filter(Variable == 'num_time_periods' |
                    Variable == 'delta_t') %>%
    dplyr::select(Variable, Value) %>%
    pivot_wider(names_from = Variable, values_from = Value) %>%
    dplyr::mutate(case = 'Global grid')

# extract the system cost per hour
system_cost_gg = read_excel(path = output_path, 
                            sheet = "case results",
                            .name_repair = 'minimal') %>%
  setNames(c('Index', 'Variable', 'Value')) %>%
  dplyr::filter(Variable == 'system_cost') %>%
  dplyr::select(Variable, Value) %>%
  dplyr::mutate(Value = as.numeric(Value)) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  dplyr::mutate(case = 'Global grid')

# now repeat this for the regional grids
output_paths = list.files(paste0('MEM/Output_Data/regional_grids'),
                          pattern = '.xlsx',
                          recursive = TRUE,
                          full.names = TRUE) 

output_paths = output_paths[grepl(output_paths, pattern = paste0(rg_output_file))]

output_paths = output_paths[grepl(output_paths, pattern = paste0('regional_grids_',year))]


for(ii in 1:length(output_paths)){
  case_input0 = read_excel(path = output_paths[ii], 
                           sheet = "case input",
                           .name_repair = 'minimal')
  
  if(ii == 1){case_input = case_input0}
  if(ii > 1){case_input = rbind(case_input,
                                case_input0)}
  
  case_results0 = read_excel(path = output_paths[ii], 
                           sheet = "case results",
                           .name_repair = 'minimal')
  
  if(ii == 1){case_results = case_results0}
  if(ii > 1){case_results = rbind(case_results,
                                case_results0)}
}

# extract the number of time periods and hours per timestep
time_parameters_rg = case_input %>%
  setNames(c('Index', 'Variable', 'Value')) %>%
  dplyr::filter(Variable == 'num_time_periods' |
                  Variable == 'delta_t') %>%
  dplyr::select(Variable, Value) %>%
  unique() %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  dplyr::mutate(case = 'Regional grids')

# extract the system cost per hour
system_cost_rg = case_results %>%
  setNames(c('Index', 'Variable', 'Value')) %>%
  dplyr::filter(Variable == 'system_cost') %>%
  dplyr::select(Variable, Value) %>%
  dplyr::group_by(Variable) %>%
  dplyr::summarise(Value = sum(as.numeric(Value))) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  dplyr::mutate(case = 'Regional grids')


time_parameters = bind_rows(time_parameters_gg,
                            time_parameters_rg)

system_costs = bind_rows(system_cost_gg,
                         system_cost_rg)

yearly_system_costs = system_costs %>% merge(time_parameters,
                                             by = 'case') %>%
                                       dplyr::mutate(yearly_cost = system_cost * as.numeric(delta_t) * as.numeric(num_time_periods)) %>%
                                       dplyr::select(case, yearly_cost)


return(yearly_system_costs)}
