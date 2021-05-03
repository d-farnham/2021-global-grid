extract_tech_costs_parameters = function(year = 2018,
                                         gg_output_file = 'connected_five_storage/global_grid_',
                                         rg_output_file = 'separate_part_'){
  
# let's load the results from the globally connected grid
output_path = list.files(paste0('MEM/Output_Data/global_grid/',gg_output_file,year),
                         pattern = '.xlsx',
                         recursive = TRUE,
                         full.names = TRUE) 

tech_inputs = read_excel(path = output_path, 
                         sheet = "tech input",
                         .name_repair = 'minimal') 

colnames(tech_inputs)[1] = 'Index'

# extract solar, wind, and storage fixed costs
wind_solar_store_fixed_costs = tech_inputs %>% dplyr::filter(grepl("solar|wind|storage",tech_name)) %>%
  tidyr::separate(tech_name, c('X', 'node', 'Tech'), '_') %>%
  dplyr::select(Tech, fixed_cost) %>%
  unique()

# extract transmission costs and parameters
trans_costs_parameters = tech_inputs %>% dplyr::filter(tech_type == 'transmission') %>%
  tidyr::separate(tech_name, c('A','Node_A', 'B', 'Node_B', 'Tech'), '_') %>%
  dplyr::mutate(Tech = 'transmission') %>%
  dplyr::select(Tech, Node_A, Node_B, length, fraction_land, 
                line_cost_land, line_cost_sea, converter_pair_cost) %>%
  unique()

# extract lost load costs
lost_load_costs = tech_inputs %>% dplyr::filter(tech_type == 'lost_load') %>%
  dplyr::mutate(Tech = tech_type) %>%
  dplyr::select(Tech, var_cost) %>%
  unique()


tech_cost_parameters_gg = bind_rows(trans_costs_parameters,
                                    wind_solar_store_fixed_costs,
                                    lost_load_costs) %>%
                          dplyr::mutate(case = 'Global grid')


# now repeat this for the regional grids
output_paths = list.files(paste0('MEM/Output_Data/regional_grids'),
                          pattern = '.xlsx',
                          recursive = TRUE,
                          full.names = TRUE) 

output_paths = output_paths[grepl(output_paths, pattern = paste0(rg_output_file))]

output_paths = output_paths[grepl(output_paths, pattern = paste0('regional_grids_',year))]


for(ii in 1:length(output_paths)){
  tech_inputs = read_excel(path = output_paths[ii], 
                           sheet = "tech input",
                           .name_repair = 'minimal') 
  
  colnames(tech_inputs)[1] = 'Index'
  
  # extract solar, wind, and storage fixed costs
  wind_solar_store_fixed_costs = tech_inputs %>% dplyr::filter(grepl("solar|wind|storage",tech_name)) %>%
    tidyr::separate(tech_name, c('X', 'node', 'Tech'), '_') %>%
    dplyr::select(Tech, fixed_cost) %>%
    unique()
  
  
  # extract lost load costs
  lost_load_costs = tech_inputs %>% dplyr::filter(tech_type == 'lost_load') %>%
    dplyr::mutate(Tech = tech_type) %>%
    dplyr::select(Tech, var_cost) %>%
    unique()
  
  
  tech_cost_parameters_rg0 = bind_rows(wind_solar_store_fixed_costs,
                                      lost_load_costs) %>%
                             dplyr::mutate(case = 'Regional grids')
  
  if(ii == 1){tech_cost_parameters_rg = tech_cost_parameters_rg0}
  if(ii > 1){tech_cost_parameters_rg = rbind(tech_cost_parameters_rg,
                                             tech_cost_parameters_rg0)}
}


tech_cost_parameters = bind_rows(tech_cost_parameters_gg,
                                 tech_cost_parameters_rg %>% unique())

return(tech_cost_parameters)}
