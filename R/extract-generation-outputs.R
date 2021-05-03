extract_generation_outputs = function(year = 2018,
                                      gg_output_file = 'connected_five_storage/global_grid_',
                                      rg_output_file = 'separate_part_'){
  
# let's load the results from the globally connected grid
output_path = list.files(paste0('MEM/Output_Data/global_grid/',gg_output_file,year),
                         pattern = '.xlsx',
                         recursive = TRUE,
                         full.names = TRUE) 
  
# extract the capacities for wind and solar
wind_solar_store_capacities = read_excel(path = output_path, 
                                         sheet = "tech results",
                                         .name_repair = 'minimal') %>%
  setNames(c('ID', 'Name', 'capacity')) %>% 
  dplyr::select(-ID) %>%
  dplyr::filter(grepl("solar|wind", Name)) %>%
  dplyr::filter(grepl("capacity", Name)) %>%
  tidyr::separate(Name, c('X', 'node', 'Tech'), "_") %>%
  dplyr::mutate(Tech = sub(x = Tech, replacement = '', pattern = ' capacity')) %>%
  dplyr::select(-X)


# extract the capacity factors for wind and solar
wind_solar_mean_generation = read_excel(path = output_path, 
                                        sheet = "time results",
                                        .name_repair = 'minimal') %>%
  dplyr::select(matches('solar|wind')) %>% 
  dplyr::select(contains('potential')) %>%
  dplyr::mutate(time_index = 1:n()) %>%
  data.table() %>%
  melt(id.vars = 'time_index',
       variable.name = 'Name',
       value.name = 'generation') %>%
  tidyr::separate(Name, c('X', 'node', 'Tech'), "_") %>%
  dplyr::mutate(Tech = sub(Tech, pattern = ' potential', replacement = '')) %>%
  dplyr::select(time_index, node, Tech, generation) %>%
  dplyr::group_by(node, Tech) %>%
  dplyr::summarise(mean_generation = mean(generation))


generation_outputs_gg = wind_solar_mean_generation %>% merge(wind_solar_store_capacities,
                                                             by = c('node', 'Tech')) %>%
                                                       dplyr::mutate(case = 'Global grid')






# now repeat this for the regional grids
output_paths = list.files(paste0('MEM/Output_Data/regional_grids'),
                          pattern = '.xlsx',
                          recursive = TRUE,
                          full.names = TRUE) 

output_paths = output_paths[grepl(output_paths, pattern = paste0(rg_output_file))]

output_paths = output_paths[grepl(output_paths, pattern = paste0('regional_grids_',year))]


for(ii in 1:length(output_paths)){
  # extract the capacities for wind and solar
  wind_solar_store_capacities0 = read_excel(path = output_paths[ii], 
                                           sheet = "tech results",
                                           .name_repair = 'minimal') %>%
    setNames(c('ID', 'Name', 'capacity')) %>% 
    dplyr::select(-ID) %>%
    dplyr::filter(grepl("solar|wind", Name)) %>%
    dplyr::filter(grepl("capacity", Name)) %>%
    tidyr::separate(Name, c('X', 'node', 'Tech'), "_") %>%
    dplyr::mutate(Tech = sub(x = Tech, replacement = '', pattern = ' capacity')) %>%
    dplyr::select(-X)
  
  
  # extract the capacity factors for wind and solar
  wind_solar_mean_generation0 = read_excel(path = output_paths[ii], 
                                          sheet = "time results",
                                          .name_repair = 'minimal') %>%
    dplyr::select(matches('solar|wind')) %>% 
    dplyr::select(contains('potential')) %>%
    dplyr::mutate(time_index = 1:n()) %>%
    data.table() %>%
    melt(id.vars = 'time_index',
         variable.name = 'Name',
         value.name = 'generation') %>%
    tidyr::separate(Name, c('X', 'node', 'Tech'), "_") %>%
    dplyr::mutate(Tech = sub(Tech, pattern = ' potential', replacement = '')) %>%
    dplyr::select(time_index, node, Tech, generation) %>%
    dplyr::group_by(node, Tech) %>%
    dplyr::summarise(mean_generation = mean(generation))
  
  
  generation_outputs_rg0 = wind_solar_mean_generation0 %>% merge(wind_solar_store_capacities0,
                                               by = c('node', 'Tech')) %>%
    dplyr::mutate(case = 'Regional grids')
  
  if(ii == 1){generation_outputs_rg = generation_outputs_rg0}
  if(ii > 1){generation_outputs_rg = rbind(generation_outputs_rg,
                                        generation_outputs_rg0)}
}


generation_outputs = bind_rows(generation_outputs_gg,
                            generation_outputs_rg)


return(generation_outputs)}
