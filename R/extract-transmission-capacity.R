extract_trans_capacity = function(year = 2018,
                                  gg_output_file = 'connected_five_storage/global_grid_'){

  
# let's load the results from the globally connected grid
output_path = list.files(paste0('MEM/Output_Data/global_grid/',gg_output_file,year),
                         pattern = '.xlsx',
                         recursive = TRUE,
                         full.names = TRUE) 
  

# extract the capacities for transmission
trans_capacities_gg = read_excel(path = output_path, 
                              sheet = "tech results",
                              .name_repair = 'minimal') %>%
  setNames(c('ID', 'Name', 'capacity')) %>% 
  dplyr::select(-ID) %>%
  dplyr::filter(grepl("trans", Name)) %>%
  tidyr::separate(Name, c('X', 'Node_A', 'Y', 'Node_B', 'Tech'), "_") %>%
  dplyr::mutate(Tech = sub(x = Tech, replacement = '', pattern = ' capacity')) %>%
  dplyr::select(-X, -Y, -Tech) %>%
  dplyr::mutate(wire_ID = 1:n())


trans_capacities_gg = trans_capacities_gg %>% dplyr::mutate(case = 'Global grid')

trans_capacities = bind_rows(trans_capacities_gg,
                             data.frame(case = 'Regional grids')) %>%
                   dplyr::mutate(Tech = 'transmission')

return(trans_capacities)}
