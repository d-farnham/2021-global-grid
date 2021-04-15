extract_trans_outputs = function(year = 2018,
                                 gg_output_file = 'connected_five_storage/global_grid_'){

  
# let's load the results from the globally connected grid
output_path = list.files(paste0('MEM/Output_Data/global_grid/',gg_output_file,year),
                         pattern = '.xlsx',
                         recursive = TRUE,
                         full.names = TRUE) 
  

# extract the transmission levels
trans_dispatch_gg = read_excel(path = output_path, 
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
  dplyr::group_by(time_index, Node_A, Node_B, Tech) %>%
  dplyr::summarise(max_dispatch = max(dispatch),
                   min_dispatch = min(dispatch)) %>%
  dplyr::mutate(dispatch = max_dispatch - min_dispatch) %>%
  dplyr::group_by(Node_A, Node_B, Tech) %>%
  dplyr::summarise(mean_dispatch = mean(dispatch))
  

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


trans_outputs_gg = trans_dispatch_gg %>% merge(trans_capacities_gg,
                                         by = c('Node_A', 'Node_B')) %>%
                                         dplyr::mutate(case = 'Global grid')

trans_outputs = bind_rows(trans_outputs_gg,
                          data.frame(case = 'Regional grids',
                                     Tech = 'transmission'))

return(trans_outputs)}