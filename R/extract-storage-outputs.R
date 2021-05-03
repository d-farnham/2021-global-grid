extract_storage_outputs = function(year = 2018,
                                   gg_output_file = 'connected_five_storage/global_grid_',
                                   rg_output_file = 'separate_part_'){
  
# let's load the results from the globally connected grid
output_path = list.files(paste0('MEM/Output_Data/global_grid/',gg_output_file,year),
                         pattern = '.xlsx',
                         recursive = TRUE,
                         full.names = TRUE) 
  

# extract the storage levels
mean_stored = read_excel(path = output_path, 
                         sheet = "time results",
                         .name_repair = 'minimal') %>%
          dplyr::select(contains('storage')) %>%
          dplyr::select(contains('stored')) %>%
          dplyr::mutate(time_index = 1:n()) %>%
          data.table() %>%
          melt(id.vars = 'time_index',
               variable.name = 'Name',
               value.name = 'storage') %>%
          tidyr::separate(Name, c('X', 'node', 'storage stored'), "_") %>%
          dplyr::mutate(Tech = 'storage') %>%
          dplyr::select(time_index, node, Tech, storage) %>%
          dplyr::group_by(node, Tech) %>%
          dplyr::summarise(mean_stored = mean(storage))
  

# extract the capacities for storage
storage_capacities = read_excel(path = output_path, 
                                sheet = "tech results",
                                .name_repair = 'minimal') %>%
  setNames(c('ID', 'Name', 'capacity')) %>% 
  dplyr::select(-ID) %>%
  dplyr::filter(grepl("storage", Name)) %>%
  tidyr::separate(Name, c('X', 'node', 'Tech'), "_") %>%
  dplyr::mutate(Tech = sub(x = Tech, replacement = '', pattern = ' capacity')) %>%
  dplyr::select(-X)


storage_outputs_gg = mean_stored %>% merge(storage_capacities,
                                           by = c('node', 'Tech')) %>%
                                     dplyr::mutate(case = 'Global grid')


rm(list=setdiff(ls(), c('storage_outputs_gg','year','gg_output_file','rg_output_file')))


# now repeat this for the regional grids
output_paths = list.files(paste0('MEM/Output_Data/regional_grids'),
                          pattern = '.xlsx',
                          recursive = TRUE,
                          full.names = TRUE) 

output_paths = output_paths[grepl(output_paths, pattern = paste0(rg_output_file))]

output_paths = output_paths[grepl(output_paths, pattern = paste0('regional_grids_',year))]


for(ii in 1:length(output_paths)){
  mean_stored0 = read_excel(path = output_paths[ii], 
                           sheet = "time results",
                           .name_repair = 'minimal') %>%
    dplyr::select(contains('storage')) %>%
    dplyr::select(contains('stored')) %>%
    dplyr::mutate(time_index = 1:n()) %>%
    data.table() %>%
    melt(id.vars = 'time_index',
         variable.name = 'Name',
         value.name = 'storage') %>%
    tidyr::separate(Name, c('X', 'node', 'storage stored'), "_") %>%
    dplyr::mutate(Tech = 'storage') %>%
    dplyr::select(time_index, node, Tech, storage) %>%
    dplyr::group_by(node, Tech) %>%
    dplyr::summarise(mean_stored = mean(storage))
  
  
  storage_capacities0 = read_excel(path = output_paths[ii], 
                                  sheet = "tech results",
                                  .name_repair = 'minimal') %>%
    setNames(c('ID', 'Name', 'capacity')) %>% 
    dplyr::select(-ID) %>%
    dplyr::filter(grepl("storage", Name)) %>%
    tidyr::separate(Name, c('X', 'node', 'Tech'), "_") %>%
    dplyr::mutate(Tech = sub(x = Tech, replacement = '', pattern = ' capacity')) %>%
    dplyr::select(-X)
  
  
  storage_outputs_rg0 = mean_stored0 %>% merge(storage_capacities0,
                                             by = c('node', 'Tech')) %>%
    dplyr::mutate(case = 'Regional grids')
  
  if(ii == 1){storage_outputs_rg = storage_outputs_rg0}
  if(ii > 1){storage_outputs_rg = rbind(storage_outputs_rg,
                                 storage_outputs_rg0)}
}

storage_outputs = bind_rows(storage_outputs_gg,
                            storage_outputs_rg)


return(storage_outputs)}
