extract_curtailment_lostload_outputs = function(year = 2018,
                                                gg_output_file = 'connected_five_storage/global_grid_',
                                                rg_output_file = 'separate_part_'){
  
# let's load the results from the globally connected grid
output_path = list.files(paste0('MEM/Output_Data/global_grid/',gg_output_file,year),
                         pattern = '.xlsx',
                         recursive = TRUE,
                         full.names = TRUE) 
  
# extract the mean curtailments
mean_curtailment = read_excel(path = output_path, 
                              sheet = "time results",
                              .name_repair = 'minimal') %>%
  dplyr::select(contains('curtailment')) %>%
  data.table() %>%
  melt(variable.name = 'Name',
       value.name = 'dispatch') %>%
  tidyr::separate(Name, c('X', 'node', 'curtailment'), "_") %>%
  dplyr::mutate(Tech = 'curtailment') %>%
  dplyr::select(node, Tech, dispatch) %>%
  dplyr::group_by(node, Tech) %>%
  dplyr::summarise(mean_dispatch = mean(dispatch))

# extract the mean lost load
mean_lost_load = read_excel(path = output_path, 
                            sheet = "time results",
                            .name_repair = 'minimal') %>%
  dplyr::select(contains('lost_load')) %>%
  data.table() %>%
  melt(variable.name = 'Name',
       value.name = 'dispatch') %>%
  tidyr::separate(Name, c('X', 'node', 'lost', 'load'), "_") %>%
  dplyr::mutate(Tech = 'lost_load') %>%
  dplyr::select(node, Tech, dispatch) %>%
  dplyr::group_by(node, Tech) %>%
  dplyr::summarise(mean_lost_load_dispatch = mean(dispatch))


curtailment_lost_load_outputs_gg = mean_curtailment %>% bind_rows(mean_lost_load) %>%
                                                        dplyr::mutate(case = 'Global grid')


# now repeat this for the regional grids
output_paths = list.files(paste0('MEM/Output_Data/regional_grids'),
                          pattern = '.xlsx',
                          recursive = TRUE,
                          full.names = TRUE) 

output_paths = output_paths[grepl(output_paths, pattern = paste0(rg_output_file))]

output_paths = output_paths[grepl(output_paths, pattern = paste0('regional_grids_',year))]


for(ii in 1:length(output_paths)){
  # extract the mean curtailments
  mean_curtailment = read_excel(path = output_paths[ii], 
                                sheet = "time results",
                                .name_repair = 'minimal') %>%
    dplyr::select(contains('curtailment')) %>%
    data.table() %>%
    melt(variable.name = 'Name',
         value.name = 'dispatch') %>%
    tidyr::separate(Name, c('X', 'node', 'curtailment'), "_") %>%
    dplyr::mutate(Tech = 'curtailment') %>%
    dplyr::select(node, Tech, dispatch) %>%
    dplyr::group_by(node, Tech) %>%
    dplyr::summarise(mean_dispatch = mean(dispatch))
  
  # extract the mean lost load
  mean_lost_load = read_excel(path = output_paths[ii], 
                              sheet = "time results",
                              .name_repair = 'minimal') %>%
    dplyr::select(contains('lost_load')) %>%
    data.table() %>%
    melt(variable.name = 'Name',
         value.name = 'dispatch') %>%
    tidyr::separate(Name, c('X', 'node', 'lost', 'load'), "_") %>%
    dplyr::mutate(Tech = 'lost_load') %>%
    dplyr::select(node, Tech, dispatch) %>%
    dplyr::group_by(node, Tech) %>%
    dplyr::summarise(mean_lost_load_dispatch = mean(dispatch))
  
  
  curtailment_lost_load_outputs_rg0 = mean_curtailment %>% bind_rows(mean_lost_load) %>%
                                                           dplyr::mutate(case = 'Regional grids')
  
  if(ii == 1){curtailment_lost_load_outputs_rg = curtailment_lost_load_outputs_rg0}
  if(ii > 1){curtailment_lost_load_outputs_rg = rbind(curtailment_lost_load_outputs_rg,
                                                      curtailment_lost_load_outputs_rg0)}
}


curtailment_lost_load_outputs = bind_rows(curtailment_lost_load_outputs_gg,
                                          curtailment_lost_load_outputs_rg)

return(curtailment_lost_load_outputs)}
