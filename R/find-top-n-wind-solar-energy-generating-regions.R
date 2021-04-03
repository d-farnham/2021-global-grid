find_top_n_wind_solar_energy_generating_regions = function(
n_sites = 5,
MEM_output_file = 'data/Output_Data/five_stor_final/global_grid_case_2016/global_grid_case_2016_20201203-121418.xlsx'){
  
  
# load the MEM tech results
MEM_results = read_excel(path = MEM_output_file, 
                         sheet = "tech results") %>% setNames(c('ID', 'Name', 'Capacity')) %>% 
                                                     dplyr::select(-ID)
  
# let's extract all of the capacities
MEM_results_capacities = MEM_results[grepl("capacity",MEM_results$Name),]
  
# let's first extract wind, solar, and storage capacities
MEM_wind_solar0 = MEM_results_capacities[grepl("solar|wind",MEM_results_capacities$Name),]
  
# now separate the 'Name' column into a column for tech and a column for SREX region ID
MEM_wind_solar = MEM_wind_solar0 %>% dplyr::mutate(Name = gsub(pattern = ' capacity', gsub(pattern = 'node_', x = Name, replacement = ''), replacement = '')) %>% 
                                     tidyr::separate(Name, c('srex', 'Tech'), "_")  

  
MEM_solar = MEM_wind_solar %>% dplyr::filter(Tech == 'solar') %>%
                               dplyr::arrange(desc(Capacity))
  
MEM_wind = MEM_wind_solar %>% dplyr::filter(Tech == 'wind') %>%
                              dplyr::arrange(desc(Capacity))

return_this = data.frame(top_solar = MEM_solar$srex[1:n_sites],
                         top_wind = MEM_wind$srex[1:n_sites])

return(return_this)
}
