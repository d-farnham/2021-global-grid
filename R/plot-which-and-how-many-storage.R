plot_which_and_how_many_storage = function(years = 2016:2018,
                                           gg_output_file = 'connected_five_storage/global_grid_'){
for(year in years){
# let's load the results from the globally connected grid -- functions require us to also load some regional grid results
#                                                            but we will not use these
source('R/extract-yearly-system-cost.R')
yearly_costs =  
  extract_system_cost(year = year,
                      gg_output_file = 'connected_five_storage/global_grid_',
                      rg_output_file = 'separate_part_') %>% dplyr::filter(case == 'Global grid')

which_regions_costs0 = data.frame(case = c('Global grid base case'),
                    year = year,
                    cost = yearly_costs$yearly_cost)

if(year == years[1]){which_regions_costs = which_regions_costs0}
if(year > years[1]){which_regions_costs = bind_rows(which_regions_costs,
                                     which_regions_costs0)}


# now load the five random storage runs
output_paths = data.frame(files = list.files(paste0('MEM/Output_Data/global_grid/assess_which_storage'),
                         pattern = '.xlsx',
                         recursive = TRUE,
                         full.names = TRUE))
            

output_paths = output_paths %>% dplyr::filter(grepl(files, pattern = year))


for(iiter in 1:nrow(output_paths)){
  
yearly_costs =  
    extract_system_cost(year = year,
                        gg_output_file = paste0('assess_which_storage/connected_five_random_storage_iter_',iiter,'/global_grid_'),
                        rg_output_file = 'separate_part_') %>% dplyr::filter(case == 'Global grid')  
  
which_regions_costs0 = data.frame(case = c(paste0('Global grid alt five storage iteration ',iiter)),
                                  year = year,
                                  cost = c(yearly_costs$yearly_cost))
  
which_regions_costs = bind_rows(which_regions_costs,
                                which_regions_costs0)
  
}
}

print(paste0('The following table shows the range of percent increase in cost above the base case of having storage in the five largest demand regions'))
print(
which_regions_costs %>% dplyr::group_by(year) %>% dplyr::mutate(least_cost = min(cost),
                                                                percent_dif = 100 * (cost - least_cost)/least_cost) %>%
                                                  dplyr::filter(case != 'Global grid base case') %>%
                                                  dplyr::summarise(`Minimum % difference` = min(percent_dif),
                                                                   `Maximum % difference` = max(percent_dif))
)  
  
which_storage_plot = 
    ggplot() +
    geom_boxplot(data = which_regions_costs %>% dplyr::filter(case != 'Global grid base case'),
                 aes(x = 0, y = cost/1e12),
                 coef = 5,
                 width = 0.75) +
    geom_hline(data = which_regions_costs %>% dplyr::filter(case == 'Global grid base case'),
               aes(yintercept = cost/1e12),
               linetype = 'dashed',
               col = 'red') +
    labs(title = "",
         x = '',
         y = 'System cost (trillion $ per year)') +  
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    scale_x_continuous(limits = c(-1,1)) +
    facet_wrap(~year,
               scales = 'free', 
               ncol = 3)

ggdraw() +
  draw_plot(which_storage_plot, x = 0, y = 0, width = 1, height = 1) +
  ggsave('figs/which_five_storage.pdf',
         device = 'pdf',
         width = 6,
         height = 3)




for(year in years){
# let's load the results from the globally connected grid -- functions require us to also load some regional grid results
#                                                            but we will not use these
source('R/extract-yearly-system-cost.R')
yearly_costs =  
    extract_system_cost(year = year,
                        gg_output_file = 'connected_five_storage/global_grid_',
                        rg_output_file = 'separate_part_') %>% dplyr::filter(case == 'Global grid')
  
  

  how_many_regions_costs0 = data.frame(case = c('Global grid 5 storage'),
                                     year = year,
                                     cost = c(yearly_costs$yearly_cost))
  
  if(year == years[1]){how_many_regions_costs = how_many_regions_costs0}
  if(year > years[1]){how_many_regions_costs = bind_rows(how_many_regions_costs,
                                                       how_many_regions_costs0)}
  
# now load the runs with different storage in differnet numbers of regions
  
for(nnum_stor in c(2:4,6:8)){
yearly_costs = 
    extract_system_cost(year,
                            gg_output_file = paste0('assess_how_many_storage/connected_',nnum_stor,'_storage/global_grid_'),
                            rg_output_file = 'separate_part_') %>% dplyr::filter(case == 'Global grid')
  

    how_many_regions_costs0 = data.frame(case = c(paste0('Global grid ', nnum_stor, ' storage')),
                                       year = year,
                                       cost = c(yearly_costs$yearly_cost))
    
    how_many_regions_costs = bind_rows(how_many_regions_costs,
                                       how_many_regions_costs0)
    
  }
}


how_many_regions_costs = how_many_regions_costs %>% dplyr::mutate(num_storage = sub(case, pattern = 'Global grid ', replacement = ''),
                                                                  num_storage = sub(num_storage, pattern = ' storage', replacement = ''),
                                                                  num_storage = as.numeric(num_storage))

five_storage = how_many_regions_costs %>% dplyr::filter(num_storage == 5) %>%
  dplyr::mutate(five_storage_cost = cost) %>%
  dplyr::select(year, five_storage_cost)

print(paste0('The following table shows the percent increase in cost above the base case of having storage in the five largest demand regions'))
print(
merge(how_many_regions_costs,
      five_storage,
      by = 'year') %>% dplyr::mutate(percent_dif_relative_to_5_storage = 100 * (cost - five_storage_cost)/five_storage_cost) %>%
                       dplyr::select(year, num_storage, cost, five_storage_cost, percent_dif_relative_to_5_storage) %>%
                       dplyr::arrange(year, num_storage)
)  


how_many_storage_plot = 
  ggplot() +
  geom_point(data = how_many_regions_costs,
             aes(num_storage, cost/1e12)) +
  geom_line(data = how_many_regions_costs,
             aes(num_storage, cost/1e12)) +
  geom_point(data = how_many_regions_costs %>% dplyr::filter(num_storage == 5),
            aes(num_storage, cost/1e12),
            col = 'red') +
  labs(x = 'Number of regions with storage',
       y = 'System cost (trillion $ per year)') +
  theme_bw() +
  facet_wrap(~year,
             scales = 'free', 
             ncol = 1)


ggdraw() +
  draw_plot(how_many_storage_plot, x = 0, y = 0, width = 1, height = 1) +
  ggsave('figs/how_many_storage.pdf',
         device = 'pdf',
         width = 4,
         height =6)

rm(list = ls())
}
