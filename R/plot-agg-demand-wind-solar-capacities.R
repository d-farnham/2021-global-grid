plot_agg_demand_wind_solar_capacities = function(
  hour_multiple = 3,
  years = c(2016:2018),
  prototype_SREX_ID = 'ENA',
  global_grid_run_ID = 'connected_five_storage'){
  
  solar_wind_agg_demand_cors_plot = list()
  solar_wind_demand_cors_plot = list()
  
  if(length(years) != 3){print('Sorry, this function is designed to plot three years of results.')}
  for(ii in 1:length(years)){
    yyear = c(years)[ii]
  
  # load the n-hour CFs and demand for that year
  for(ssrex in 1:26){
    
    word_numbers <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
    
    wind_CF0 = read_csv(file = paste0('MEM/Input_Data/',word_numbers[hour_multiple],'_hour_wind_srex',ssrex,'.csv'),
                        skip = 1) %>%
      dplyr::mutate(srex = ssrex) %>%
      dplyr::filter(year == yyear)
    
    
    solar_CF0 = read_csv(file = paste0('MEM/Input_Data/',word_numbers[hour_multiple],'_hour_solar_srex',ssrex,'.csv'),
                         skip = 1) %>%
      dplyr::mutate(srex = ssrex) %>%
      dplyr::filter(year == yyear)
    
    
    
    demand0 = read_csv(file = paste0('MEM/Input_Data/',word_numbers[hour_multiple],'_hour_',prototype_SREX_ID,'_prototype_demand_unnormalized_srex',ssrex,'.csv'),
                       skip = 1) %>%
      dplyr::mutate(srex = ssrex) %>%
      dplyr::filter(year == yyear)
    
    
    CF_demand0 = merge(merge(wind_CF0,
                             solar_CF0,
                             by = c('year', 'month', 'day', 'hour', 'srex')),
                       demand0,
                       by = c('year', 'month', 'day', 'hour', 'srex'))
    
    if(ssrex == 1){CF_demand = CF_demand0}
    if(ssrex > 1){CF_demand = bind_rows(CF_demand, CF_demand0)}
  }
  
  rm(wind_CF0, solar_CF0, demand0, CF_demand0)
    
CF_agg_demand = CF_demand %>% dplyr::group_by(year, month, day, hour) %>%
                        dplyr::mutate(agg_demand = sum(as.numeric(demand)))


CF_agg_demand_summary = CF_agg_demand %>% dplyr::group_by(srex) %>%
                                          dplyr::summarise(`solar-demand` = cor(solar_capacity, demand),
                                                           `wind-demand` = cor(wind_capacity, demand),
                                                           `solar-aggregate global demand` = cor(solar_capacity, agg_demand),
                                                           `wind-aggregate global demand` = cor(wind_capacity, agg_demand),
                                                           mean_demand = mean(demand))

# load the installed wind and solar capacities for a given year simulation
    
# let's load the results from the globally connected grid
output_path = list.files(paste0('MEM/Output_Data/global_grid/',global_grid_run_ID,'/global_grid_',yyear),
                         pattern = '.xlsx',
                         recursive = TRUE,
                         full.names = TRUE) 
    
# extract the capacities for wind and solar
wind_solar_capacities = read_excel(path = output_path, 
                                   sheet = "tech results",
                                   .name_repair = 'minimal') %>%
      setNames(c('ID', 'Name', 'Capacity')) %>% 
      dplyr::select(-ID) %>%
      dplyr::filter(grepl("solar|wind", Name)) %>%
      dplyr::filter(grepl("capacity", Name)) %>%
      tidyr::separate(Name, c('X', 'srex', 'Tech'), "_") %>%
      dplyr::mutate(Tech = sub(x = Tech, replacement = '', pattern = ' capacity')) %>%
      dplyr::select(-X)
    
    
capacities_and_cors = merge(wind_solar_capacities,
                            CF_agg_demand_summary,
                            by = 'srex',
                            all.x = TRUE)

solar_wind_agg_demand_cors_plot[[ii]] = 
      ggplot() +
      geom_point(data = capacities_and_cors %>% dplyr::filter(Tech == 'solar'),
                 aes(`solar-aggregate global demand`, Capacity/1e6, col = Tech),
                 size = 4,
                 alpha = 0.75) +
  geom_point(data = capacities_and_cors %>% dplyr::filter(Tech == 'wind'),
             aes(`wind-aggregate global demand`, Capacity/1e6, col = Tech),
             size = 4,
             alpha = 0.75) +
      scale_color_manual(name = 'Generation type',
                         values = c('#f16913', '#238b45')) +
      labs(title = paste0(letters[ii],') ', yyear),
           x = 'Correlation with aggregate demand',
           y = ' Installed Capacity (GW)') +
      theme_classic()


solar_wind_demand_cors_plot[[ii]] = 
  ggplot() +
  geom_point(data = capacities_and_cors %>% dplyr::filter(Tech == 'solar'),
             aes(`solar-demand`, Capacity/mean_demand, col = Tech),
             size = 4,
             alpha = 0.75) +
  geom_point(data = capacities_and_cors %>% dplyr::filter(Tech == 'wind'),
             aes(`wind-demand`, Capacity/mean_demand, col = Tech),
             size = 4,
             alpha = 0.75) +
  scale_color_manual(name = 'Generation type',
                     values = c('#f16913', '#238b45')) +
  labs(title = paste0(letters[ii],') ', yyear),
       x = 'Correlation with regional demand',
       y = ' Installed Capacity / mean regional demand') +
  theme_classic()
    
  }
  
  
ggdraw() +
    draw_plot(solar_wind_agg_demand_cors_plot[[1]], x = 0, y = 2/3, width = 1, height = 1/3) +
    draw_plot(solar_wind_agg_demand_cors_plot[[2]], x = 0, y = 1/3, width = 1, height = 1/3) +
    draw_plot(solar_wind_agg_demand_cors_plot[[3]], x = 0, y = 0, width = 1, height = 1/3) +
    ggsave(filename = 'figs/figure_solar_wind_agg_demand_cors_vs_gen.pdf',
           device = 'pdf',
           width = 8,
           height = 15)
  
  
ggdraw() +
    draw_plot(solar_wind_demand_cors_plot[[1]], x = 0, y = 2/3, width = 1, height = 1/3) +
    draw_plot(solar_wind_demand_cors_plot[[2]], x = 0, y = 1/3, width = 1, height = 1/3) +
    draw_plot(solar_wind_demand_cors_plot[[3]], x = 0, y = 0, width = 1, height = 1/3) +
    ggsave(filename = 'figs/figure_solar_wind_demand_cors_vs_normed_gen.pdf',
           device = 'pdf',
           width = 8,
           height = 15)  
  
  
  
  
}