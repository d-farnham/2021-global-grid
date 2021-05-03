plot_tail_dep_capacities = function(
  hour_multiple = 3,
  years = c(2016:2018),
  prototype_SREX_ID = 'ENA',
  global_grid_run_ID = 'connected_five_storage'){
  
  tail_dep_plot = list()

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
      dplyr::mutate(agg_demand = sum(as.numeric(demand))) %>%
      dplyr::select(-demand)
    
    
    CF_agg_demand_long = CF_agg_demand %>% data.table() %>%
                                           melt(id.vars = c('srex','year', 'month', 'day', 'hour', 'agg_demand'),
                                                variable.name = 'Tech',
                                                value.name = "Capacity") %>%
                                           dplyr::mutate(Tech = sub(x = Tech, pattern = '_capacity', replacement = ''))
    
    
    library(extRemes)
    
    # mark solar_CF as NAs so that these hours are not considered in the correlation between solar_CF and demand
    CF_agg_demand_long_tdep = CF_agg_demand_long %>% dplyr::arrange(year, month, day, hour) %>%
      dplyr::group_by(Tech, srex) %>%
      dplyr::summarise(tail_dep_95 = taildep(x = agg_demand, 
                                             y = Capacity, 
                                             u = 0.95,
                                             type = 'chi'),
                       tail_dep_97.5 = taildep(x = agg_demand, 
                                               y = Capacity, 
                                               u = 0.975,
                                               type = 'chi'),
                       tail_dep_99 = taildep(x = agg_demand, 
                                             y = Capacity, 
                                             u = 0.99,
                                             type = 'chi'))
    
    
    
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
    
    
capacities_and_tdeps = merge(wind_solar_capacities,
                             CF_agg_demand_long_tdep,
                             by = c('Tech','srex'))    
    
capacities_and_tdeps_long = capacities_and_tdeps %>% data.table() %>%
                                                     melt(id.vars = c('Tech', 'srex', 'Capacity'),
                                                          variable.name = 'tail_dep_type') %>%
  dplyr::mutate(tail_dep_type = sub(x = tail_dep_type,
                                    pattern = 'tail_dep_',
                                    replacement = '')) %>%
  dplyr::mutate(tail_dep_type_label = paste0('Top ',(100-as.numeric(tail_dep_type)),'%'))
    
tail_dep_plot[[ii]] = 
      ggplot(capacities_and_tdeps_long) +
      geom_point(aes(value, Capacity/1e6, col = Tech),
                 size = 2.5,
                 alpha = 0.75) +
      scale_color_manual(name = 'Generation type',
                         values = c('#f16913', '#238b45')) +
      labs(title = paste0(letters[ii],') ', yyear),
           x = 'Tail Dependence with aggregate demand',
           y = ' Installed Capacity (GW)') +
      theme_classic() +
      facet_wrap(~tail_dep_type_label)
  }  
  
  
ggdraw() +
    draw_plot(tail_dep_plot[[1]], x = 0, y = 2/3, width = 1, height = 1/3) +
    draw_plot(tail_dep_plot[[2]], x = 0, y = 1/3, width = 1, height = 1/3) +
    draw_plot(tail_dep_plot[[3]], x = 0, y = 0, width = 1, height = 1/3) +
    ggsave(filename = 'figs/figure_solar_wind_agg_demand_tail_dep.pdf',
           device = 'pdf',
           width = 10,
           height = 9)
  
}