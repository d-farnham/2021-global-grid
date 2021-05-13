plot_costs_alternative_scenarios = function(
years = c(2016:2018),
global_grid_run_ID = 'connected_five_storage',
storage_100_global_grid_run_ID = 'connected_five_one_hundred_cost_storage',
storage_1_10th_global_grid_run_ID = 'connected_five_one_tenth_cost_storage',
exclude_high_density_regions_global_grid_run_ID = 'connected_five_storage_exclude_top_5_wind_and_top_5_solar_density',
exclude_high_gen_regions_global_grid_run_ID = 'connected_five_storage_exclude_top_5_wind_and_top_5_solar_generating',
regional_grids_run_ID = 'separate',
storage_100_regional_grids_run_ID = 'separate_one_hundred_cost_storage',
storage_1_10th_regional_grids_run_ID = 'separate_one_tenth_cost_storage'){
  
for(yyear in years){

source('R/extract-yearly-system-cost.R')
yearly_costs =  
  extract_system_cost(year = yyear,
                      gg_output_file = paste0(global_grid_run_ID,'/global_grid_'),
                      rg_output_file = paste0(regional_grids_run_ID,'_part_')) 

# initialize the system cost data frame
system_costs = data.frame(run_ID = c(global_grid_run_ID, regional_grids_run_ID),
                          yearly_cost = c(yearly_costs$yearly_cost))

# let's load the results from the globally connected grid for storage costs of 100 $/kW
yearly_costs =  
  extract_system_cost(year = yyear,
                      gg_output_file = paste0(storage_100_global_grid_run_ID,'/global_grid_'),
                      rg_output_file = paste0(storage_100_regional_grids_run_ID,'_part_'))


system_costs = rbind(system_costs,
                     data.frame(run_ID = c(storage_100_global_grid_run_ID,storage_100_regional_grids_run_ID),
                                yearly_cost = c(yearly_costs$yearly_cost)))


# let's load the results from the globally connected grid for storage costs 1/10th of baseline
yearly_costs =  
  extract_system_cost(year = yyear,
                      gg_output_file = paste0(storage_1_10th_global_grid_run_ID,'/global_grid_'),
                      rg_output_file = paste0(storage_1_10th_regional_grids_run_ID,'_part_'))


system_costs = rbind(system_costs,
                     data.frame(run_ID = c(storage_1_10th_global_grid_run_ID,storage_1_10th_regional_grids_run_ID),
                                yearly_cost = c(yearly_costs$yearly_cost)))

# let's load the results from the globally connected grid w/o high density regions
yearly_costs =  
  extract_system_cost(year = yyear,
                      gg_output_file = paste0(exclude_high_density_regions_global_grid_run_ID,'/global_grid_'),
                      rg_output_file = 'separate_part_') %>% dplyr::filter(case == 'Global grid')


system_costs = rbind(system_costs,
                     data.frame(run_ID = exclude_high_density_regions_global_grid_run_ID,
                                yearly_cost = yearly_costs$yearly_cost))


# let's load the results from the globally connected grid w/o high gen regions
yearly_costs =  
  extract_system_cost(year = yyear,
                      gg_output_file = paste0(exclude_high_gen_regions_global_grid_run_ID,'/global_grid_'),
                      rg_output_file = 'separate_part_') %>% dplyr::filter(case == 'Global grid')


system_costs = rbind(system_costs,
                     data.frame(run_ID = exclude_high_gen_regions_global_grid_run_ID,
                                yearly_cost = yearly_costs$yearly_cost))

# compute and print the cost difference for no to 5 wind and solar density region gen
cost_comparison = system_costs %>% dplyr::filter(run_ID == global_grid_run_ID |
                                                 run_ID == exclude_high_density_regions_global_grid_run_ID) 

percent_dif = (cost_comparison %>% dplyr::filter(run_ID == exclude_high_density_regions_global_grid_run_ID) %>% dplyr::select(yearly_cost) - 
               cost_comparison %>% dplyr::filter(run_ID == global_grid_run_ID) %>% dplyr::select(yearly_cost))/
               cost_comparison %>% dplyr::filter(run_ID == global_grid_run_ID) %>% dplyr::select(yearly_cost)

print(paste0('The global grid for year ',
             yyear,
             ' is about ', 
             round(percent_dif*100,digits = 0),
             '% more expensive when the top 5 wind and top 5 solar density regions are excluded from generating electricity'))

# compute and print the cost difference for no to 5 wind and solar high gen regions
cost_comparison = system_costs %>% dplyr::filter(run_ID == global_grid_run_ID |
                                                   run_ID == exclude_high_gen_regions_global_grid_run_ID) 

percent_dif = (cost_comparison %>% dplyr::filter(run_ID == exclude_high_gen_regions_global_grid_run_ID) %>% dplyr::select(yearly_cost) - 
                 cost_comparison %>% dplyr::filter(run_ID == global_grid_run_ID) %>% dplyr::select(yearly_cost))/
  cost_comparison %>% dplyr::filter(run_ID == global_grid_run_ID) %>% dplyr::select(yearly_cost)

print(paste0('The global grid for year ',
             yyear,
             ' is about ', 
             round(percent_dif*100,digits = 0),
             '% more expensive when the top 5 wind and top 5 solar ggenerating regions from prior optimization are excluded from generating electricity'))

# prepare data for the reduced generating regions plot
reduced_generating_regions0 = 
  data.frame(scenario = c('all \n gen regions \n (baseline)',
                          'exclude \n high density \n gen regions',
                          'exclude \n high generating \n gen regions'),
             yearly_cost_global_grid = c(system_costs$yearly_cost[system_costs$run_ID == global_grid_run_ID],
                                         system_costs$yearly_cost[system_costs$run_ID == exclude_high_density_regions_global_grid_run_ID],
                                         system_costs$yearly_cost[system_costs$run_ID == exclude_high_gen_regions_global_grid_run_ID]),
             yearly_cost_regional_grids = c(system_costs$yearly_cost[system_costs$run_ID == regional_grids_run_ID],
                                            NA,
                                            NA),
             yearly_cost_difference_percent = c((system_costs$yearly_cost[system_costs$run_ID == regional_grids_run_ID] -
                                                 system_costs$yearly_cost[system_costs$run_ID == global_grid_run_ID]) /
                                                  system_costs$yearly_cost[system_costs$run_ID == regional_grids_run_ID] * 100,
                                                (system_costs$yearly_cost[system_costs$run_ID == regional_grids_run_ID] -
                                                 system_costs$yearly_cost[system_costs$run_ID == exclude_high_density_regions_global_grid_run_ID]) / 
                                                  system_costs$yearly_cost[system_costs$run_ID == regional_grids_run_ID] * 100,
                                                (system_costs$yearly_cost[system_costs$run_ID == regional_grids_run_ID] -
                                                 system_costs$yearly_cost[system_costs$run_ID == exclude_high_gen_regions_global_grid_run_ID]) /
                                                  system_costs$yearly_cost[system_costs$run_ID == regional_grids_run_ID] * 100),
             year = yyear)

# prepare data for the reduced storage cost plot
reduced_storage_cost_regions0 =
  data.frame(scenario = c('364 $/kWh \n (baseline based \n on EIA estimate)', 
                          '100 $/kWh', 
                          '36 $/kWh'),
             yearly_cost_global_grid = c(system_costs$yearly_cost[system_costs$run_ID == global_grid_run_ID],
                                         system_costs$yearly_cost[system_costs$run_ID == storage_100_global_grid_run_ID],
                                         system_costs$yearly_cost[system_costs$run_ID == storage_1_10th_global_grid_run_ID]),
             yearly_cost_regional_grids = c(system_costs$yearly_cost[system_costs$run_ID == regional_grids_run_ID],
                                            system_costs$yearly_cost[system_costs$run_ID == storage_100_regional_grids_run_ID],
                                            system_costs$yearly_cost[system_costs$run_ID == storage_1_10th_regional_grids_run_ID])) %>%
             dplyr::mutate(yearly_cost_difference_percent = (yearly_cost_regional_grids - yearly_cost_global_grid) / yearly_cost_regional_grids * 100,
                           year = yyear)

if(yyear == years[1]){reduced_generating_regions = reduced_generating_regions0}
if(yyear == years[1]){reduced_storage_cost_regions = reduced_storage_cost_regions0}

if(yyear > years[1]){reduced_generating_regions = rbind(reduced_generating_regions, reduced_generating_regions0)}
if(yyear > years[1]){reduced_storage_cost_regions = rbind(reduced_storage_cost_regions, reduced_storage_cost_regions0)}
}

# Construct and save 2 tables of the numeric values for the forthcoming figure
reduced_storage_table_data = reduced_storage_cost_regions %>% dplyr::mutate(yearly_cost_bil_global_grid = yearly_cost_global_grid/1e9,
                                                                            yearly_cost_bil_regional_grids = yearly_cost_regional_grids/1e9,
                                                                            yearly_cost_difference_percent = yearly_cost_difference_percent) %>%
  dplyr::select(scenario, year, yearly_cost_bil_regional_grids, yearly_cost_bil_global_grid, yearly_cost_difference_percent) %>%
  dplyr::mutate_at(vars(yearly_cost_bil_regional_grids, yearly_cost_bil_global_grid, yearly_cost_difference_percent), funs(round(., 0)))


reduced_storage_table_data %>% gt(auto_align = 'auto') %>%
  cols_label(scenario = md('Case'),
             year = md('Year'),
             yearly_cost_bil_global_grid = md('Global grid yearly cost (billion $)'),
             yearly_cost_bil_regional_grids = md('Regional grids yearly cost (billion $)'),
             yearly_cost_difference_percent = md('Cost reduction of global grid vs. regional grids (%)')) %>%
  cols_width(vars(scenario) ~ px(200),
             vars(year) ~ px(75),
             everything() ~ px(195)) %>%
  tab_style(style = list(cell_fill(color = "lightgray")),
            locations = cells_body(columns = c('yearly_cost_difference_percent', 'year'))) %>%
  opt_table_font(font = list(google_font(name = "Spectral"))) %>%
  gtsave(filename = paste0('table_reduced_storage_cost.png'),
          path = 'figs/')


reduced_generating_regions_table_data = reduced_generating_regions %>% dplyr::mutate(yearly_cost_bil_global_grid = yearly_cost_global_grid/1e9,
                                                                                     yearly_cost_bil_regional_grids = yearly_cost_regional_grids/1e9,
                                                                                     yearly_cost_difference_percent = yearly_cost_difference_percent) %>%
  dplyr::select(scenario, year, yearly_cost_bil_regional_grids, yearly_cost_bil_global_grid, yearly_cost_difference_percent) %>%
  dplyr::mutate_at(vars(yearly_cost_bil_regional_grids, yearly_cost_bil_global_grid, yearly_cost_difference_percent), funs(round(., 0)))


reduced_generating_regions_table_data %>% gt(auto_align = 'auto') %>%
  cols_label(scenario = md('Case'),
             year = md('Year'),
             yearly_cost_bil_global_grid = md('Global grid yearly cost (billion $)'),
             yearly_cost_bil_regional_grids = md('Regional grids yearly cost (billion $)'),
             yearly_cost_difference_percent = md('Cost reduction of global grid vs. regional grids (%)')) %>%
  cols_width(vars(scenario) ~ px(295),
             vars(year) ~ px(75),
             everything() ~ px(195)) %>%
  tab_style(style = list(cell_fill(color = "lightgray")),
            locations = cells_body(columns = c('yearly_cost_difference_percent', 'year'))) %>%
  opt_table_font(font = list(google_font(name = "Spectral"))) %>%
  gtsave(filename = paste0('table_reduced_generating_regions.png'),
         path = 'figs/')

# now make and save the figure

panel_labels1 = data.frame(year = years,
                          x = 3.5,
                          y = 5,
                          label = paste0('(',letters[1:length(years)],')'))

explainer_labels = data.frame(year = years[1],
                              x = 4.3,
                              y = c(25),
                              label = c('Global grid \n   is less costly \n than the \n regional grids'))

explainer_arrows = data.frame(year = years[1],
                              x = 3.75,
                              xend = 3.75,
                              y = c(0),
                              yend = c(30))

explainer_line = data.frame(year = years[1],
                            x = 3.7,
                            xend = 3.8,
                            y = 0,
                            yend = 0)

reduced_gen_regions_comparison_plot = 
  ggplot(reduced_generating_regions) +
  geom_col(aes(scenario, yearly_cost_difference_percent, fill = scenario),
               width = 0.75,
           position = 'dodge') +
  theme_classic() +
  scale_fill_manual(name = '',
                    values = c('#8c510a','#d8b365','#f6e8c3')) +
  theme(legend.position = '') +
  labs(title = 'Impact of excluding generating regions \n in global grid scenario',
       y = 'System cost reduction vs. regional grids (%)',
       x = '\n Where generation is allowed in the \n global grid scenario') +
  geom_hline(yintercept = 0,
             linetype = 'dashed') +
  geom_text(data = panel_labels1,
             aes(x = x, y = y, label = label),
             col="black",
             size = 5) +
  coord_cartesian(xlim = c(1, 3), clip = 'off') +
  geom_segment(data = explainer_arrows,
               aes(x = x, xend = xend, y = y, yend = yend),
               arrow=arrow(length=unit(0.3, "cm"))) +
  geom_segment(data = explainer_line,
               aes(x = x, xend = xend, y = y, yend = yend)) +
  geom_text(data = explainer_labels,
            aes(x = x, y = y, label = label),
            size = 3) +
  theme(plot.margin = unit(c(1,7.5,1,1), "lines")) +
  facet_wrap(~year, 
             nrow = 3)

#### now look at the cheap storage
reduced_storage_cost_regions = reduced_storage_cost_regions %>% dplyr::mutate(scenario = factor(scenario, levels = c('364 $/kWh \n (baseline based \n on EIA estimate)', '100 $/kWh', '36 $/kWh')))

panel_labels2 = data.frame(year = years,
                           x = 3.5,
                           y = 5,
                           label = paste0('(',letters[(length(years)+1):(2*length(years))],')'))

cheap_storage_comparison_plot = 
  ggplot(reduced_storage_cost_regions) +
  geom_col(aes(scenario, yearly_cost_difference_percent, fill = scenario),
           width = 0.75,
           position = 'dodge') +
  theme_classic() +
  scale_fill_manual(name = '',
                    values = c('#f1b6da','#de77ae','#c51b7d')) +
  theme(legend.position = '') +
  labs(x = '\n Storage capital cost',
       y = 'System cost reduction vs. regional grids (%)',
       title = "Impact of reduced storage capital \n cost in both scenarios") +
  geom_hline(yintercept = 0,
             linetype = 'dashed') +
  geom_text(data = panel_labels2,
            aes(x = x, y = y, label = label),
            col="black",
            size = 5) +
  coord_cartesian(xlim = c(1, 3), clip = 'off') +
  geom_segment(data = explainer_arrows,
               aes(x = x, xend = xend, y = y, yend = yend),
               arrow=arrow(length=unit(0.3, "cm"))) +
  geom_segment(data = explainer_line,
               aes(x = x, xend = xend, y = y, yend = yend)) +
  geom_text(data = explainer_labels,
            aes(x = x, y = y, label = label),
            size = 3) +
  theme(plot.margin = unit(c(1,7.5,1,1), "lines")) +
  facet_wrap(~year, 
             nrow = 3)


ggdraw() +
  draw_plot(reduced_gen_regions_comparison_plot, x = 0, y = 0, width = 0.5, height = 1) +
  draw_plot(cheap_storage_comparison_plot, x = 0.5, y = 0.025, width = 0.5, height = 1 - 0.025) +
  ggsave(filename = 'figs/figure_storage_cost_and_generating_region_sensitivity.pdf',
         device = 'pdf',
         width = 10.5,
         height = 6.5)


rm(list = ls())
}

