plot_cost_capacity = function(year,
                              gg_output_file,
                              rg_output_file){
    
# extract the storage results
source('R/extract-storage-outputs.R')
storage_outputs = 
  extract_storage_outputs(year,
                          gg_output_file,
                          rg_output_file)

# extract the transmission results
source('R/extract-transmission-outputs.R')
trans_outputs = 
  extract_trans_outputs(year,
                        gg_output_file)
  
# extract the wind/solar generation results
source('R/extract-generation-outputs.R')
generation_outputs = 
  extract_generation_outputs(year,
                             gg_output_file,
                             rg_output_file) 

# extract the curtailment results
source('R/extract-curtailment-lostload-outputs.R')
curtailment_lostload_outputs = 
  extract_curtailment_lostload_outputs(year,
                                       gg_output_file,
                                       rg_output_file) 



# make the gen bar plot
generation_outputs_summary = generation_outputs %>% dplyr::group_by(Tech, case) %>%
                                                    dplyr::summarise(mean_generation = sum(mean_generation),
                                                                     capacity = sum(capacity))
top_gen_lim = 
generation_outputs_summary %>% dplyr::group_by(case) %>%
                               dplyr::summarise(mean_generation = sum(mean_generation)) %>%
                               dplyr::select(mean_generation) %>%
                               max()/1e9

gen_plot = 
  ggplot() +
  geom_bar(data = generation_outputs_summary,
           aes(x = case, y = mean_generation/1e9, fill = Tech), 
           col = 'transparent',
           stat="identity", 
           width = 0.75,
           size = 0.25) +
  scale_fill_manual(values = c(as.character(my_colors$value[my_colors$color == 'green']),
                               as.character(my_colors$value[my_colors$color == 'orange']))) +
  theme_bw() +
  theme(legend.position = '') +
  labs(title = "Generation",
       y = 'Mean Generation (TW)',
       x = '') +
  scale_x_discrete(expand=expansion(mult = c(1.00,1.10))) +
  scale_y_continuous(expand = expansion( add = c(0,0.25)),
                     limits = c(0,top_gen_lim + 0.2))

# make the curtail bar plot
curtailment_lostload_outputs_summary = curtailment_lostload_outputs %>% dplyr::group_by(Tech, case) %>%
                                                                        dplyr::summarise(mean_dispatch = sum(mean_dispatch))

curtail_plot = 
  ggplot() +
  geom_bar(data = curtailment_lostload_outputs_summary %>% dplyr::filter(Tech == 'curtailment'),
           aes(x = case, y = mean_dispatch/1e9, fill = Tech), 
           col = 'transparent',
           stat="identity", 
           width = 0.75,
           size = 0.25) +
  scale_fill_manual(values = c(as.character(my_colors$value[my_colors$color == 'gray']))) +
  theme_bw() +
  theme(legend.position = '') +
  labs(title = "Curtailment",
       y = 'Mean Curtailment (TW)',
       x = '') +
  scale_x_discrete(expand=expansion(mult = c(1.00,1.10))) +
  scale_y_continuous(expand = expansion( add = c(0,0.25)),
                     limits = c(0,top_gen_lim + 0.2))


# make the storage bar plot
storage_outputs_summary = storage_outputs %>% dplyr::group_by(Tech, case) %>%
                                              dplyr::summarise(mean_stored = sum(mean_stored))

top_stor_lim = max(storage_outputs_summary$mean_stored, na.rm = TRUE)/1e9

storage_plot = 
  ggplot() +
  geom_bar(data = storage_outputs_summary,
           aes(x = case, y = mean_stored/1e9, fill = Tech), 
           col = 'transparent',
           stat="identity", 
           width = 0.75,
           size = 0.25) +
  scale_fill_manual(values = c(as.character(my_colors$value[my_colors$color == 'pink']))) +
  theme_bw() +
  theme(legend.position = '') +
  labs(title = "Intra-region Storage",
       y = 'Mean stored (TWh)',
       x = '') +
  scale_x_discrete(expand=expansion(mult = c(1.00,1.10))) +
  scale_y_continuous(expand = expansion( add = c(0,0.25)),
                     limits = c(0,top_stor_lim))


# make the transmission bar plot
trans_outputs_summary = trans_outputs %>% dplyr::group_by(Tech, case) %>%
                                          dplyr::summarise(mean_dispatch = sum(mean_dispatch))

top_trans_lim = max(trans_outputs_summary$mean_dispatch, na.rm = TRUE)/1e9

trans_plot = 
  ggplot() +
  geom_bar(data = trans_outputs_summary,
           aes(x = case, y = mean_dispatch/1e9, fill = Tech), 
           col = 'transparent',
           stat="identity", 
           width = 0.75,
           size = 0.25) +
  scale_fill_manual(values = c(as.character(my_colors$value[my_colors$color == 'black']))) +
  theme_bw() +
  theme(legend.position = '') +
  labs(title = 'Inter-region Transmissoin',
       y = 'Mean transmitted (TW)',
       x = '') +
  scale_x_discrete(expand=expansion(mult = c(1.00,1.10))) +
  scale_y_continuous(expand = expansion( add = c(0,0.25)),
                     limits = c(0,top_trans_lim))


# extract the costs of each of the elements
source('R/extract-tech-costs-parameters.R')
tech_costs_parameters = 
extract_tech_costs_parameters(year,
                              gg_output_file,
                              rg_output_file)

# extract the number of time periods and hours per timestep
source('R/extract-time-parameters.R')
time_parameters = 
extract_time_parameters(year,
                        gg_output_file,
                        rg_output_file)



# compute the costs of the generation
gen_costs = merge(merge(tech_costs_parameters %>% dplyr::filter(Tech %in% c('wind', 'solar')) %>%
                                                  dplyr::select(Tech, fixed_cost, case),
                        generation_outputs,
                        by = c('Tech','case'),
                        all = TRUE),
                  time_parameters,
                  by = c('case'),
                  all = TRUE) %>%
  dplyr::mutate(yearly_cost = as.numeric(capacity) * as.numeric(fixed_cost) * 
                              as.numeric(num_time_periods) * as.numeric(delta_t)) %>%
  dplyr::select(case, Tech, yearly_cost)

# compute the costs of storage
stor_costs = merge(merge(tech_costs_parameters %>% dplyr::filter(Tech %in% c('storage')) %>%
                          dplyr::select(Tech, fixed_cost, case),
                        storage_outputs,
                        by = c('Tech','case'),
                        all = TRUE),
                  time_parameters,
                  by = c('case'),
                  all = TRUE) %>%
  dplyr::mutate(yearly_cost = as.numeric(capacity) * as.numeric(fixed_cost) * 
                              as.numeric(num_time_periods) * as.numeric(delta_t)) %>%
  dplyr::select(case, Tech, yearly_cost)

# compute the costs of the transmission
trans_costs = merge(merge(tech_costs_parameters %>% dplyr::filter(Tech %in% c('transmission')),
                   trans_outputs,
                   by = c('Tech','Node_A','Node_B','case'),
                   all = TRUE),
             time_parameters,
             by = c('case'),
             all = TRUE) %>%
  dplyr::mutate(yearly_cost = (as.numeric(capacity) * as.numeric(converter_pair_cost) * 
                                 as.numeric(num_time_periods) * as.numeric(delta_t)) +# the cost of the convertor pair
                  (as.numeric(capacity) * as.numeric(line_cost_land) * as.numeric(length) * 
                     as.numeric(fraction_land) * as.numeric(num_time_periods) * as.numeric(delta_t)) +# the cost of land lines
                  (as.numeric(capacity) * as.numeric(line_cost_sea) * as.numeric(length) * 
                     (1 - as.numeric(fraction_land)) * as.numeric(num_time_periods) * as.numeric(delta_t))) %>%# the cost of submarine lines
  dplyr::select(case, Tech, yearly_cost)

# compute the "cost" of the lost load
lost_loads_costs = merge(merge(tech_costs_parameters %>% dplyr::filter(Tech %in% c('lost_load')) %>%
                                                         dplyr::select(Tech, var_cost, case),
                               curtailment_lostload_outputs %>% dplyr::filter(Tech %in% c('lost_load')),
                               by = c('Tech','case'),
                               all = TRUE),
                         time_parameters,
                         by = c('case'),
                         all = TRUE) %>%
    dplyr::mutate(yearly_cost = as.numeric(total_dispatch) * as.numeric(var_cost) * as.numeric(delta_t)) %>%
    dplyr::select(case, Tech, yearly_cost)


# combine costs and plot
all_costs = bind_rows(gen_costs,
                      stor_costs,
                      trans_costs,
                      lost_loads_costs)

all_costs_summary = all_costs %>% dplyr::group_by(case, Tech) %>%
                                  dplyr::summarise(yearly_cost = sum(yearly_cost))


all_costs_summary$Tech = factor(all_costs_summary$Tech, levels = c('lost_load','transmission','storage','solar','wind'))

# tech labels
tech_labels = data.frame(x = c(2.5, 2.5, 2.5, 2.5, -0.05),
                         y = c(0.5, 2, 2.55, 0.975 * sum(all_costs_summary$yearly_cost[all_costs_summary$case == 'Regional grids'], na.rm = TRUE)/1e12, 
                               0.85 * sum(all_costs_summary$yearly_cost[all_costs_summary$case == 'Global grid'], na.rm = TRUE)/1e12),
                         label = c('Wind', 'Solar', 'Storage', 'Unmet \n demand', 'Transmission'),
                         col = c('#f16913','#238b45','#e7298a','gray','#252525'))

cost_plot =
  ggplot(all_costs_summary) +
  geom_bar(aes(x = case, y = yearly_cost/1e12, fill = Tech),
           col = 'transparent',
           stat="identity", width = 0.75,
           size = 0.25) +
  scale_fill_manual(name = '',
                    values=c('gray','black','#e7298a','#f16913','#238b45'))+
  geom_text(data = tech_labels,
            aes(x = x, y = y, label = label, col = col),
            hjust = 'left',
            angle = 30) +
  scale_color_manual(name = '',
                     values=c('#f16913','black','#e7298a','#238b45','gray'))+
  theme_bw() +
  labs(title = "Cost",
       x = '',
       y = 'Yearly Costs (trillion $)') +
  theme(legend.position = '') +
  scale_x_discrete(expand=expansion(mult = c(1.25,1.45))) +
  scale_y_continuous(expand = expansion( add = c(0,0.25))) +
  coord_cartesian(clip = "off")



ggdraw() +
  draw_plot(cost_plot, x = 0, y = 0, width = 0.45, height = 1) +
  draw_label(label = '(a)', x = 0.425, y = 0.1, size = 20) +
  draw_plot(gen_plot, x = 0.5, y = 0.5, width = 0.25, height = 0.5) +
  draw_label(label = '(b)', x = 0.725, y = 0.6, size = 20) +
  draw_plot(curtail_plot, x = 0.75, y = 0.5, width = 0.25, height = 0.5) +
  draw_label(label = '(c)', x = 0.975, y = 0.6, size = 20) + 
  draw_plot(trans_plot, x = 0.5, y = 0, width = 0.25, height = 0.5) +
  draw_label(label = '(d)', x = 0.725, y = 0.1, size = 20) +
  draw_plot(storage_plot, x = 0.75, y = 0, width = 0.25, height = 0.5) +
  draw_label(label = '(e)', x = 0.975, y = 0.1, size = 20) + 
  ggsave(filename = paste0('figs/figure_cost_capacity_',year,'.pdf'),
         device = 'pdf',
         width = 13,
         height = 7)

rm(list = ls())
}