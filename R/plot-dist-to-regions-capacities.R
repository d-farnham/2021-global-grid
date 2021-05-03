plot_dist_to_regions_capacities = function(
proposed_grid = 'data/proposed_grid_coords.RData',
years = c(2016:2018),
global_grid_run_ID = 'connected_five_storage'){
  
  

load(file = proposed_grid)
  
  
dist_calc = merge(proposed_grid_coords[c(seq(1, nrow(proposed_grid_coords)-1, by = 2)),],
                  proposed_grid_coords[c(seq(2, nrow(proposed_grid_coords), by = 2)),],
                  by = 'wire_IDs')
  
# find the distance of these wires
for(ii in 1:nrow(dist_calc)){
    dist_calc$dist[ii] = distGeo(c(dist_calc$center_lon.x[ii], dist_calc$center_lat.x[ii]), c(dist_calc$center_lon.y[ii], dist_calc$center_lat.y[ii]))/1000
}
  
  
trans_lengths = dist_calc %>% dplyr::select(SREX_id.x, SREX_id.y, dist) %>%
                              setNames(c('node_from', 'node_to', 'length'))

## Weighted shortest paths

el <- as.matrix(trans_lengths)
g2 <- as.undirected(add_edges(make_empty_graph(26), t(el[,1:2]), weight=el[,3]))

# function to compute the shortest length between 
# one regional node to all other regional nodes
source('R/path-length-from-specific-region.R')



dist_to_regions = merge(merge(merge(merge(path_length_from_specific_region(g2, 22,26, 'dist_to_EAS'),
                                          path_length_from_specific_region(g2, 12,26, 'dist_to_CEU'),
                                          by = 'srex'),
                                    path_length_from_specific_region(g2, 23,26, 'dist_to_SAS'),
                                    by = 'srex'),
                              path_length_from_specific_region(g2, 3,26, 'dist_to_WNA'),
                              by = 'srex'),
                        path_length_from_specific_region(g2, 5,26, 'dist_to_ENA'),
                        by = 'srex')


dist_cap_plot = list()

# now load the installed capacities
if(length(years) != 3){print('Sorry, this function is designed to plot three years of results.')}
for(ii in 1:length(years)){
  yyear = c(years)[ii]

  
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


dist_to_regions_capacities = merge(dist_to_regions,
                                   wind_solar_capacities,
                                   by = 'srex',
                                   all.y = TRUE)


# rename the 'dist_to_srex_ variables and add in the % demand for each of the srex regions
srex_demands0 = read_csv('data/SREX_demand_weightings.csv') %>% setNames(c('srex', 'demand')) %>%
                                                                dplyr::mutate(total_demand = sum(demand),
                                                                              percent_demand = demand/total_demand * 100)

load(file = 'data/SREX_region_nodes_manual.RData')

srex_demands = merge(srex_demands0,
                     SREX_region_nodes_manual %>% dplyr::select(SREX_id, LAB) %>% unique() %>% setNames(c('srex', 'srex_name')),
                     by = 'srex') %>%
               dplyr::select(srex_name, percent_demand)



dist_to_regions_capacities_long = dist_to_regions_capacities %>% data.table() %>% 
                                                                 melt(id.vars = c('srex', 'Tech', 'Capacity'),
                                                                      value.name = 'dist')

dist_to_regions_capacities_long = dist_to_regions_capacities_long %>% dplyr::mutate(srex_dist = sub(x = variable,
                                                                                                    pattern = 'dist_to_',
                                                                                                    replacement = ''))

grid_locs_caps_dist_plot_data = merge(dist_to_regions_capacities_long %>% data.table() %>%
                                                                          melt(id.vars = c('srex', 'srex_dist', 'Tech', 'Capacity', 'dist')),
                                      srex_demands,
                                      by.x = 'srex_dist',
                                      by.y = 'srex_name') %>%
  dplyr::mutate(srex_dist_label = paste0(srex_dist,
                                         ' (', 
                                         round(percent_demand,0),
                                         '% of total demand)'))
# reorder the labels based on percent_demand
grid_locs_caps_dist_plot_data$srex_dist_label = factor(x = grid_locs_caps_dist_plot_data$srex_dist_label, 
                                                       levels = unique(grid_locs_caps_dist_plot_data$srex_dist_label[order(-grid_locs_caps_dist_plot_data$percent_demand)]), 
                                                       ordered = TRUE)


# reorder the labels based on percent_demand
grid_locs_caps_dist_plot_data$srex_dist_label = factor(x = grid_locs_caps_dist_plot_data$srex_dist_label, 
                                                       levels = unique(grid_locs_caps_dist_plot_data$srex_dist_label[order(-grid_locs_caps_dist_plot_data$percent_demand)]), 
                                                       ordered = TRUE)

# add in srex_name for labeling purposes
plot_data0 = merge(grid_locs_caps_dist_plot_data,
                  SREX_region_nodes_manual %>% dplyr::select(SREX_id, LAB) %>% setNames(c('srex', 'srex_name')) %>% unique(),
                  by = 'srex')

# mark the top 5 wind and top 5 solar generating regions
top_cap_regions = plot_data0 %>% dplyr::group_by(Tech) %>% dplyr::top_frac(5/26, Capacity) %>% dplyr::mutate(top_5 = 'yyy')

not_top_cap_regions = plot_data0 %>% dplyr::group_by(Tech) %>% dplyr::top_frac(21/26, -Capacity) %>% dplyr::mutate(top_5 = 'zzz')

plot_data = bind_rows(top_cap_regions,
                      not_top_cap_regions)

dist_cap_plot[[ii]] = 
  ggplot(plot_data) +
  geom_point(aes(dist/1000, Capacity/1e6, col = Tech),
             size = 2.0,
             alpha = 0.75) +
  geom_text_repel(data = plot_data %>% dplyr::filter(srex_name %in% c('EAS', 'SAS',
                                                                      'WSA', 'SSA', 'SEA') |
                                                       top_5 == 'yyy'),
            aes(dist/1000, Capacity/1e6, label = srex_name, colour = top_5),
            size = 1.75,
            max.overlaps = 25) +
  scale_color_manual(name = 'Generation type',
                     values = c('#f16913', '#238b45','red','black'),
                     breaks = c('wind', 'solar')) +
  theme_bw() +
  labs(title = paste0(letters[ii],') ', yyear),
       x = 'Shortest path to high demand center (1000 km)',
       y = 'Installed Capacity (GW)') +
  theme(legend.position = 'bottom') +
  facet_grid( ~ srex_dist_label,
             scales = 'free_y')
}


ggdraw() +
  draw_plot(dist_cap_plot[[1]], x = 0, y = 2/3, width = 1, height = 1/3) +
  draw_plot(dist_cap_plot[[2]], x = 0, y = 1/3, width = 1, height = 1/3) +
  draw_plot(dist_cap_plot[[3]], x = 0, y = 0, width = 1, height = 1/3) +
  ggsave(filename = 'figs/figure_distance_to_regions_capacities.pdf',
         device = 'pdf',
         width = 12,
         height = 11)

}

