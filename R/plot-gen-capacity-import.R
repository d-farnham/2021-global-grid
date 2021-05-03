plot_wind_solar_capacity_import = function(year = 2018,
                                           gg_output_file = 'connected_five_storage/global_grid_'){
# let's load the results from the globally connected grid
output_path = list.files(paste0('MEM/Output_Data/global_grid/',gg_output_file,year),
                         pattern = '.xlsx',
                         recursive = TRUE,
                         full.names = TRUE) 


# extract the wind/solar generation results
source('R/extract-generation-outputs.R')
generation_outputs = 
  extract_generation_outputs(year,
                             gg_output_file,
                             rg_output_file = 'separate_part_') %>% dplyr::filter(case == 'Global grid') 

load('data/srex_polys_land.RData')

SREX_gen_cap_plot = merge(srex_polys_land,
                          generation_outputs,
                          by.x = 'srex_id',
                          by.y = 'node',
                          all = TRUE)

solar_max = ceiling(max(SREX_gen_cap_plot$capacity[SREX_gen_cap_plot$Tech == 'solar']/1e6) / 100) * 100

solar_breaks = round(seq(0, solar_max, solar_max/5),0)

world_outline = data.frame(xmin=(-180),
                           xmax=(180),
                           ymin=(-65),
                           ymax=(70))

load(file = 'data/SREX_region_nodes_manual.RData')

solar_plot = 
  ggplot() + 
  geom_polygon(data = SREX_gen_cap_plot %>% dplyr::filter(Tech == 'solar'),
               aes(long, lat, group = piece_LAB, fill = capacity/1e6),
               alpha = 1,
               col = 'transparent',
               size = 0) +
  scale_fill_distiller(name = 'Installed \n cap. (GW)',
                       breaks = c(solar_breaks[-1]), 
                       palette = "Oranges",
                       direction = 1) +
  geom_polygon(data = SREX_region_nodes_manual, 
               aes(long,lat,group=factor(LAB)), 
               alpha = 0, 
               col = 'white') +
  labs(title = 'Solar Generation',
       x = 'lon',
       y = 'lat') +
  coord_map(projection = 'mollweide',
            xlim = c(world_outline$xmin, world_outline$xmax),
            ylim = c(world_outline$ymin, world_outline$ymax - 6), 
            clip = "on") +
  theme_classic() +
  theme(axis.line = element_blank()) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position=c(0, 0.5),
        legend.justification = 'left',
        legend.key.size = unit(0.75, "cm"),
        axis.line = element_blank(),
        plot.title = element_text(size = 22),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) +
  geom_rect(data=world_outline, 
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
            color='gray', fill="transparent", size=1.25)

wind_max = ceiling(max(SREX_gen_cap_plot$capacity[SREX_gen_cap_plot$Tech == 'wind']/1e6) / 100) * 100

wind_breaks = round(seq(0, wind_max, wind_max/5),0)

wind_plot = 
  ggplot() + 
  geom_polygon(data = SREX_gen_cap_plot %>% dplyr::filter(Tech == 'wind'),
               aes(long, lat, group = piece_LAB, fill = capacity/1e6),
               alpha = 1,
               col = 'transparent',
               size = 0) +
  scale_fill_distiller(name = 'Installed \n cap. (GW)',
                       breaks = c(wind_breaks[-1]), 
                       palette = "Greens",
                       direction = 1) +
  geom_polygon(data = SREX_region_nodes_manual, 
               aes(long,lat,group=factor(LAB)), 
               alpha = 0, 
               col = 'white') +
  labs(title = 'Wind Generation',
       x = 'lon',
       y = 'lat') +
  coord_map(projection = 'mollweide',
            xlim = c(world_outline$xmin, world_outline$xmax),
            ylim = c(world_outline$ymin, world_outline$ymax - 6), 
            clip = "on") +
  theme_classic() +
  theme(axis.line = element_blank()) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position=c(0, 0.5),
        legend.justification = 'left',
        legend.key.size = unit(0.75, "cm"),
        axis.line = element_blank(),
        plot.title = element_text(size = 22),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) +
  geom_rect(data=world_outline, 
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
            color='gray', fill="transparent", size=1.25)


# extract the transmission capacities
source('R/extract-transmission-capacity.R')
trans_outputs = 
  extract_trans_capacity(year,
                         gg_output_file) %>% dplyr::filter(case == 'Global grid') 
  
trans_capacities = trans_outputs %>% dplyr::select(Node_A, Node_B, Tech, capacity, wire_ID) %>%
                                     data.table() %>%
                                     melt(id.vars = c('Tech', 'capacity', 'wire_ID'), value.name = 'SREX_id')


# prepare the in/out for plotting
source('R/extract-transmission-dispatch.R')
trans_dispatch_by_node =
           extract_trans_dispatch(year = year,
                                  gg_output_file = 'connected_five_storage/global_grid_')

trans_import_export = trans_dispatch_by_node %>% dplyr::mutate(import = mean_dispatch_into_node - mean_dispatch_out_of_node) %>%
                                                 dplyr::group_by(Node) %>%
                                                 dplyr::summarise(mean_import = mean(import))

trans_in_out_srex =  merge(srex_polys_land,
                           trans_import_export,
                           by.x = 'srex_id',
                           by.y = 'Node',
                           all = TRUE)

# make a table for the SI
cap_import_table_data = trans_in_out_srex %>% dplyr::mutate(region_label = paste0(LAB,' (', srex_id, ')')) %>%
                                              dplyr::select(srex_id, region_label, mean_import) %>%
                                              unique() %>%
                                              merge(SREX_gen_cap_plot %>% dplyr::select(srex_id, Tech, capacity) %>%
                                                                          unique() %>%
                                                                          data.table() %>%
                                                                          dcast(srex_id ~ Tech, value.var = 'capacity'),
                                                    by = 'srex_id') %>%
                                              dplyr::mutate(value_in_out_GW = round(mean_import / 1e6, 0),
                                                            installed_wind_GW = round(wind / 1e6, 0),
                                                            installed_solar_GW = round(solar / 1e6, 0)) %>%
                                              dplyr::arrange(desc(value_in_out_GW)) %>%
                                              dplyr::select(region_label, value_in_out_GW, installed_wind_GW, installed_solar_GW)


cap_import_table_data %>% gt(auto_align = 'auto') %>%
                          cols_label(region_label = md('Region'),
                                     value_in_out_GW = md('Mean Net  \n Import (GW)'),
                                     installed_wind_GW = md('Installed wind  \n capacity (GW)'),
                                     installed_solar_GW = md('Installed solar  \n capacity (GW)')) %>%
                          cols_width(everything() ~ px(150)) %>%
                          opt_table_font(font = list(google_font(name = "Spectral"))) %>%
                          gtsave(filename = paste0('table_import_installed_wind_solar_cap_', year,'.png'),
                                 path = 'figs/')


# prepare the wires for ploting
trans_caps = merge(SREX_region_nodes_manual %>% dplyr::select(SREX_id, center_lon, center_lat) %>% unique(),
                   trans_capacities,
                   by = 'SREX_id')

# find the midpoint of each wire in order to place the capacity value in that location
trans_caps = trans_caps %>% dplyr::group_by(wire_ID) %>%
                            dplyr::mutate(center_lon_prod = prod(center_lon))

# now let's remove the wires that should wrap around the back of the globe
trans_caps_non_long = trans_caps %>% dplyr::filter(center_lon_prod > -5000) %>%
                                     dplyr::group_by(wire_ID) %>%
                                     dplyr::mutate(wire_midpoint_lat = mean(center_lat),
                                                   wire_midpoint_lon = mean(center_lon))



# now we need to make two segments for each wire that wraps around the globe
trans_caps_long_wires = trans_caps %>% dplyr::filter(center_lon_prod < -5000) %>%
                                       dplyr::group_by(wire_ID) %>%
                                       dplyr::mutate(center_lat_2 = mean(center_lat),
                                                     center_lon_2 = 180 * sign(center_lon),
                                                     wire_midpoint_lat = ifelse(center_lon_2 < 0, (center_lat + center_lat_2)/2,NA),
                                                     wire_midpoint_lon = ifelse(center_lon_2 < 0, (center_lon + center_lon_2)/2, NA))


fill_lims = ceiling(max(abs(trans_in_out_srex$mean_import/1e6))/25)*25

fill_breaks = round(seq(-fill_lims, fill_lims, fill_lims/3),0)

SREX_in_out_plot = 
  ggplot() + 
  geom_polygon(data = trans_in_out_srex,
               aes(long, lat, group = piece_LAB, fill = mean_import / 1e6),
               alpha = 1,
               col = 'transparent',
               size = 0) +
  scale_fill_fermenter(name = 'Mean net \n import (GW)',
                       breaks = fill_breaks[-c(1,length(fill_breaks))], 
                       palette = "RdBu",
                       limits = c(-fill_lims,fill_lims)) +
  geom_polygon(data = SREX_region_nodes_manual, 
               aes(long,lat,group=factor(LAB)), 
               alpha = 0, 
               col = 'white') +
  labs(title = 'Transmission & Import',
       x = 'lon',
       y = 'lat') +
  geom_path(data = trans_caps_non_long %>% dplyr::filter(capacity != 0),
            aes(x = center_lon, y = center_lat, group = wire_ID, size = capacity/1e6),
            col = 'black',
            alpha = 1) +
  geom_segment(data = trans_caps_long_wires %>% dplyr::filter(capacity != 0),
            aes(x = center_lon, xend = center_lon_2, y = center_lat, yend = center_lat_2, group = wire_ID, size = capacity/1e6),
            col = 'black',
            alpha = 1) +
  scale_size_continuous(name = 'Transmission \n cap. (GW)',
                        breaks = c(5,50,500),
                        range = c(0.25, 3.75)) +
  geom_point(data = SREX_region_nodes_manual %>% dplyr::select(center_lon, center_lat) %>% unique(),
             aes(x=center_lon, y=center_lat),
             col = 'black',
             size = 3.75) +
  coord_map(projection = 'mollweide',
            xlim = c(world_outline$xmin, world_outline$xmax),
            ylim = c(world_outline$ymin, world_outline$ymax - 6), 
            clip = "on") +
  geom_rect(data=world_outline, 
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
            color='gray', fill="transparent", size=1.5) +
  geom_label(aes(x = -193, y = 30, label = 'Net \n importer'), col = '#de2d26', fill = 'white') +
  geom_label(aes(x = -177, y = 6, label = 'Net \n exporter'), col = '#3182bd', fill = 'white') +
  theme_classic() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position=c(0.095, 0.5),
        legend.justification = 'left',
        legend.key.size = unit(0.75, "cm"),
        axis.line = element_blank(),
        plot.title = element_text(size = 22),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))


ggdraw() +
  draw_plot(wind_plot, x = 0, y = 0.6, width = 0.5, height = 0.4) +
  draw_label(label = '(a)', x = 0.45, y = 0.7, size = 20) +
  draw_plot(solar_plot, x = 0.5, y = 0.6, width = 0.5, height = 0.4) +
  draw_label(label = '(b)', x = 0.95, y = 0.7, size = 20) +
  draw_plot(SREX_in_out_plot, x = 0, y = 0, width = 1, height = 0.65) +
  draw_label(label = '(c)', x = 0.9, y = 0.1, size = 20) +
  ggsave(paste0('figs/figure_capacity_import_',year,'.pdf'),
         device = 'pdf',
         width = 11,
         height = 8.5)


# create a table to highlight the transmission stats

# extract the number of time periods and hours per timestep
source('R/extract-time-parameters.R')
time_parameters = 
  extract_time_parameters(year,
                          gg_output_file) %>%
  dplyr::filter(case == 'Global grid')

# extract the demand
demand = read_excel(path = output_path,
                    sheet = "tech results",
                    .name_repair = 'minimal') %>%
         setNames(c('Index', 'Variable', 'Value')) %>%
         dplyr::filter(grepl("demand", Variable)) %>%
         dplyr::select(Variable, Value)


# transmission capacity -- capacity output is in kW
total_trans_capacity = trans_outputs %>%
                       dplyr::select(capacity, wire_ID) %>%
                       unique() %>%
                       dplyr::summarise(total_capacity_kW = sum(capacity)) %>%
                       dplyr::mutate(total_capacity_TWh = total_capacity_kW / 1e9 *
                                                          as.numeric(time_parameters$num_time_periods) *
                                                          as.numeric(time_parameters$delta_t))

# print the total electricity transmitted (before and after losses)

# compute the total dispatch into the wire
trans_dispatch_in = trans_dispatch_by_node %>% dplyr::summarise(total_dispatch_in_kW = sum(mean_dispatch_out_of_node)) %>%
                                                dplyr::mutate(total_dispatch_in_TWh = total_dispatch_in_kW / 1e9 *
                                                                                      as.numeric(time_parameters$delta_t))

trans_dispatch_out = trans_dispatch_by_node %>% dplyr::summarise(total_dispatch_out_kW = sum(mean_dispatch_into_node)) %>%
                                                dplyr::mutate(total_dispatch_out_TWh = total_dispatch_out_kW / 1e9 *
                                                        as.numeric(time_parameters$delta_t))

total_demand = demand %>% dplyr::summarise(total_demand_kW = sum(Value)) %>%
                          dplyr::mutate(total_demand_TWh = total_demand_kW / 1e9 *
                                          as.numeric(time_parameters$num_time_periods) *
                                          as.numeric(time_parameters$delta_t))

trans_table_data = data.frame(Variable = c('Transmission capacity (per year)',
                                           'Energy transmitted out of regions (per year)',
                                           'Energy transmitted into regions (per year)',
                                           'Transmission losses',
                                           'Portion of demand met with transmitted energy'),
                              Value = c(paste0(round(total_trans_capacity$total_capacity_TWh,0),' TWh'),
                                        paste0(round(trans_dispatch_in$total_dispatch_in_TWh,0),' TWh'),
                                        paste0(round(trans_dispatch_out$total_dispatch_out_TWh,0),' TWh'),
                                        paste0(round(100 * ((trans_dispatch_in$total_dispatch_in_TWh - trans_dispatch_out$total_dispatch_out_TWh) / trans_dispatch_in$total_dispatch_in_TWh),0),'%'),
                                        paste0(round(100 * (1 - (total_demand$total_demand_TWh - trans_dispatch_out$total_dispatch_out_TWh) / total_demand$total_demand_TWh),0),'%')))

trans_table_data %>% gt(auto_align = 'auto') %>%
  cols_label(Variable = md(''),
             Value = md('')) %>%
  cols_width(vars(Variable) ~ px(400),
             vars(Value) ~ px(100)) %>%
  opt_table_font(font = list(google_font(name = "Spectral"))) %>%
  gtsave(filename = paste0('table_transmission_stats_', year,'.png'),
         path = 'figs/')


print('Largest trans line capacity (GW):')
print(round(max(trans_capacities$capacity)/1e6,0))

print('Smallest trans line capacity (GW):')
print(round(min(trans_capacities$capacity)/1e6,0))

rm(list = ls())
}
