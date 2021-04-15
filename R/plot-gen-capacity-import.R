plot_wind_solar_capacity_import = function(year = 2018,
                                           gg_output_file = 'connected_five_storage/global_grid_'){
# let's load the results from the globally connected grid
output_path = list.files(paste0('MEM/Output_Data/global_grid/',gg_output_file,year),
                         pattern = '.xlsx',
                         recursive = TRUE,
                         full.names = TRUE) 


# extract the capacities for wind, solar, and storage
wind_solar_capacities = read_excel(path = output_path, 
                                   sheet = "tech results",
                                   .name_repair = 'minimal') %>%
  setNames(c('ID', 'Name', 'Capacity')) %>% 
  dplyr::select(-ID) %>%
  dplyr::filter(grepl("solar|wind", Name)) %>%
  dplyr::filter(grepl("capacity", Name)) %>%
  tidyr::separate(Name, c('X', 'node', 'Tech'), "_") %>%
  dplyr::mutate(Tech = sub(x = Tech, replacement = '', pattern = ' capacity')) %>%
  dplyr::select(-X)

load('data/srex_polys_land.RData')

SREX_gen_cap_plot = merge(srex_polys_land,
                          wind_solar_capacities,
                          by.x = 'srex_id',
                          by.y = 'node',
                          all = TRUE)

solar_max = ceiling(max(SREX_gen_cap_plot$Capacity[SREX_gen_cap_plot$Tech == 'solar']/1e6) / 100) * 100

solar_breaks = round(seq(0, solar_max, solar_max/5),0)

world_outline = data.frame(xmin=(-180),
                           xmax=(180),
                           ymin=(-65),
                           ymax=(70))

solar_plot = 
  ggplot() + 
  geom_polygon(data = SREX_gen_cap_plot %>% dplyr::filter(Tech == 'solar'),
               aes(long, lat, group = piece_LAB, fill = Capacity/1e6),
               alpha = 1,
               col = 'transparent',
               size = 0) +
  scale_fill_distiller(name = 'Installed \n cap. (GW)',
                       breaks = c(solar_breaks[-1]), 
                       palette = "Oranges",
                       direction = 1) +
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


# solar_plot_rounded_box <- ggplotGrob(solar_plot)
# bg <- solar_plot_rounded_box$grobs[[1]]
# round_bg <- roundrectGrob(x=bg$x, y=bg$y, width=bg$width, height=bg$height,
#                           r=unit(10, "mm"),
#                           just=bg$just, name=bg$name, gp=bg$gp, vp=bg$vp)
# solar_plot_rounded_box$grobs[[1]] <- round_bg

wind_max = ceiling(max(SREX_gen_cap_plot$Capacity[SREX_gen_cap_plot$Tech == 'wind']/1e6) / 100) * 100

wind_breaks = round(seq(0, wind_max, wind_max/5),0)

wind_plot = 
  ggplot() + 
  geom_polygon(data = SREX_gen_cap_plot %>% dplyr::filter(Tech == 'wind'),
               aes(long, lat, group = piece_LAB, fill = Capacity/1e6),
               alpha = 1,
               col = 'transparent',
               size = 0) +
  scale_fill_distiller(name = 'Installed \n cap. (GW)',
                       breaks = c(wind_breaks[-1]), 
                       palette = "Greens",
                       direction = 1) +
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



# wind_plot_rounded_box <- ggplotGrob(wind_plot)
# bg <- wind_plot_rounded_box$grobs[[1]]
# round_bg <- roundrectGrob(x=bg$x, y=bg$y, width=bg$width, height=bg$height,
#                           r=unit(10, "mm"),
#                           just=bg$just, name=bg$name, gp=bg$gp, vp=bg$vp)
# wind_plot_rounded_box$grobs[[1]] <- round_bg

# # make a table for the SI
# capacity_table = cap_regions %>% dplyr::select(region_label, Tech, Capacity) %>% 
#                                  unique() %>% 
#                                  reshape2::dcast(region_label ~ Tech, value.var = 'Capacity') %>% 
#                                  dplyr::mutate(`Installed wind capacity (GW)` = round(wind/1e6,0),
#                                                `Installed solar capacity (GW)` = round(solar/1e6,0),
#                                                `Region` = region_label) %>%
#                                  dplyr::select(`Region`, `Installed wind capacity (GW)`, `Installed solar capacity (GW)`)

# extract the capacities for transmission
trans_capacities = read_excel(path = output_path, 
                              sheet = "tech results",
                              .name_repair = 'minimal') %>%
  setNames(c('ID', 'Name', 'Capacity')) %>% 
  dplyr::select(-ID) %>%
  dplyr::filter(grepl("trans", Name)) %>%
  tidyr::separate(Name, c('X', 'node.x', 'Y', 'node.y', 'Tech'), "_") %>%
  dplyr::mutate(Tech = sub(x = Tech, replacement = '', pattern = ' capacity')) %>%
  dplyr::select(-X, -Y) %>%
  dplyr::mutate(wire_ID = 1:n()) %>%
  reshape2::melt(, id.vars = c('Tech', 'Capacity', 'wire_ID'), value.name = 'SREX_id')



# prepare the in/out for plotting
# let's extract transmission capacities
trans_time_results0 = read_excel(path = output_path, 
                                 sheet = "time results",
                                 .name_repair = 'minimal') %>% 
  dplyr::select(contains(match = 'trans')) %>% 
  dplyr::select(contains(match = 'dispatch')) %>% 
  colSums() %>%
  reshape2::melt() %>%
  rownames_to_column(var = 'variable')


trans_time_results = trans_time_results0 %>% dplyr::mutate(Node_A = str_split_fixed(variable, '_', 5)[,2],
                                                                    Node_B = str_split_fixed(variable, '_', 5)[,4],
                                                                    IN = grepl(" in ", variable),
                                                                    REVERSE = grepl(" reverse ", variable))


trans_time_results_in1 = trans_time_results %>% dplyr::filter(REVERSE & 
                                                                         !IN) %>%
  dplyr::group_by(Node_A) %>%
  dplyr::summarise(value_in1 = mean(value, na.rm = TRUE))


trans_time_results_in2 = trans_time_results %>% dplyr::filter(!REVERSE & 
                                                                         !IN) %>%
  dplyr::group_by(Node_B) %>%
  dplyr::summarise(value_in2 = mean(value, na.rm = TRUE))

trans_time_results_out1 = trans_time_results %>% dplyr::filter(!REVERSE & 
                                                                          IN) %>%
  dplyr::group_by(Node_A) %>%
  dplyr::summarise(value_out1 = mean(value, na.rm = TRUE))

trans_time_results_out2 = trans_time_results %>% dplyr::filter(REVERSE & 
                                                                          IN) %>%
  dplyr::group_by(Node_B) %>%
  dplyr::summarise(value_out2 = mean(value, na.rm = TRUE))

trans_time_results_in_out1 = merge(trans_time_results_in1,
                                       trans_time_results_out1,
                                       by = 'Node_A')

trans_time_results_in_out2 = merge(trans_time_results_in2,
                                       trans_time_results_out2,
                                       by = 'Node_B')


trans_time_results_in_out_connect = merge(trans_time_results_in_out1,
                                              trans_time_results_in_out2,
                                              by.x = 'Node_A',
                                              by.y = 'Node_B',
                                              all = TRUE) %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(value_in = value_in1 + value_in2,
                value_out = value_out1 + value_out2,
                value_in_out = value_in - value_out) %>%
  dplyr::select(Node_A, value_in_out)


trans_in_out_srex =  merge(srex_polys_land,
                           trans_time_results_in_out_connect,
                           by.x = 'srex_id',
                           by.y = 'Node_A',
                           all = TRUE)


import = trans_in_out_srex %>% dplyr::mutate(region_label = paste0(LAB,' (', srex_id, ')')) %>%
                               dplyr::select(region_label, value_in_out) %>%
                               unique() %>%
                               dplyr::mutate(value_in_out = round(value_in_out / 1e9, 0)) %>%
                               setNames(c('Region', 'Mean Net \n Import (GW)'))

# table_data = merge(import,
#                    capacity_table,
#                    by = 'Region')%>%
#   dplyr::arrange(desc(`Mean Net \n Import (GW)`))
# 
# 
# table_data %>% gt(auto_align = 'auto') %>%
#                cols_label(`Region` = md('Region'),
#                           `Mean Net \n Import (GW)` = md('Mean Net  \n Import (GW)'),
#                           `Installed wind capacity (GW)` = md('Installed wind  \n capacity (GW)'),
#                           `Installed solar capacity (GW)` = md('Installed solar  \n capacity (GW)')) %>%
#                cols_width(everything() ~ px(150)) %>%
#                opt_table_font(font = list(google_font(name = "Spectral"))) %>%
#                gtsave(filename = paste0('Table_fig_3_', year,'.png'),
#                       path = 'figs/')


# prepare the wires for ploting
load(file = 'data/SREX_region_nodes_manual.RData')

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



# now we need to make twto segmentst for each wire that wraps around the globe
trans_caps_long_wires = trans_caps %>% dplyr::filter(center_lon_prod < -5000) %>%
                                       dplyr::group_by(wire_ID) %>%
                                       dplyr::mutate(center_lat_2 = mean(center_lat),
                                                     center_lon_2 = 180 * sign(center_lon),
                                                     wire_midpoint_lat = ifelse(center_lon_2 < 0, (center_lat + center_lat_2)/2,NA),
                                                     wire_midpoint_lon = ifelse(center_lon_2 < 0, (center_lon + center_lon_2)/2, NA))


fill_lims = ceiling(max(abs(trans_in_out_srex$value_in_out/1e9))/25)*25

fill_breaks = round(seq(-fill_lims, fill_lims, fill_lims/3),0)

SREX_in_out_plot = 
  ggplot() + 
  geom_polygon(data = trans_in_out_srex,
               aes(long, lat, group = piece_LAB, fill = value_in_out / 1e9),
               alpha = 1,
               col = 'transparent',
               size = 0) +
  scale_fill_fermenter(name = 'Mean net \n import (GW)',
                       breaks = fill_breaks[-c(1,length(fill_breaks))], 
                       palette = "RdBu",
                       limits = c(-fill_lims,fill_lims)) +
  labs(title = 'Transmission & Import',
       x = 'lon',
       y = 'lat') +
  geom_path(data = trans_caps_non_long %>% dplyr::filter(Capacity != 0),
            aes(x = center_lon, y = center_lat, group = wire_ID, size = Capacity/1e6),
            col = 'black',
            alpha = 1) +
  geom_segment(data = trans_caps_long_wires %>% dplyr::filter(Capacity != 0),
            aes(x = center_lon, xend = center_lon_2, y = center_lat, yend = center_lat_2, group = wire_ID, size = Capacity/1e6),
            col = 'black',
            alpha = 1) +
  scale_size_continuous(name = 'Transmission \n cap. (GW)',
                        breaks = c(1,25,100,250),
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
    geom_label(aes(x = -184, y = -22.5, label = 'Net \n importer'), col = '#de2d26', fill = 'white') +
    geom_label(aes(x = -231, y = -50, label = 'Net \n exporter'), col = '#3182bd', fill = 'white') +
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


# SREX_in_out_plot_rounded_box <- ggplotGrob(SREX_in_out_plot)
# bg <- SREX_in_out_plot_rounded_box$grobs[[1]]
# round_bg <- roundrectGrob(x=bg$x, y=bg$y, width=bg$width, height=bg$height,
#                           r=unit(10, "mm"),
#                           just=bg$just, name=bg$name, gp=bg$gp, vp=bg$vp)
# SREX_in_out_plot_rounded_box$grobs[[1]] <- round_bg

#plot(SREX_in_out_plot_rounded_box)

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

# 
# # create a table to highlight the transmission stats
# # extract the number of time periods and hours per timestep
# time_parameters = read_excel(path = output_path, 
#                              sheet = "case input",
#                              .name_repair = 'minimal') %>%
#   setNames(c('Index', 'Variable', 'Value')) %>%
#   dplyr::filter(Variable == 'num_time_periods' |
#                   Variable == 'time_step_hours') %>%
#   dplyr::select(Variable, Value)
# 
# # extract the demand
# demand = read_excel(path = output_path, 
#                              sheet = "tech results",
#                              .name_repair = 'minimal') %>%
#   setNames(c('Index', 'Variable', 'Value')) %>%
#   dplyr::filter(grepl("demand", Variable)) %>%
#   dplyr::select(Variable, Value)


# # transmission capacity -- capacity output is in kW
# total_trans_capacity = trans_capacities %>% 
#                        dplyr::select(Capacity, wire_ID) %>% 
#                        unique() %>%
#                        dplyr::summarise(total_capacity_kW = sum(Capacity)) %>%
#                        dplyr::mutate(total_capacity_TWh = total_capacity_kW / 1e9 * 
#                                                           as.numeric(time_parameters$Value[time_parameters$Variable == 'num_time_periods']) *
#                                                           as.numeric(time_parameters$Value[time_parameters$Variable == 'time_step_hours']))
# 
# # print the total electricity transmitted (before and after losses)
# total_trans_in = trans_time_results0 %>% dplyr::filter(grepl(x = variable, pattern = 'in')) %>% 
#                                          dplyr::summarise(total_dispatch_in = sum(value) / 1e9)
# 
# total_trans_out = trans_time_results0 %>% dplyr::filter(!grepl(x = variable, pattern = 'in')) %>% 
#                                           dplyr::summarise(total_dispatch_out = sum(value) / 1e9)
# 
# 
# total_demand = demand %>% dplyr::summarise(total_demand_kW = sum(Value)) %>%
#                           dplyr::mutate(total_demand_TWh = total_demand_kW / 1e9 * 
#                                                            as.numeric(time_parameters$Value[time_parameters$Variable == 'num_time_periods']) *
#                                                            as.numeric(time_parameters$Value[time_parameters$Variable == 'time_step_hours']))
# 
# trans_table_data = data.frame(Variable = c('Transmission capacity (per year)',
#                                            'Energy transmitted out of regions (per year)',
#                                            'Energy transmitted into regions (per year)',
#                                            'Transmission losses',
#                                            'Portion of demand met with transmitted energy'),
#                               Value = c(paste0(round(total_trans_capacity$total_capacity_TWh,0),' TWh'),
#                                         paste0(round(total_trans_in$total_dispatch_in,0),' TWh'),
#                                         paste0(round(total_trans_out$total_dispatch_out,0),' TWh'),
#                                         paste0(round(100 * ((total_trans_in$total_dispatch_in - total_trans_out$total_dispatch_out) / total_trans_in$total_dispatch_in),0),'%'),
#                                         paste0(round(100 * (1 - (total_demand$total_demand_TWh - total_trans_out$total_dispatch_out) / total_demand$total_demand_TWh),0),'%')))
# 
# 
# mytheme <- gridExtra::ttheme_default(
#   core = list(fg_params=list(cex = 0.75)),
#   colhead = list(fg_params=list(cex = 1.0)),
#   rowhead = list(fg_params=list(cex = 1.0)))
# 
# 
# trans_table = tableGrob(trans_table_data, 
#                         rows = NULL, 
#                         cols = NULL, 
#                         theme = mytheme)

# dev.off()
# gt <- gtable(widths = unit(c(rep(1,10)), "null"), heights = unit(c(rep(1,20)), "null"))
# gt <- gtable_add_grob(gt, ggplotGrob(wind_solar_plot), t = 1, l = 1, b = 10, r = 10)
# gt <- gtable_add_grob(gt, ggplotGrob(SREX_in_out_plot), t = 11, l = 1, b = 20, r = 10)
# gt <- gtable_add_grob(gt, trans_table, t = 18, b = 20, l = 6, r = 7)
# 
# pdf(file = paste0('figs/Figure_3_capacity_import_',year,'.pdf'),
#     width = 12,
#     height = 11)
# grid.draw(gt)
# # #print(
# # grid.arrange(
# #   #   wind_solar_plot,
# #   #   SREX_in_out_plot,
# #   #   nrow = 2
# #   wind_solar_plot,
# #   grid.draw(gt),
# #   nrow = 2
# # )
# # #)
# dev.off()


rm(list = ls())
}
