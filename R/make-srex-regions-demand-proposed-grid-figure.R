
load(file = 'data/SREX_region_nodes_manual.RData')

load(file = 'data/proposed_grid_coords.RData')


# we need to remove the wire from region 1 to 18 and region 9 to 26 and replace it with with two segments that can each stretch to the edge of the maps domain
long_wire_segments = data.frame(wire = c(1, 1, 2, 2, 3, 3, 4, 4),
                                x = c(-130, -180, 110, 180, -70, -180, 145, 180), 
                                y = c(65, 62.5, 60, 62.5, -20, -27.5, -35, -27.5),
                                Grid = 'Interconnected')

# now load the regional wieghtings (i.e. avereage electricity usage)
SREX_demand_weight <- read.csv("data/SREX_demand_weightings.csv",
                               skip = 0,
                               header = TRUE) %>% setNames(c('SREX_id', 'mean_demand'))


TWh_to_GWh = 1e3

GWh_to_mean_GW = 1 / (366 * 24)



load('data/srex_polys_land.RData')

SREX_demand_plot = merge(srex_polys_land,
                         SREX_demand_weight,
                         by.x = 'srex_id',
                         by.y = 'SREX_id',
                         all = TRUE)

world_outline = data.frame(xmin=(-180),
                     xmax=(180),
                     ymin=(-65),
                     ymax=(70))

srex_regions_w_wires_plot = 
  ggplot() + 
   geom_polygon(data = SREX_demand_plot,
                aes(long, lat, group = piece_LAB, fill = mean_demand * TWh_to_GWh * GWh_to_mean_GW),
                alpha = 1,
                col = 'transparent',
                size = 0) +
  scale_fill_distiller(name = 'Mean Demand \n (GW)',
                       palette = "Purples",
                       trans = 'log',
                       direction = 1,
                       breaks = c(5*3, 5*3^2, 5*3^3, 5*3^4)) +
  geom_polygon(data = SREX_region_nodes_manual, 
               aes(long,lat,group=factor(LAB)), 
               alpha = 0, 
               col = 'white') + 
  geom_line(data = proposed_grid_coords %>% dplyr::filter(!(wire_IDs %in% c(3,35))),
            aes(x = center_lon, y = center_lat, group = wire_IDs),
            col = 'black',
            size = 0.75,
            linetype = 'dashed',
            alpha = 0.95) +
  geom_line(data = long_wire_segments,
            aes(x = x, y = y, group = wire),
            col = 'black',
            size = 0.75,
            linetype = 'dashed',
            alpha = 0.95) +
  geom_point(data = SREX_region_nodes_manual %>% dplyr::select(center_lon, center_lat) %>% unique(),
             aes(x=center_lon, y=center_lat),
             col = 'black',
             size = 2.75) +
  geom_label(data = SREX_region_nodes_manual, 
             aes(x=center_lon, y=center_lat - 1.75, label=LAB), 
             col = 'black',
             size = 2,
             label.padding = unit(0.15, "lines")) +
  labs(x = 'lon',
       y = 'lat') +
  coord_map(projection = 'mollweide',
            xlim = c(world_outline$xmin, world_outline$xmax),
            ylim = c(world_outline$ymin, world_outline$ymax - 6), 
            clip = "on") +
  theme_classic() +
  theme(axis.line = element_blank()) +
  theme(legend.position=c(0.13, 0.4),
        legend.justification = 'left',
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.margin = unit(c(1, 0, 0, 0), "lines"),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) +
  geom_rect(data=world_outline, 
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
            color='gray', fill="transparent", size=1.25)

ggdraw() +
  draw_plot(srex_regions_w_wires_plot, x = 0, y = 0, width = 1, height = 1) +
    ggsave(device = 'pdf',
          filename = 'figs/figure_proposed_grid_and_mean_demand.pdf',
          width = 10,
          height = 5)


rm(list = ls())

