load(file = 'data/SREX_region_nodes_manual.RData')


world <- data.frame(maps::map("world", plot=FALSE)[c("x","y")])

# add an outline for the plot
world_outline = data.frame(xmin=(-180),
                           xmax=(180),
                           ymin=(-90),
                           ymax=(90))

srex_regions_plot = 
  ggplot() + 
  geom_path(data = world, aes(x = x, y = y), color = "black", size = 0.1, alpha = 0.3) + 
  geom_point(data = SREX_region_nodes_manual,
             aes(x=center_lon, y=center_lat),
             col = 'black',
             size = 2.5,
             alpha = 0.45) +
  geom_polygon(data = SREX_region_nodes_manual, 
               aes(long,lat,group=factor(LAB)), 
               alpha = 0.001, 
               linetype = 'dashed',
               col = 'black') + 
  geom_label(data = SREX_region_nodes_manual, 
             aes(x=center_lon, y=center_lat + 4.5, label=region_label), 
             col = 'black',
             size = 2,
             label.padding = unit(0.15, "lines")) +
  labs(x = 'lon',
       y = 'lat') +
  coord_map(projection = 'mollweide',
            xlim = c(-180, 180),
            ylim = c(-90,90)) +
  theme_classic() +
  theme(axis.line = element_blank()) +
  theme(legend.position=c(0.025, 0.325),
        legend.justification = 'left',
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.margin = unit(c(1, 0, 0, 0), "lines")) +
  geom_rect(data=world_outline, 
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
            color='gray', fill="transparent", size=1.25)


ggdraw() +
  draw_plot(srex_regions_plot, x = 0, y = 0, width = 1, height = 1) +
    ggsave(device = 'pdf',
          filename = 'figs/figure_srex_regions_SI.pdf',
          width = 10,
          height = 6)


rm(list = ls())
