plot_power_flows = function(year = 2018,
                            gg_output_file = 'connected_five_storage/global_grid_',
                            hour_a = 4.5,
                            hour_b = 13.5,
                            hour_c = 22.5,
                            date_a = as.POSIXct('2018-12-21', tz = 'UTC'),
                            date_b = as.POSIXct('2018-06-21', tz = 'UTC')){

# extract the transmission results
source('R/extract-transmission-dispatch-by-wire-and-timestep.R')
trans_outputs = 
    extract_trans_dispatch_by_wire_and_timestep(year,
                                                gg_output_file)

# compute the means over the four time periods
trans_dispatch = trans_outputs %>% dplyr::mutate(hour = lubridate::hour(date_time) + lubridate::minute(date_time) / 60,
                                                 date = as.Date(date_time)) %>%
                                  dplyr::mutate(hour_id = ifelse(hour == hour_a, hour_a,
                                                                 ifelse(hour == hour_b, hour_b,
                                                                        ifelse(hour == hour_c, hour_c, NA))),
                                                week_id = ifelse((date_a - 4 * 3600 * 24) < date & date < (date_a + 4 * 3600 * 24), as.character(date_a), 
                                                                 ifelse((date_b - 4 * 3600 * 24) < date & date < (date_b + 4 * 3600 * 24), as.character(date_b), NA))) %>%
                                  dplyr::filter(!is.na(hour_id) &
                                                !is.na(week_id)) %>%
                                  dplyr::mutate(hour_week_id = paste0(hour_id, ';', as.character(week_id),' UTC'),
                                                hour_week_label = hour_week_id) %>%
                                  dplyr::group_by(Node_A, Node_B, hour_week_label) %>%
                                  dplyr::summarise(mean_dispatch_out = mean(dispatch_out))

# compute the net import for each region for each of the regions during the weeks and
# times of day of interest
source('R/extract-transmission-dispatch.R')
trans_dispatch_by_node =
  extract_trans_dispatch(year = year,
                         gg_output_file = 'connected_five_storage/global_grid_')

regional_import = trans_dispatch_by_node  %>% dplyr::mutate(hour = lubridate::hour(date_time) + lubridate::minute(date_time) / 60,
                                                                date = as.Date(date_time)) %>%
  dplyr::mutate(hour_id = ifelse(hour == hour_a, hour_a,
                                 ifelse(hour == hour_b, hour_b,
                                        ifelse(hour == hour_c, hour_c, NA))),
                week_id = ifelse((date_a - 4 * 3600 * 24) < date & date < (date_a + 4 * 3600 * 24), as.character(date_a), 
                                 ifelse((date_b - 4 * 3600 * 24) < date & date < (date_b + 4 * 3600 * 24), as.character(date_b), NA))) %>%
  dplyr::filter(!is.na(hour_id) &
                  !is.na(week_id)) %>%
  dplyr::mutate(hour_week_id = paste0(hour_id, ';', as.character(week_id),' UTC'),
                hour_week_label = hour_week_id) %>%
  dplyr::mutate(import = mean_dispatch_into_node - mean_dispatch_out_of_node) %>%
  dplyr::group_by(Node, hour_week_label) %>%
  dplyr::summarise(mean_import = mean(import))


load('data/srex_polys_land.RData')

SREX_import_plot = merge(srex_polys_land,
                          regional_import,
                          by.x = 'srex_id',
                          by.y = 'Node',
                          all = TRUE)

# try making this into a plot
load('data/SREX_region_nodes_manual.RData')

# add in wire IDs
wire_IDs = trans_dispatch %>% dplyr::select(Node_A, Node_B) %>% 
  unique() %>%
  ungroup() %>%
  dplyr::mutate(wire_ID = 1:n())


trans_dispatch_long = trans_dispatch %>% merge(wire_IDs,
                                               by = c('Node_A', 'Node_B')) %>%
                                         data.table() %>%
                                         melt(id.vars = c('hour_week_label', 'wire_ID', 'mean_dispatch_out'), value.name = 'node')


trans_regions = merge(SREX_region_nodes_manual %>% dplyr::select(SREX_id, center_lon, center_lat) %>% unique(),
                      trans_dispatch_long,
                      by.x = 'SREX_id',
                      by.y = 'node')

# find the midpoint of each wire in order to place the capacity value in that location
trans_regions = trans_regions %>% dplyr::group_by(wire_ID) %>%
  dplyr::mutate(center_lon_prod = min(center_lon) * max(center_lon))

# now let's remove the wires that should wrap around the back of the globe
trans_regions_non_long = trans_regions %>% dplyr::filter(center_lon_prod > -5000) %>%
  dplyr::group_by(wire_ID) %>%
  dplyr::mutate(wire_midpoint_lat = mean(center_lat),
                wire_midpoint_lon = mean(center_lon))



# now we need to make two segmentst for each wire that wraps around the globe
trans_regions_long_wires = trans_regions %>% dplyr::filter(center_lon_prod < -5000) %>%
  dplyr::group_by(wire_ID) %>%
  dplyr::mutate(center_lat_2 = mean(center_lat),
                center_lon_2 = 180 * sign(center_lon),
                wire_midpoint_lat = ifelse(center_lon_2 < 0, (center_lat + center_lat_2)/2,NA),
                wire_midpoint_lon = ifelse(center_lon_2 < 0, (center_lon + center_lon_2)/2, NA))


fill_lims = ceiling(max(abs(SREX_import_plot$mean_import/1e6))/20)*20
                  
fill_breaks = seq(-fill_lims, fill_lims, fill_lims/4)

world_outline = data.frame(xmin=(-180),
                           xmax=(180),
                           ymin=(-65),
                           ymax=(70))

arrow_angle = 10
arrow_length = 0.125

# plot title label that shows the time like: "X-Y UTC"
SREX_import_plot = SREX_import_plot %>% tidyr::separate(col = hour_week_label, sep = ';', into = c("hour", "week"), remove = FALSE) %>%
                                        dplyr::mutate(plot_label = paste0(as.numeric(hour) - 1.5, '-', as.numeric(hour) + 1.5,' UTC'))

dispatch_plots = list()

for(ttime in unique(SREX_import_plot$hour_week_label)){
  
plot_label = SREX_import_plot %>% dplyr::filter(hour_week_label == ttime) %>% dplyr::select(plot_label) %>% unique()

dispatch_plots[[ttime]] = 
  ggplot() + 
  geom_polygon(data = SREX_import_plot %>% dplyr::filter(hour_week_label == ttime),
               aes(long, lat, group = piece_LAB, fill = mean_import / 1e6),
               alpha = 1,
               col = 'transparent',
               size = 0) +
  geom_polygon(data = SREX_import_plot %>% dplyr::filter(hour_week_label == ttime &
                                                           mean_import == 0),
               aes(long, lat, group = piece_LAB),
               fill = 'gray',
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
  labs(title = plot_label,
       x = 'lon',
       y = 'lat') +
  theme_classic() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position='',
        legend.justification = 'left',
        legend.key.size = unit(0.75, "cm"),
        axis.line = element_blank(),
        plot.title = element_text(size = 22, hjust = 0.35),
        plot.margin = margin(1.5, 0.5, 0.5, 0.5, "cm"),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)) +
  geom_path(data = trans_regions_non_long %>% dplyr::filter(hour_week_label == ttime) %>% 
                                              dplyr::filter(mean_dispatch_out > 0), # reverse trans -- arrow ends on first node
            aes(x = center_lon, y = center_lat, group = wire_ID, col = mean_dispatch_out/1e6),
            size = 0.5,
            alpha = 1) +
  geom_path(data = trans_regions_non_long %>% dplyr::filter(hour_week_label == ttime) %>% 
                                              dplyr::filter(mean_dispatch_out < 0), # reverse trans -- arrow ends on first node
            aes(x = center_lon, y = center_lat, group = wire_ID, col = mean_dispatch_out/1e6),
            size = 0.5,
            alpha = 1) +
  geom_segment(data = trans_regions_long_wires %>% dplyr::filter(hour_week_label == ttime) %>% 
                                                   dplyr::filter(mean_dispatch_out > 0 &
                                                                 variable == 'Node_A'),
               aes(x = center_lon, xend = center_lon_2, y = center_lat, yend = center_lat_2, group = wire_ID, col = mean_dispatch_out/1e6),
               size = 0.5,
               alpha = 1) +
  geom_segment(data = trans_regions_long_wires %>% dplyr::filter(hour_week_label == ttime) %>% dplyr::filter(mean_dispatch_out > 0 &
                                                                        variable == 'Node_B'),
               aes(x = center_lon, xend = center_lon_2, y = center_lat, yend = center_lat_2, group = wire_ID, col = mean_dispatch_out/1e6),
               size = 0.5,
               alpha = 1) +
  geom_segment(data = trans_regions_long_wires %>% dplyr::filter(hour_week_label == ttime) %>% dplyr::filter(mean_dispatch_out < 0 &
                                                                        variable == 'Node_A'),
               aes(x = center_lon, xend = center_lon_2, y = center_lat, yend = center_lat_2, group = wire_ID, col = -mean_dispatch_out/1e6),
               size = 0.5,
               alpha = 1) +
  geom_segment(data = trans_regions_long_wires %>% dplyr::filter(hour_week_label == ttime) %>% dplyr::filter(mean_dispatch_out < 0 &
                                                                        variable == 'Node_B'),
               aes(x = center_lon, xend = center_lon_2, y = center_lat, yend = center_lat_2, group = wire_ID, col = -mean_dispatch_out/1e6),
               size = 0.5,
               alpha = 1) +
  geom_path(data = trans_regions_non_long %>% dplyr::filter(hour_week_label == ttime) %>% 
              dplyr::filter(mean_dispatch_out > 0), # reverse trans -- arrow ends on first node
            aes(x = center_lon, y = center_lat, group = wire_ID, col = mean_dispatch_out/1e6),
            arrow = arrow(angle = arrow_angle, length = unit(arrow_length, "inches"),
                          ends = "last", type = "closed"),
            size = 0.5,
            alpha = 1) +
  geom_path(data = trans_regions_non_long %>% dplyr::filter(hour_week_label == ttime) %>% 
              dplyr::filter(mean_dispatch_out < 0), # reverse trans -- arrow ends on first node
            aes(x = center_lon, y = center_lat, group = wire_ID, col = -mean_dispatch_out/1e6),
            arrow = arrow(angle = arrow_angle, length = unit(arrow_length, "inches"),
                          ends = "first", type = "closed"),
            size = 0.5,
            alpha = 1) +
  geom_segment(data = trans_regions_long_wires %>% dplyr::filter(hour_week_label == ttime) %>% dplyr::filter(mean_dispatch_out > 0 &
                                                                                                            variable == 'Node_A'),
               aes(x = center_lon, xend = center_lon_2, y = center_lat, yend = center_lat_2, group = wire_ID, col = mean_dispatch_out/1e6),
               arrow = arrow(angle = arrow_angle, length = unit(arrow_length, "inches"),
                             ends = "last", type = "closed"),
               size = 0.5,
               alpha = 1) +
  geom_segment(data = trans_regions_long_wires %>% dplyr::filter(hour_week_label == ttime) %>% dplyr::filter(mean_dispatch_out > 0 &
                                                                                                               variable == 'Node_B'),
               aes(x = center_lon, xend = center_lon_2, y = center_lat, yend = center_lat_2, group = wire_ID, col = mean_dispatch_out/1e6),
               arrow = arrow(angle = arrow_angle, length = unit(arrow_length, "inches"),
                             ends = "first", type = "closed"),
               size = 0.5,
               alpha = 1) +
  geom_segment(data = trans_regions_long_wires %>% dplyr::filter(hour_week_label == ttime) %>% dplyr::filter(mean_dispatch_out < 0 &
                                                                                                               variable == 'Node_A'),
               aes(x = center_lon, xend = center_lon_2, y = center_lat, yend = center_lat_2, group = wire_ID, col = -mean_dispatch_out/1e6),
               arrow = arrow(angle = arrow_angle, length = unit(arrow_length, "inches"),
                             ends = "first", type = "closed"),
               size = 0.5,
               alpha = 1) +
  geom_segment(data = trans_regions_long_wires %>% dplyr::filter(hour_week_label == ttime) %>% dplyr::filter(mean_dispatch_out < 0 &
                                                                                                               variable == 'Node_B'),
               aes(x = center_lon, xend = center_lon_2, y = center_lat, yend = center_lat_2, group = wire_ID, col = -mean_dispatch_out/1e6),
               arrow = arrow(angle = arrow_angle, length = unit(arrow_length, "inches"),
                             ends = "last", type = "closed"),
               size = 0.5,
               alpha = 1) +
  geom_point(data = SREX_region_nodes_manual %>% dplyr::select(center_lon, center_lat) %>% unique(),
             aes(x=center_lon, y=center_lat),
             col = 'black',
             size = 0.5) +
  scale_color_fermenter(name = 'Mean trans. \n dispatch (GW)',
                        direction = 1,
                        breaks = c(2^1, 2^2, 2^3, 2^4, 2^5, 2^6, 2^7, 2^8),
                        palette = "Greys",
                        limits=c(0,500)) +
  coord_map(projection = 'mollweide',
            xlim = c(world_outline$xmin, world_outline$xmax),
            ylim = c(world_outline$ymin, world_outline$ymax - 6), 
            clip = "on") +
  geom_rect(data=world_outline, 
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
            color='gray', fill="transparent", size=1.25) 
 
}

# extract the legend
leg <- get_legend(
  ggplot() + 
    geom_polygon(data = SREX_import_plot %>% dplyr::filter(hour_week_label == ttime),
                 aes(long, lat, group = piece_LAB, fill = mean_import / 1e6),
                 alpha = 1,
                 col = 'transparent',
                 size = 0) +
    scale_fill_fermenter(name = 'Mean net \n import (GW)',
                         breaks = fill_breaks[-c(1,length(fill_breaks))],
                         palette = "RdBu",
                         limits = c(-fill_lims,fill_lims)) +
    scale_color_fermenter(name = 'Mean trans. \n dispatch (GW)',
                          direction = 1,
                          breaks = c(2^1, 2^2, 2^3, 2^4, 2^5, 2^6, 2^7, 2^8),
                          palette = "Greys",
                          limits=c(0,500)) +
    theme_classic() +
    theme(legend.title = element_text(size = 15),
          legend.text = element_text(size = 12),
          legend.position = 'right',
          legend.key.height = unit(0.75, "cm"),
          legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) +
    geom_path(data = trans_regions_non_long, # reverse trans -- arrow ends on first node
              aes(x = center_lon, y = center_lat, group = wire_ID, col = mean_dispatch_out/1e6),
              alpha = 1)
    )


# 1) compute the sunset/sunrise for each point
resolution = 1
sun_winter = data.frame(lat = rep(seq(-90,90,resolution), each = length(seq(-180,180,resolution))),
                            lon = rep(seq(-180,180,resolution), length(seq(-90,90,resolution))),
                            dateTime = as.POSIXct(date_a, tz = 'UTC'),
                            sunrise = NA,
                            next_sunrise = NA,
                            sunset = NA,
                            prior_sunset = NA)

sun_winter$sunrise = 
sunriset(crds = as.matrix(sun_winter[,c('lon','lat')]),
         dateTime = sun_winter$dateTime,
         direction = 'sunrise',
         POSIXct.out = TRUE)$time

sun_winter$next_sunrise = 
  sunriset(crds = as.matrix(sun_winter[,c('lon','lat')]),
           dateTime = sun_winter$dateTime + (60 * 60 * 24),
           direction = 'sunrise',
           POSIXct.out = TRUE)$time

sun_winter$sunset = 
  sunriset(crds = as.matrix(sun_winter[,c('lon','lat')]),
           dateTime = sun_winter$dateTime,
           direction = 'sunset',
           POSIXct.out = TRUE)$time

sun_winter$prior_sunset = 
  sunriset(crds = as.matrix(sun_winter[,c('lon','lat')]),
           dateTime = sun_winter$dateTime - (60 * 60 * 24),
           direction = 'sunset',
           POSIXct.out = TRUE)$time

# 2) ask whether the relavent data-time is between these times

for(hhour in c(hour_a, hour_b, hour_c)){
  time_temp = unique(sun_winter$dateTime) + 60 * 60 * hhour
  
  sun_winter = sun_winter %>% dplyr::mutate(temp = ifelse(is.na(sunset) & lat < (-50), 'light',
                                                          ifelse(is.na(sunset) & lat > (50), 'dark',
                                                                 ifelse(time_temp > as.POSIXct(sunrise,tz = 'UTC') &
                                                                        time_temp < as.POSIXct(sunset,tz = 'UTC') |
                                                                        time_temp > as.POSIXct(next_sunrise,tz = 'UTC') |
                                                                        time_temp < as.POSIXct(prior_sunset,tz = 'UTC'),'light',
                                                                        'dark'))))
  
  names(sun_winter)[length(names(sun_winter))] = hhour
}

sun_winter_long = sun_winter %>% data.table() %>% 
                                 melt(id.vars = c('lat', 'lon', 'dateTime', 'sunrise', 'next_sunrise', 'sunset', 'prior_sunset'),
                                      variable.name = 'hour',
                                      value.name = 'light_or_dark')

sun_winter_long_curves = sun_winter_long %>% dplyr::filter(light_or_dark == 'dark') %>%
                                             dplyr::group_by(hour, lon) %>%
                                             dplyr::summarise(min_lat_dark = min(lat))

winter_day_night_plots = list()
for(pplot_num in 1:3){
  
hhour = c(hour_a, hour_b, hour_c)[pplot_num]

winter_day_night_plots[[pplot_num]] =
  ggplot(sun_winter_long_curves %>% dplyr::filter(hour == hhour)) +
  borders("world", colour = "transparent", fill = "gray") +
  geom_ribbon(aes(x = lon, ymin = min_lat_dark, ymax = 90),
              fill = 'black', 
              alpha = 0.25) +
  theme_classic() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.position='',
        panel.border = element_rect(colour = "gray", fill=NA, size=2),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)) +
  labs(title = '') +
  coord_equal(
    xlim = c(-162.5, 162.5),
    ylim = c(-55,65),
    clip = 'on')
}


sun_summer = data.frame(lat = rep(seq(-90,90,resolution), each = length(seq(-180,180,resolution))),
                        lon = rep(seq(-180,180,resolution), length(seq(-90,90,resolution))),
                        dateTime = as.POSIXct(date_b, tz = 'UTC'),
                        sunrise = NA,
                        next_sunrise = NA,
                        sunset = NA,
                        prior_sunset = NA)

sun_summer$sunrise = 
  sunriset(crds = as.matrix(sun_summer[,c('lon','lat')]),
           dateTime = sun_summer$dateTime,
           direction = 'sunrise',
           POSIXct.out = TRUE)$time

sun_summer$next_sunrise = 
  sunriset(crds = as.matrix(sun_summer[,c('lon','lat')]),
           dateTime = sun_summer$dateTime + (60 * 60 * 24),
           direction = 'sunrise',
           POSIXct.out = TRUE)$time

sun_summer$sunset = 
  sunriset(crds = as.matrix(sun_summer[,c('lon','lat')]),
           dateTime = sun_summer$dateTime,
           direction = 'sunset',
           POSIXct.out = TRUE)$time

sun_summer$prior_sunset = 
  sunriset(crds = as.matrix(sun_summer[,c('lon','lat')]),
           dateTime = sun_summer$dateTime - (60 * 60 * 24),
           direction = 'sunset',
           POSIXct.out = TRUE)$time

# 2) ask whether the relavent data-time is between these times

for(hhour in c(hour_a, hour_b, hour_c)){
  time_temp = unique(sun_summer$dateTime) + 60 * 60 * hhour
  
  sun_summer = sun_summer %>% dplyr::mutate(temp = ifelse(is.na(sunset) & lat < (-50), 'dark',
                                                          ifelse(is.na(sunset) & lat > (50), 'light',
                                                                 ifelse(time_temp > as.POSIXct(sunrise,tz = 'UTC') &
                                                                        time_temp < as.POSIXct(sunset,tz = 'UTC') |
                                                                        time_temp > as.POSIXct(next_sunrise,tz = 'UTC') |
                                                                        time_temp < as.POSIXct(prior_sunset,tz = 'UTC'), 'light',
                                                                        'dark'))))
  
  names(sun_summer)[length(names(sun_summer))] = hhour
}

sun_summer_long = sun_summer %>% data.table() %>%
                                 melt(id.vars = c('lat', 'lon', 'dateTime', 'sunrise', 'next_sunrise', 'sunset', 'prior_sunset'),
                                      variable.name = 'hour',
                                      value.name = 'light_or_dark')

sun_summer_long_curves = sun_summer_long %>% dplyr::filter(light_or_dark == 'dark') %>%
                                             dplyr::group_by(hour, lon) %>%
                                             dplyr::summarise(max_lat_dark = max(lat))

summer_day_night_plots = list()
for(pplot_num in 1:3){
  
hhour = c(hour_a, hour_b, hour_c)[pplot_num]

summer_day_night_plots[[pplot_num]] =
    ggplot(sun_summer_long_curves %>% dplyr::filter(hour == hhour)) +
    borders("world", colour = "transparent", fill = "gray") +
    geom_ribbon(aes(x = lon, ymin = -90, ymax = max_lat_dark),
                fill = 'black', 
                alpha = 0.25) +
    theme_classic() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          plot.margin = unit(c(0,0,0,0), "cm"),
          legend.position='',
          panel.border = element_rect(colour = "gray", fill=NA, size=2),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA)) +
    labs(title = '') +
    coord_equal(#projection = 'mollweide',
      xlim = c(-162.5, 162.5),
      ylim = c(-55,65),
      clip = 'on')
}


day_night_plot_size = 0.075
day_night_plot_v = 0.25
day_night_plot_h = 0.25


ggdraw() +
  draw_plot(dispatch_plots[[which(grepl(pattern = paste0(lubridate::month(date_b),'-21'), names(dispatch_plots)) & grepl(pattern = hour_a, names(dispatch_plots)))]], 
            x = 0, y = (2*0.315), width = 0.5, height = 0.315, scale = 1.15) +
  draw_plot(dispatch_plots[[which(grepl(pattern = paste0(lubridate::month(date_b),'-21'), names(dispatch_plots)) & grepl(pattern = hour_b, names(dispatch_plots)))]], 
            x = 0, y = 0.315, width = 0.5, height = 0.315, scale = 1.15) +
  draw_plot(dispatch_plots[[which(grepl(pattern = paste0(lubridate::month(date_b),'-21'), names(dispatch_plots)) & grepl(pattern = hour_c, names(dispatch_plots)))]], 
            x = 0, y = 0, width = 0.5, height = 0.315, scale = 1.15) +
  draw_plot(dispatch_plots[[which(grepl(pattern = paste0(lubridate::month(date_a),'-21'), names(dispatch_plots)) & grepl(pattern = hour_a, names(dispatch_plots)))]], 
            x = 0.5, y = (2*0.315), width = 0.5, height = 0.315, scale = 1.15) +
  draw_plot(dispatch_plots[[which(grepl(pattern = paste0(lubridate::month(date_a),'-21'), names(dispatch_plots)) & grepl(pattern = hour_b, names(dispatch_plots)))]], 
            x = 0.5, y = 0.315, width = 0.5, height = 0.315, scale = 1.15) +
  draw_plot(dispatch_plots[[which(grepl(pattern = paste0(lubridate::month(date_a),'-21'), names(dispatch_plots)) & grepl(pattern = hour_c, names(dispatch_plots)))]], 
            x = 0.5, y = 0, width = 0.5, height = 0.315, scale = 1.15) +
  draw_plot(winter_day_night_plots[[1]], x = 0.5 + day_night_plot_h, y = (2*0.315) + day_night_plot_v, width = day_night_plot_size, height = day_night_plot_size) +
  draw_plot(winter_day_night_plots[[2]], x = 0.5 + day_night_plot_h, y = 0.315 + day_night_plot_v, width = day_night_plot_size, height = day_night_plot_size) +
  draw_plot(winter_day_night_plots[[3]], x = 0.5 + day_night_plot_h, y = 0 + day_night_plot_v, width = day_night_plot_size, height = day_night_plot_size) +
  draw_plot(summer_day_night_plots[[1]], x = 0 + day_night_plot_h, y = (2*0.315) + day_night_plot_v, width = day_night_plot_size, height = day_night_plot_size) +
  draw_plot(summer_day_night_plots[[2]], x = 0 + day_night_plot_h, y = 0.315 + day_night_plot_v, width = day_night_plot_size, height = day_night_plot_size) +
  draw_plot(summer_day_night_plots[[3]], x = 0 + day_night_plot_h, y = 0 + day_night_plot_v, width = day_night_plot_size, height = day_night_plot_size) +
  draw_plot(leg, x = 0.215, y = 0.325, width = 0.6, height = 0.4, scale = 1) +
  draw_label(label = paste0('7 days centered around June 21, ', year), x = 0.25, y = 0.975, fontface = 'plain', size = 25) +
  draw_label(label = paste0('7 days centered around December 21, ', year), x = 0.75, y = 0.975, fontface = 'plain', size = 25) +
  draw_label(label = '(a)', x = 0.08, y = 1-0.12, size = 25) +
  draw_label(label = '(b)', x = 0.08, y = 1-0.12-(1 * 0.315), size = 25) +
  draw_label(label = '(c)', x = 0.08, y = 1-0.12-(2 * 0.315), size = 25) +
  draw_label(label = '(d)', x = 0.58, y = 1-0.12, size = 25) +
  draw_label(label = '(e)', x = 0.58, y = 1-0.12-(1 * 0.315), size = 25) +
  draw_label(label = '(f)', x = 0.58, y = 1-0.12-(2 * 0.315), size = 25) +
  ggsave(filename = paste0('figs/figure_power_flows_',year,'.pdf'),
         device = 'pdf',
         width = 16,
         height = 12)

}