plot_wind_solar_CFs_capacities = function(
hour_multiple = 3,
years = c(2016:2018),
global_grid_run_ID = 'connected_five_storage'){
  
solar_wind_box_plot = list()


if(length(years) != 3){
  print('Sorry, this function is designed to plot three years of results.')
  stop(call. = FALSE)}
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


# load the n-hour CFs for that year
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


CF0 = merge(wind_CF0,
            solar_CF0,
            by = c('year', 'month', 'day', 'hour', 'srex'))

if(ssrex == 1){CF = CF0}
if(ssrex > 1){CF = bind_rows(CF, CF0)}
}

rm(wind_CF0, solar_CF0, CF0)

CF_long = CF %>% reshape2::melt(id.vars = c('year', 'month', 'day', 'hour', 'srex'),
                                variable.name = 'Tech',
                                value.name = 'CF') %>%
                 dplyr::mutate(Tech = sub(x = Tech, pattern = '_capacity', replacement = ''))


CF_installed_cap_by_srex = merge(CF_long,
                                 wind_solar_capacities,
                                 by = c('srex', 'Tech')) %>%
                           dplyr::group_by(srex, Tech) %>%
                           dplyr::mutate(CF_mean = mean(CF))


load("data/SREX_regions.RData")

plot_data = merge(CF_installed_cap_by_srex,
                  SREX_regions,
                  by.x = 'srex',
                  by.y = 'SREX_id')

solar_wind_box_plot[[ii]] = 
  ggplot(plot_data) +
  geom_boxplot(aes(x = LAB, y = CF * 100, fill = Capacity/1e6)) +
  geom_point(data = plot_data %>% dplyr::select(LAB, Tech, CF_mean) %>% unique(),
             aes(x = LAB, y = CF_mean * 100, col = Tech),
             shape = 17,
             size = 3.5) +
  scale_color_manual(name = 'Generation type',
                     values = c('#f16913', '#238b45')) +
  scale_fill_distiller(name = "Installed \n Capacity (GW)",
                       palette = 'Blues',
                       direction = 1) +
  labs(title = paste0(letters[ii],') ', yyear),
       x = 'Region',
       y = 'Wind CF (%)                Solar CF (%)') +
  theme_bw() +
  theme(legend.position = 'right',
        axis.text.x = element_text(angle = 45,
                                   hjust=1)) +
  facet_wrap(~Tech, nrow = 5)

}

ggdraw() +
  draw_plot(solar_wind_box_plot[[1]], x = 0, y = 2/3, width = 1, height = 1/3) +
  draw_plot(solar_wind_box_plot[[2]], x = 0, y = 1/3, width = 1, height = 1/3) +
  draw_plot(solar_wind_box_plot[[3]], x = 0, y = 0, width = 1, height = 1/3) +
  ggsave(filename = 'figs/figure_solar_wind_CFs_capacities.pdf',
         device = 'pdf',
         width = 9,
         height = 12)
  

rm(list = ls())
}
