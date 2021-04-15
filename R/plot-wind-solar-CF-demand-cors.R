plot_wind_solar_demand_cors = function(
hour_multiple = 3,
prototype_SREX_ID = 'ENA',
years = c(2016:2018)){
  
wind_solar_demand_cors_plot = list()


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

demand_solar_wind_cors = CF_demand %>% dplyr::filter(year == yyear) %>%
  dplyr::group_by(srex) %>%
  dplyr::summarise(`solar-demand` = cor(solar_capacity, demand),
                   `wind-demand` = cor(wind_capacity, demand))

demand_solar_wind_cors_long = reshape2::melt(demand_solar_wind_cors, 
                                             id.vars = 'srex',
                                             value.name = 'cor',
                                             variable.name = 'cor_type')

wind_solar_demand_cors_plot[[ii]] =
  ggplot(demand_solar_wind_cors_long) +
  geom_hline(yintercept = 0,
             linetype = 'dashed') +
  geom_boxplot(aes(cor_type, cor, fill = cor_type)) +
  labs(title = paste0(letters[ii],') ', yyear),
       y = 'Correlation',
       x = '') +
  scale_fill_manual(values = c('#f16913', '#238b45')) +
  scale_y_continuous(limits = c(-0.6,0.6)) +
  theme_classic() +
  theme(legend.position = '')

}


ggdraw() +
  draw_plot(wind_solar_demand_cors_plot[[1]], x = 0, y = 2/3, width = 1, height = 1/3) +
  draw_plot(wind_solar_demand_cors_plot[[2]], x = 0, y = 1/3, width = 1, height = 1/3) +
  draw_plot(wind_solar_demand_cors_plot[[3]], x = 0, y = 0, width = 1, height = 1/3) +
  ggsave(filename = 'figs/figure_wind_solar_CFs_demand_cors.pdf',
         device = 'pdf',
         width = 5,
         height = 12)


rm(list = ls())
}
