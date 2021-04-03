make_regional_demand_table = function(time_offsets_file = "data/srex_region_time_offsets.csv",
                                      regional_demand_weights_file = "data/SREX_demand_weightings.csv"){
  
srex_region_time_offsets <- read_csv(time_offsets_file)


SREX_demand_weightings <- read_csv(regional_demand_weights_file)



region_table0 = merge(srex_region_time_offsets,
                     SREX_demand_weightings,
                     by.x = 'Index',
                     by.y = 'SREX region') %>% setNames(c('Region #', 'Short name', 'Full name', 'Hour bias relative to region ENA',
                                                          'Hemisphere', 'Estimated demand (TWh)')) %>% 
                                               dplyr::mutate(`Mean demand (GW)` = `Estimated demand (TWh)` * 1000 / 365 / 24,
                                                             Hemisphere = ifelse(Hemisphere == 1, 'S', 'N'))


region_table_totals = data.frame('Region #' = 'Total',
                                 'Short name' = 'Total',
                                 'Full name' = 'Total',
                                 'Hour bias relative to region ENA' = '-',
                                 'Hemisphere' = '-',
                                  t(colSums(region_table0[,-c(1:5)],  na.rm = TRUE)),
                                 check.names = FALSE)


region_table = bind_rows(region_table0 %>% dplyr::mutate(`Region #` = as.factor(`Region #`),
                                                         `Hour bias relative to region ENA` = as.factor(`Hour bias relative to region ENA`)),
                         region_table_totals) %>%
  dplyr::mutate(`Mean demand (GW)` = round(`Mean demand (GW)`, 0))

region_table %>% gt(auto_align = 'auto') %>%
  cols_width(vars('Region #') ~ px(75),
             vars('Short name') ~ px(95),
             vars('Full name') ~ px(230),
             vars('Hemisphere') ~ px(100),
             everything() ~ px(150)) %>%
  opt_table_font(font = list(google_font(name = "Spectral"))) %>%
  gtsave(filename = paste0('table_regions_and_demand.png'),
         path = 'figs/')


}
