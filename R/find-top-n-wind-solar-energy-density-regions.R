find_top_n_wind_solar_energy_density_regions = function(
year,
n_sites = 5,
solar_file = 'data/capacity_factors_wind_solar/SREXs_capacity.csv',
wind_file = 'data/capacity_factors_wind_solar/SREXw_capacity.csv'){
  
# load the solar cap. factor data and only keep the data from years specified
SREXs_capacity <- read.csv(solar_file) %>% dplyr::filter(Year %in% c(year)) %>%
                                           data.table() %>%
                                           melt(id.vars = c('Year', 'Month', 'Day', 'Hour'),
                                                variable.name = 'srex',
                                                value.name = 'cf') %>%
                                           dplyr::mutate(srex = sub(x = srex, pattern = 'SREX', replacement = '')) %>%
                                           tidyr::separate(col = srex, into = c('srex', 'srex_id'), "([.])") %>%
                                           dplyr::group_by(srex) %>%
                                           dplyr::summarise(mean_cf = mean(cf, na.rm = TRUE)) %>%
                                           dplyr::arrange(desc(mean_cf))

# load the wind cap. factor data and only keep the data from years specified
SREXw_capacity <- read.csv(wind_file) %>% dplyr::filter(Year %in% c(year)) %>%
                                          data.table() %>%
                                          melt(id.vars = c('Year', 'Month', 'Day', 'Hour'),
                                                         variable.name = 'srex',
                                                         value.name = 'cf') %>%
                                          dplyr::mutate(srex = sub(x = srex, pattern = 'SREX', replacement = '')) %>%
                                          tidyr::separate(col = srex, into = c('srex', 'srex_id'), "([.])") %>%
                                          dplyr::group_by(srex) %>%
                                          dplyr::summarise(mean_cf = mean(cf, na.rm = TRUE)) %>%
                                          dplyr::arrange(desc(mean_cf))

return_this = data.frame(top_solar = SREXs_capacity$srex[1:n_sites],
                         top_wind = SREXw_capacity$srex[1:n_sites])

return(return_this)
}
