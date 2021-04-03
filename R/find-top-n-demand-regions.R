find_top_n_demand_regions = function(
n_sites = 5,
demand_file = 'data/SREX_demand_weightings.csv'){
  
# load the solar cap. factor data and only keep the data from years 2016 through 2018
# load the srex demand regions mean demand
SREX_demand_weight <- read.csv(demand_file,
                               skip = 0,
                               header = TRUE) %>% setNames(c('srex', 'mean_demand_TWh')) %>%
                                                  dplyr::arrange(desc(mean_demand_TWh))
  
return_this = data.frame(top_demand = SREX_demand_weight$srex[1:n_sites])

return(return_this)
}
