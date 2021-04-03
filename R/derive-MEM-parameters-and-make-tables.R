derive_MEM_parameters_and_make_tables = function(MEM_cost_assumptions_file = "data/MEM_cost_assumptions.xlsx"){


MEM_cost_assumptions <- read_excel(MEM_cost_assumptions_file) %>% dplyr::select(-`Source long`)


MEM_cost_assumptions %>% dplyr::mutate(Value = signif(Value, 3),
                                       Source = replace_na(Source, replace = '-')) %>%
  gt() %>%
  cols_align(align = c('left'),
             columns = everything()) %>%
  cols_align(align = c('center'),
             columns = vars(Source)) %>%
  opt_table_font(font = list(google_font(name = "Spectral"))) %>%
  cols_width(vars(Component) ~ px(225),
             vars(Value) ~ px(150),
             vars(Source) ~ px(100),
             everything() ~ px(350)) %>%
  tab_options(table.font.size = px(25)) %>%
  gtsave(filename = paste0('table_raw_MEM_parameters.png'),
         path = 'figs/')


constants = data.frame(MEM_cost_assumptions %>% dplyr::filter(`Value description` == 'Project life (years)') %>% dplyr::select(Component, Value) %>% setNames(c('Tech', 'project_life')),
                       hours_per_year = 24 * 365,
                       discount_rate = MEM_cost_assumptions$Value[MEM_cost_assumptions$`Value description` == 'Discount rate (%)']/100)



MEM_inputs = data.frame(Tech = c('Wind', 'Solar', 'Storage', 'Storage ($100 per kWh)', 'Storage (1/10th cap cost)', 'Trans converter pair', 'Trans line (land)', 'Trans line (sea)'),
                        capacity_fixed_cost_per_kW = NA,
                        capacity_fixed_cost_per_kWh = NA,
                        capacity_fixed_cost_per_kW_per_km = NA,
                        loss_percent = NA,
                        loss_percent_per_km = NA,
                        efficiency = NA,
                        decay_rate = NA,
                        charge_time = NA)

MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Wind'] = 
  (MEM_cost_assumptions$Value[MEM_cost_assumptions$Component == 'Wind' & MEM_cost_assumptions$`Value description` == 'Capital costs ($/kW)'] * 
   constants$discount_rate[constants$Tech == 'Wind'] * (1 + constants$discount_rate[constants$Tech == 'Wind'])^(constants$project_life[constants$Tech == 'Wind']) / 
   ((1 + constants$discount_rate[constants$Tech == 'Wind'])^(constants$project_life[constants$Tech == 'Wind']) - 1) +
   MEM_cost_assumptions$Value[MEM_cost_assumptions$Component == 'Wind' & MEM_cost_assumptions$`Value description` == 'Fixed O&M ($/kW-yr)']) / 
  constants$hours_per_year[constants$Tech == 'Wind']


MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Solar'] = 
  (MEM_cost_assumptions$Value[MEM_cost_assumptions$Component == 'Solar' & MEM_cost_assumptions$`Value description` == 'Capital costs ($/kW)'] * 
   constants$discount_rate[constants$Tech == 'Solar'] * (1 + constants$discount_rate[constants$Tech == 'Solar'])^(constants$project_life[constants$Tech == 'Solar']) / 
   ((1 + constants$discount_rate[constants$Tech == 'Solar'])^(constants$project_life[constants$Tech == 'Solar']) - 1) +
   MEM_cost_assumptions$Value[MEM_cost_assumptions$Component == 'Solar' & MEM_cost_assumptions$`Value description` == 'Fixed O&M ($/kW-yr)']) /
  constants$hours_per_year[constants$Tech == 'Solar']

MEM_inputs$capacity_fixed_cost_per_kWh[MEM_inputs$Tech == 'Storage'] = 
  (MEM_cost_assumptions$Value[MEM_cost_assumptions$Component == 'Storage' & MEM_cost_assumptions$`Value description` == 'Capital costs ($/kWh)'] * 
     constants$discount_rate[constants$Tech == 'Storage'] * (1 + constants$discount_rate[constants$Tech == 'Storage'])^(constants$project_life[constants$Tech == 'Storage']) / 
     ((1 + constants$discount_rate[constants$Tech == 'Storage'])^(constants$project_life[constants$Tech == 'Storage']) - 1) +
     MEM_cost_assumptions$Value[MEM_cost_assumptions$Component == 'Storage' & MEM_cost_assumptions$`Value description` == 'Fixed O&M ($/kWh-yr)']) /
  constants$hours_per_year[constants$Tech == 'Storage']

MEM_inputs$capacity_fixed_cost_per_kWh[MEM_inputs$Tech == 'Storage ($100 per kWh)'] = 
  (100 * 
     constants$discount_rate[constants$Tech == 'Storage'] * (1 + constants$discount_rate[constants$Tech == 'Storage'])^(constants$project_life[constants$Tech == 'Storage']) / 
     ((1 + constants$discount_rate[constants$Tech == 'Storage'])^(constants$project_life[constants$Tech == 'Storage']) - 1) +
     MEM_cost_assumptions$Value[MEM_cost_assumptions$Component == 'Storage' & MEM_cost_assumptions$`Value description` == 'Fixed O&M ($/kWh-yr)']) /
  constants$hours_per_year[constants$Tech == 'Storage']

MEM_inputs$capacity_fixed_cost_per_kWh[MEM_inputs$Tech == 'Storage (1/10th cap cost)'] = 
  (MEM_cost_assumptions$Value[MEM_cost_assumptions$Component == 'Storage' & MEM_cost_assumptions$`Value description` == 'Capital costs ($/kWh)'] / 10 * 
     constants$discount_rate[constants$Tech == 'Storage'] * (1 + constants$discount_rate[constants$Tech == 'Storage'])^(constants$project_life[constants$Tech == 'Storage']) / 
     ((1 + constants$discount_rate[constants$Tech == 'Storage'])^(constants$project_life[constants$Tech == 'Storage']) - 1) +
     MEM_cost_assumptions$Value[MEM_cost_assumptions$Component == 'Storage' & MEM_cost_assumptions$`Value description` == 'Fixed O&M ($/kWh-yr)']) /
  constants$hours_per_year[constants$Tech == 'Storage']

MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Trans converter pair'] = 
  (MEM_cost_assumptions$Value[MEM_cost_assumptions$Component == 'Trans' & MEM_cost_assumptions$`Value description` == 'Converter pair capital costs ($/kW)'] * 
     constants$discount_rate[constants$Tech == 'Trans'] * (1 + constants$discount_rate[constants$Tech == 'Trans'])^(constants$project_life[constants$Tech == 'Trans']) / 
     ((1 + constants$discount_rate[constants$Tech == 'Trans'])^(constants$project_life[constants$Tech == 'Trans']) - 1)) /
  constants$hours_per_year[constants$Tech == 'Trans']

MEM_inputs$capacity_fixed_cost_per_kW_per_km[MEM_inputs$Tech == 'Trans line (land)'] = 
  (MEM_cost_assumptions$Value[MEM_cost_assumptions$Component == 'Trans' & MEM_cost_assumptions$`Value description` == 'Overland line capital costs ($/kW per 1000 km)'] / 1000 * 
     constants$discount_rate[constants$Tech == 'Trans'] * (1 + constants$discount_rate[constants$Tech == 'Trans'])^(constants$project_life[constants$Tech == 'Trans']) / 
     ((1 + constants$discount_rate[constants$Tech == 'Trans'])^(constants$project_life[constants$Tech == 'Trans']) - 1)) /
  constants$hours_per_year[constants$Tech == 'Trans']

MEM_inputs$capacity_fixed_cost_per_kW_per_km[MEM_inputs$Tech == 'Trans line (sea)'] = 
  (MEM_cost_assumptions$Value[MEM_cost_assumptions$Component == 'Trans' & MEM_cost_assumptions$`Value description` == 'Submarine line capital costs ($/kW per 1000 km)'] / 1000 * 
     constants$discount_rate[constants$Tech == 'Trans'] * (1 + constants$discount_rate[constants$Tech == 'Trans'])^(constants$project_life[constants$Tech == 'Trans']) / 
     ((1 + constants$discount_rate[constants$Tech == 'Trans'])^(constants$project_life[constants$Tech == 'Trans']) - 1)) /
  constants$hours_per_year[constants$Tech == 'Trans']

MEM_inputs$loss_percent[MEM_inputs$Tech == 'Trans converter pair'] = MEM_cost_assumptions$Value[MEM_cost_assumptions$Component == 'Trans' & MEM_cost_assumptions$`Value description` == 'Converter pair loss (%)']
MEM_inputs$loss_percent_per_km[MEM_inputs$Tech == 'Trans line (land)'] = MEM_cost_assumptions$Value[MEM_cost_assumptions$Component == 'Trans' & MEM_cost_assumptions$`Value description` == 'Line loss (% per 1000 km)']/1000
MEM_inputs$loss_percent_per_km[MEM_inputs$Tech == 'Trans line (sea)'] = MEM_cost_assumptions$Value[MEM_cost_assumptions$Component == 'Trans' & MEM_cost_assumptions$`Value description` == 'Line loss (% per 1000 km)']/1000


MEM_inputs$efficiency[MEM_inputs$Tech %in% c('Storage','Storage ($100 per kWh)','Storage (1/10th cap cost)')] = 
  MEM_cost_assumptions$Value[MEM_cost_assumptions$Component == 'Storage' & MEM_cost_assumptions$`Value description` == 'Charging efficiency (%)']

MEM_inputs$decay_rate[MEM_inputs$Tech %in% c('Storage','Storage ($100 per kWh)','Storage (1/10th cap cost)')] = 
  MEM_cost_assumptions$Value[MEM_cost_assumptions$Component == 'Storage' & MEM_cost_assumptions$`Value description` == 'Decay rate (fraction per hour)']

MEM_inputs$charge_time[MEM_inputs$Tech %in% c('Storage','Storage ($100 per kWh)','Storage (1/10th cap cost)')] = 
  MEM_cost_assumptions$Value[MEM_cost_assumptions$Component == 'Storage' & MEM_cost_assumptions$`Value description` == 'Charging time (hours)']

save(MEM_inputs, 
     file = 'data/MEM_inputs.RData')


# now make a table of these derived quantities

MEM_inputs_table = MEM_inputs %>% dplyr::mutate_at(vars(capacity_fixed_cost_per_kW, capacity_fixed_cost_per_kWh,capacity_fixed_cost_per_kW_per_km,
                                                        loss_percent,loss_percent_per_km,charge_time,decay_rate,efficiency), funs(signif(., 4))) %>%
                                  tidyr::replace_na(list(capacity_fixed_cost_per_kW = '-', 
                                                         capacity_fixed_cost_per_kWh = '-',
                                                         capacity_fixed_cost_per_kW_per_km = '-',
                                                         loss_percent = '-',
                                                         loss_percent_per_km = '-',
                                                         charge_time = '-',
                                                         decay_rate = '-',
                                                         efficiency = '-')) 
                               
MEM_inputs_table %>% gt(auto_align = 'auto') %>%
  cols_label(capacity_fixed_cost_per_kW = md('Fixed cost ($/kW/h)'),
             capacity_fixed_cost_per_kWh = md('Fixed cost ($/kWh/h)'),
             capacity_fixed_cost_per_kW_per_km = md('Fixed cost ($/kW/h per km)'),
             loss_percent = md('Loss (%)'),
             loss_percent_per_km = md('Loss (% per km)'),
             charge_time = md('Charging time (hours)'),
             efficiency = md('Charging efficiency (%)'),
             decay_rate = md('Decay rate (fraction per hour)')) %>%
  opt_table_font(font = list(google_font(name = "Spectral"))) %>%
  cols_width(vars(Tech) ~ px(150),
             vars(capacity_fixed_cost_per_kW_per_km) ~ px(125),
             everything() ~ px(100)) %>%
  tab_options(table.font.size = px(18)) %>%
  cols_align(align = c('left'), columns = vars(Tech)) %>%
  gtsave(filename = paste0('table_MEM_inputs.png'),
         path = 'figs/')


}
