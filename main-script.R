###########################################################
###### Part 0: Clear the workspace and load packages ######
###########################################################

rm(list = ls())
package.list = list('dplyr', 'plyr', 'rgdal', 'ggplot2', 'maptools', 'rgeos', 'cowplot', 
                    'readr', 'gt', 'lubridate', 'readxl', 'tidyr', 'data.table', 'stringr',
                    'tibble', 'igraph', 'ggrepel','webshot','mapproj', 'extRemes', 'geosphere',
                    'vecsets')
source('R/load-packages.R') # load packages
webshot::install_phantomjs()

###########################################################
## Part 1: Define and plot the regions and proposed grid ##
###########################################################

# Load the SREX region outlines and save them for later use
source('R/load-srex-regions.R')

# define srex polygons
source('R/prepare-srex-land-polygons.R')

# define the node coordinates of each srex region
source('R/define-region-node-locations.R')

# plot the SREX regions (SI figure 1)
source('R/make-srex-regions-figure.R')

# define preposed grid
source('R/define-proposed-grid.R')

# plot the SREX regional demands and proposed grid (figure 1)
source('R/make-srex-regions-demand-proposed-grid-figure.R')

# make a table of the srex regions, time offsets, and demand
source('R/make-regional-demand-table.R')
make_regional_demand_table(time_offsets_file = "data/srex_region_time_offsets.csv",
                           regional_demand_weights_file = "data/SREX_demand_weightings.csv")

rm(list = ls())

###########################################################
### Part 2: Prepare Macro Energy Model input timeseries ###
###########################################################
system("mkdir MEM/Input_Data")

# Demand -- computes the hourly mean
# NOTE: data source is https://github.com/d-farnham/EIA_Cleaned_Hourly_Electricity_Demand_Data/tree/master/data/release_2020_Oct/interconnects
source('R/prepare-n-hour-srex-demand.R')
prepare_n_hour_srex_demand(prototype_demand_series_file = 'data/demand_data/interconnects/EASTERN_IC.csv',
                           prototype_SREX_ID = 'ENA',
                           hour_multiple = 3)

# Solar and Wind -- computes the hourly mean 
source('R/prepare-n-hour-srex-wind-solar.R')
prepare_n_hour_srex_demand_wind_solar(hour_multiple = 3)

rm(list = ls())

###########################################################
##### Part 3: Prepare Macro Energy Model input sheets #####
###########################################################
system("mkdir MEM/case_inputs")
system("mkdir MEM/case_inputs/regional_grids")
system("mkdir MEM/case_inputs/global_grid")


# compute and save the costs and other parameters
# needed for MEM and save two tables
source('R/derive-MEM-parameters-and-make-tables.R')
derive_MEM_parameters_and_make_tables(MEM_cost_assumptions_file = "data/MEM_cost_assumptions.xlsx")

load(file = 'data/MEM_inputs.RData')

# (1) Each region having a separate electricity grid
source('R/case-input-setup-separate-regional-grids.R')

# we need to do this in batches of 5 regions at a time for computatioin reasons
parts = list(c(1:5),
             c(6:10),
             c(11:15),
             c(16:20),
             c(21:25),
             c(26))

for(ppart in 1:6){
  for(yyear in 2016:2018){
    MEM_case_input_setup_separate_regional_grids(
      year = yyear,
      delta_t = 3,
      save_file_name = paste0('MEM/case_inputs/regional_grids/regional_grids_separate_part_',
                              ppart,'_',yyear,'.csv'),
      data_path = 'Input_Data',
      output_path = paste0('Output_Data/regional_grids/separate_part_',ppart),
      demand_regions = parts[[ppart]],
      wind_regions = parts[[ppart]],
      solar_regions = parts[[ppart]],
      curtailment_regions = parts[[ppart]],
      lost_load_regions = parts[[ppart]],
      storage_regions = parts[[ppart]],
      storage_efficiency = MEM_inputs$efficiency[MEM_inputs$Tech == 'Storage'] / 100,
      storage_decay_rate = MEM_inputs$decay_rate[MEM_inputs$Tech == 'Storage'],
      storage_charging_time = MEM_inputs$charge_time[MEM_inputs$Tech == 'Storage'],
      solar_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Solar'],
      wind_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Wind'],
      storage_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kWh[MEM_inputs$Tech == 'Storage'],
      lost_load_var_cost = 10
    )
  }
}

# (2) A global grid where the demand is weighted and there are 26 possible solar genreators, 26 wind generators, and 5 batteries
# first find the top n (we want 5 in this case) demand regions
source('R/find-top-n-demand-regions.R')
top_five_demand_regions =
  find_top_n_demand_regions(
    n_sites = 5,
    demand_file = 'data/SREX_demand_weightings.csv')

source('R/case-input-setup-connected-global-grid.R')

for(yyear in 2016:2018){
  MEM_case_input_setup_connected_global_grid(
    year = yyear,
    delta_t = 3,
    save_file_name = paste0('MEM/case_inputs/global_grid/global_grid_connected_five_storage_',
                            yyear,'.csv'),
    data_path = 'Input_Data',
    output_path = 'Output_Data/global_grid/connected_five_storage',
    demand_regions = 1:26,
    wind_regions = 1:26,
    solar_regions = 1:26,
    curtailment_regions = 1:26,
    lost_load_regions = 1:26,
    storage_regions = top_five_demand_regions$top_demand,
    storage_efficiency = MEM_inputs$efficiency[MEM_inputs$Tech == 'Storage'] / 100,
    storage_decay_rate = MEM_inputs$decay_rate[MEM_inputs$Tech == 'Storage'],
    storage_charging_time = MEM_inputs$charge_time[MEM_inputs$Tech == 'Storage'],
    solar_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Solar'],
    wind_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Wind'],
    storage_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kWh[MEM_inputs$Tech == 'Storage'],
    lost_load_var_cost = 10,
    trans_var_cost = 1e-6,
    line_loss = MEM_inputs$loss_percent_per_km[MEM_inputs$Tech == 'Trans line (land)'] / 100,
    converter_pair_loss = MEM_inputs$loss_percent[MEM_inputs$Tech == 'Trans converter pair'] / 100,
    line_cost_land = MEM_inputs$capacity_fixed_cost_per_kW_per_km[MEM_inputs$Tech == 'Trans line (land)'],
    line_cost_sea = MEM_inputs$capacity_fixed_cost_per_kW_per_km[MEM_inputs$Tech == 'Trans line (sea)'],
    converter_pair_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Trans converter pair']
  )
}

# (2b) A global grid where the demand is weighted and there are 26 possible solar genreators, 26 wind generators, and 5 batteries
#      but where we do not allow cross ocean nor cross equator transmission

source('R/case-input-setup-connected-global-grid-no-cross-ocean-cross-equator-trans.R')

for(yyear in 2016:2018){
  MEM_case_input_setup_connected_global_grid_no_cross_ocean_cross_equator_trans(
    year = yyear,
    delta_t = 3,
    save_file_name = paste0('MEM/case_inputs/global_grid/global_grid_connected_five_storage_no_cross_ocean_cross_equator_trans_lines_',
                            yyear,'.csv'),
    data_path = 'Input_Data',
    output_path = 'Output_Data/global_grid/connected_five_storage_no_cross_ocean_cross_equator_trans_lines',
    demand_regions = 1:26,
    wind_regions = 1:26,
    solar_regions = 1:26,
    curtailment_regions = 1:26,
    lost_load_regions = 1:26,
    storage_regions = top_five_demand_regions$top_demand,
    storage_efficiency = MEM_inputs$efficiency[MEM_inputs$Tech == 'Storage'] / 100,
    storage_decay_rate = MEM_inputs$decay_rate[MEM_inputs$Tech == 'Storage'],
    storage_charging_time = MEM_inputs$charge_time[MEM_inputs$Tech == 'Storage'],
    solar_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Solar'],
    wind_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Wind'],
    storage_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kWh[MEM_inputs$Tech == 'Storage'],
    lost_load_var_cost = 10,
    trans_var_cost = 1e-6,
    line_loss = MEM_inputs$loss_percent_per_km[MEM_inputs$Tech == 'Trans line (land)'] / 100,
    converter_pair_loss = MEM_inputs$loss_percent[MEM_inputs$Tech == 'Trans converter pair'] / 100,
    line_cost_land = MEM_inputs$capacity_fixed_cost_per_kW_per_km[MEM_inputs$Tech == 'Trans line (land)'],
    line_cost_sea = MEM_inputs$capacity_fixed_cost_per_kW_per_km[MEM_inputs$Tech == 'Trans line (sea)'],
    converter_pair_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Trans converter pair']
  )
}

rm(list = ls())


###########################################################
### Part 4: Solve the prepared Macro Energy Model cases ###
###########################################################
# change thet working directory to the MEM directory
setwd('MEM')

# make sure we are using the correct version of python
Sys.setenv(PATH = paste("/anaconda3/bin", Sys.getenv("PATH"), sep=":"))

# check version -- should be 3.7.x
system('python --version')

# optimize the regional grids
system('sh run_MEM_for_regional_grids.sh')

# optimize the global grid
system('sh run_MEM_for_global_grid.sh')

# optimize the global grid w/o cross ocean and cross equator trans lines
system('sh run_MEM_for_global_grid_no_cross_ocean_cross_equator_trans.sh')

# change thet working directory back out of the MEM directory
setwd('~/Google Drive/2021-global-grid')

###########################################################
## Part 5: Prepare other Macro Energy Model input sheets ##
###########################################################

# (3) Same as (1) but with storage costs being 1/10th the cost of baseline
source('R/case-input-setup-separate-regional-grids.R')

load(file = 'data/MEM_inputs.RData')

# we need to do this in batches of 5 regions at a time for computatioin reasons
parts = list(c(1:5),
             c(6:10),
             c(11:15),
             c(16:20),
             c(21:25),
             c(26))

for(ppart in 1:6){
  for(yyear in 2016:2018){
    MEM_case_input_setup_separate_regional_grids(
      year = yyear,
      delta_t = 3,
      save_file_name = paste0('MEM/case_inputs/regional_grids/regional_grids_separate_one_tenth_cost_storage_part_',
                              ppart,'_',yyear,'.csv'),
      data_path = 'Input_Data',
      output_path = paste0('Output_Data/regional_grids/separate_one_tenth_cost_storage_part_',ppart),
      demand_regions = parts[[ppart]],
      wind_regions = parts[[ppart]],
      solar_regions = parts[[ppart]],
      curtailment_regions = parts[[ppart]],
      lost_load_regions = parts[[ppart]],
      storage_regions = parts[[ppart]],
      storage_efficiency = MEM_inputs$efficiency[MEM_inputs$Tech == 'Storage'] / 100,
      storage_decay_rate = MEM_inputs$decay_rate[MEM_inputs$Tech == 'Storage'],
      storage_charging_time = MEM_inputs$charge_time[MEM_inputs$Tech == 'Storage'],
      solar_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Solar'],
      wind_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Wind'],
      storage_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kWh[MEM_inputs$Tech == 'Storage (1/10th cap cost)'],
      lost_load_var_cost = 10
    )
  }
}


# (4) Same as (2) but with storage costs being 1/10th the cost of baseline
source('R/find-top-n-demand-regions.R')
top_five_demand_regions =
  find_top_n_demand_regions(
    n_sites = 5,
    demand_file = 'data/SREX_demand_weightings.csv')

source('R/case-input-setup-connected-global-grid.R')

for(yyear in 2016:2018){
  MEM_case_input_setup_connected_global_grid(
    year = yyear,
    delta_t = 3,
    save_file_name = paste0('MEM/case_inputs/global_grid/global_grid_connected_five_one_tenth_cost_storage_',
                            yyear,'.csv'),
    data_path = 'Input_Data',
    output_path = 'Output_Data/global_grid/connected_five_one_tenth_cost_storage',
    demand_regions = 1:26,
    wind_regions = 1:26,
    solar_regions = 1:26,
    curtailment_regions = 1:26,
    lost_load_regions = 1:26,
    storage_regions = top_five_demand_regions$top_demand,
    storage_efficiency = MEM_inputs$efficiency[MEM_inputs$Tech == 'Storage'] / 100,
    storage_decay_rate = MEM_inputs$decay_rate[MEM_inputs$Tech == 'Storage'],
    storage_charging_time = MEM_inputs$charge_time[MEM_inputs$Tech == 'Storage'],
    solar_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Solar'],
    wind_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Wind'],
    storage_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kWh[MEM_inputs$Tech == 'Storage (1/10th cap cost)'],
    lost_load_var_cost = 10,
    trans_var_cost = 1e-6,
    line_loss = MEM_inputs$loss_percent_per_km[MEM_inputs$Tech == 'Trans line (land)'] / 100,
    converter_pair_loss = MEM_inputs$loss_percent[MEM_inputs$Tech == 'Trans converter pair'] / 100,
    line_cost_land = MEM_inputs$capacity_fixed_cost_per_kW_per_km[MEM_inputs$Tech == 'Trans line (land)'],
    line_cost_sea = MEM_inputs$capacity_fixed_cost_per_kW_per_km[MEM_inputs$Tech == 'Trans line (sea)'],
    converter_pair_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Trans converter pair']
  )
}


# (5) Same as (1) but with storage costs being 100 $/kWh the cost of baseline

for(ppart in 1:6){
  for(yyear in 2016:2018){
    MEM_case_input_setup_separate_regional_grids(
      year = yyear,
      delta_t = 3,
      save_file_name = paste0('MEM/case_inputs/regional_grids/regional_grids_separate_one_hundred_cost_storage_part_',
                              ppart,'_',yyear,'.csv'),
      data_path = 'Input_Data',
      output_path = paste0('Output_Data/regional_grids/separate_one_hundred_cost_storage_part_',ppart),
      demand_regions = parts[[ppart]],
      wind_regions = parts[[ppart]],
      solar_regions = parts[[ppart]],
      curtailment_regions = parts[[ppart]],
      lost_load_regions = parts[[ppart]],
      storage_regions = parts[[ppart]],
      storage_efficiency = MEM_inputs$efficiency[MEM_inputs$Tech == 'Storage'] / 100,
      storage_decay_rate = MEM_inputs$decay_rate[MEM_inputs$Tech == 'Storage'],
      storage_charging_time = MEM_inputs$charge_time[MEM_inputs$Tech == 'Storage'],
      solar_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Solar'],
      wind_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Wind'],
      storage_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kWh[MEM_inputs$Tech == 'Storage ($100 per kWh)'],
      lost_load_var_cost = 10
    )
  }
}

# (6) Same as (2) but with storage costs being 100 $/kWh the cost of baseline

for(yyear in 2016:2018){
  MEM_case_input_setup_connected_global_grid(
    year = yyear,
    delta_t = 3,
    save_file_name = paste0('MEM/case_inputs/global_grid/global_grid_connected_five_one_hundred_cost_storage_',
                            yyear,'.csv'),
    data_path = 'Input_Data',
    output_path = 'Output_Data/global_grid/connected_five_one_hundred_cost_storage',
    demand_regions = 1:26,
    wind_regions = 1:26,
    solar_regions = 1:26,
    curtailment_regions = 1:26,
    lost_load_regions = 1:26,
    storage_regions = top_five_demand_regions$top_demand,
    storage_efficiency = MEM_inputs$efficiency[MEM_inputs$Tech == 'Storage'] / 100,
    storage_decay_rate = MEM_inputs$decay_rate[MEM_inputs$Tech == 'Storage'],
    storage_charging_time = MEM_inputs$charge_time[MEM_inputs$Tech == 'Storage'],
    solar_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Solar'],
    wind_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Wind'],
    storage_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kWh[MEM_inputs$Tech == 'Storage ($100 per kWh)'],
    lost_load_var_cost = 10,
    trans_var_cost = 1e-6,
    line_loss = MEM_inputs$loss_percent_per_km[MEM_inputs$Tech == 'Trans line (land)'] / 100,
    converter_pair_loss = MEM_inputs$loss_percent[MEM_inputs$Tech == 'Trans converter pair'] / 100,
    line_cost_land = MEM_inputs$capacity_fixed_cost_per_kW_per_km[MEM_inputs$Tech == 'Trans line (land)'],
    line_cost_sea = MEM_inputs$capacity_fixed_cost_per_kW_per_km[MEM_inputs$Tech == 'Trans line (sea)'],
    converter_pair_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Trans converter pair']
  )
}

# (7) Same as (2) but remove the top 5 energy density regions for each wind and solar
source('R/find-top-n-wind-solar-energy-density-regions.R')

top_five_regions = list('2016' = NA,
                        '2017' = NA,
                        '2018' = NA)

top_five_regions[['2016']] =
  find_top_n_wind_solar_energy_density_regions(
    year = 2016,
    n_sites = 5,
    solar_file = 'data/capacity_factors_wind_solar/SREXs_capacity.csv',
    wind_file = 'data/capacity_factors_wind_solar/SREXw_capacity.csv')

top_five_regions[['2017']] =
  find_top_n_wind_solar_energy_density_regions(
    year = 2017,
    n_sites = 5,
    solar_file = 'data/capacity_factors_wind_solar/SREXs_capacity.csv',
    wind_file = 'data/capacity_factors_wind_solar/SREXw_capacity.csv')

top_five_regions[['2018']] =
  find_top_n_wind_solar_energy_density_regions(
    year = 2018,
    n_sites = 5,
    solar_file = 'data/capacity_factors_wind_solar/SREXs_capacity.csv',
    wind_file = 'data/capacity_factors_wind_solar/SREXw_capacity.csv')

for(yyear in 2016:2018){
  MEM_case_input_setup_connected_global_grid(
    year = yyear,
    delta_t = 3,
    save_file_name = paste0('MEM/case_inputs/global_grid/global_grid_connected_five_storage_exclude_top_5_wind_and_top_5_solar_density_',
                            yyear,'.csv'),
    data_path = 'Input_Data',
    output_path = 'Output_Data/global_grid/connected_five_storage_exclude_top_5_wind_and_top_5_solar_density',
    demand_regions = 1:26,
    wind_regions = vecsets::vsetdiff(1:26, top_five_regions[[as.character(yyear)]]$top_wind),
    solar_regions = vecsets::vsetdiff(1:26, top_five_regions[[as.character(yyear)]]$top_solar),
    curtailment_regions = 1:26,
    lost_load_regions = 1:26,
    storage_regions = top_five_demand_regions$top_demand,
    storage_efficiency = MEM_inputs$efficiency[MEM_inputs$Tech == 'Storage'] / 100,
    storage_decay_rate = MEM_inputs$decay_rate[MEM_inputs$Tech == 'Storage'],
    storage_charging_time = MEM_inputs$charge_time[MEM_inputs$Tech == 'Storage'],
    solar_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Solar'],
    wind_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Wind'],
    storage_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kWh[MEM_inputs$Tech == 'Storage'],
    lost_load_var_cost = 10,
    trans_var_cost = 1e-6,
    line_loss = MEM_inputs$loss_percent_per_km[MEM_inputs$Tech == 'Trans line (land)'] / 100,
    converter_pair_loss = MEM_inputs$loss_percent[MEM_inputs$Tech == 'Trans converter pair'] / 100,
    line_cost_land = MEM_inputs$capacity_fixed_cost_per_kW_per_km[MEM_inputs$Tech == 'Trans line (land)'],
    line_cost_sea = MEM_inputs$capacity_fixed_cost_per_kW_per_km[MEM_inputs$Tech == 'Trans line (sea)'],
    converter_pair_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Trans converter pair']
  )
}
rm('top_five_regions')
# (6) Same as (2) but remove the top 5 energy generating regions for each wind and solar
# NOTE: You need to have run MEM on the baseline connectted scenarios before you can successfully run the lines below 

source('R/find-top-n-wind-solar-energy-generating-regions.R')
top_five_regions = list('2016' = NA,
                        '2017' = NA,
                        '2018' = NA)

top_five_regions[['2016']] =
  find_top_n_wind_solar_energy_generating_regions(
    n_sites = 5,
    MEM_output_file = list.files("MEM/Output_Data/global_grid/connected_five_storage/global_grid_2016/",
                                 pattern = '.xlsx',
                                 full.names = TRUE))

top_five_regions[['2017']] =
  find_top_n_wind_solar_energy_generating_regions(
    n_sites = 5,
    MEM_output_file = list.files("MEM/Output_Data/global_grid/connected_five_storage/global_grid_2017/",
                                 pattern = '.xlsx',
                                 full.names = TRUE))

top_five_regions[['2018']] =
  find_top_n_wind_solar_energy_generating_regions(
    n_sites = 5,
    MEM_output_file = list.files("MEM/Output_Data/global_grid/connected_five_storage/global_grid_2018/",
                                 pattern = '.xlsx',
                                 full.names = TRUE))


for(yyear in 2016:2018){
  MEM_case_input_setup_connected_global_grid(
    year = yyear,
    delta_t = 3,
    save_file_name = paste0('MEM/case_inputs/global_grid/global_grid_connected_five_storage_exclude_top_5_wind_and_top_5_solar_generating_',
                            yyear,'.csv'),
    data_path = 'Input_Data',
    output_path = 'Output_Data/global_grid/connected_five_storage_exclude_top_5_wind_and_top_5_solar_generating',
    demand_regions = 1:26,
    wind_regions = vecsets::vsetdiff(1:26, top_five_regions[[as.character(yyear)]]$top_wind),
    solar_regions = vecsets::vsetdiff(1:26, top_five_regions[[as.character(yyear)]]$top_solar),
    curtailment_regions = 1:26,
    lost_load_regions = 1:26,
    storage_regions = top_five_demand_regions$top_demand,
    storage_efficiency = MEM_inputs$efficiency[MEM_inputs$Tech == 'Storage'] / 100,
    storage_decay_rate = MEM_inputs$decay_rate[MEM_inputs$Tech == 'Storage'],
    storage_charging_time = MEM_inputs$charge_time[MEM_inputs$Tech == 'Storage'],
    solar_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Solar'],
    wind_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Wind'],
    storage_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kWh[MEM_inputs$Tech == 'Storage'],
    lost_load_var_cost = 10,
    trans_var_cost = 1e-6,
    line_loss = MEM_inputs$loss_percent_per_km[MEM_inputs$Tech == 'Trans line (land)'] / 100,
    converter_pair_loss = MEM_inputs$loss_percent[MEM_inputs$Tech == 'Trans converter pair'] / 100,
    line_cost_land = MEM_inputs$capacity_fixed_cost_per_kW_per_km[MEM_inputs$Tech == 'Trans line (land)'],
    line_cost_sea = MEM_inputs$capacity_fixed_cost_per_kW_per_km[MEM_inputs$Tech == 'Trans line (sea)'],
    converter_pair_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Trans converter pair']
  )
}

rm(list = ls())
###########################################################
## Part 6: Solve the additional Macro Energy Model cases ##
###########################################################
setwd('MEM')

# make sure we are using the correct version of python
Sys.setenv(PATH = paste("/anaconda3/bin", Sys.getenv("PATH"), sep=":"))

# run the regional grids
system("sh run_MEM_for_regional_grids_batch_2.sh")

# run the global grid
system("sh run_MEM_for_global_grid_batch_2.sh")

setwd('~/Google Drive/2021-global-grid')

###########################################################
### Part 7: Process and plot Macro Energy Model outputs ###
###########################################################

# define our color palette
my_colors = data.frame(color = c('green', 'brown', 'black', 'pink', 'gray', 'orange'),
                       value = c('#f16913', '#8c2d04', 'black', '#e7298a', 'gray', '#238b45'))

# Plot the cost and capacities (Figure 2)
source('R/plot-cost-capacity.R')
plot_cost_capacity(year = 2018,
                   gg_output_file = 'connected_five_storage/global_grid_',
                   rg_output_file = 'separate_part_',
                   plot_name = 'cost_capacity')

plot_cost_capacity(year = 2017,
                   gg_output_file = 'connected_five_storage/global_grid_',
                   rg_output_file = 'separate_part_',
                   plot_name = 'cost_capacity')

plot_cost_capacity(year = 2016,
                   gg_output_file = 'connected_five_storage/global_grid_',
                   rg_output_file = 'separate_part_',
                   plot_name = 'cost_capacity')


# Plot the capacities and import (Figure 3)
source('R/plot-gen-capacity-import.R')
plot_wind_solar_capacity_import(year = 2018,
                                gg_output_file = 'connected_five_storage/global_grid_',
                                plot_name = 'capacity_import')

plot_wind_solar_capacity_import(year = 2017,
                                gg_output_file = "connected_five_storage/global_grid_",
                                plot_name = 'capacity_import')

plot_wind_solar_capacity_import(year = 2016,
                                gg_output_file = "connected_five_storage/global_grid_",
                                plot_name = 'capacity_import')


# plot the mean transmission and import/export during various UTC times for a week in June vs. a week in December (figure for SI)
source('R/plot-power-flows.R')
plot_power_flows(year = 2018,
                 gg_output_file = 'connected_five_storage/global_grid_',
                 hour_a = 4.5,
                 hour_b = 13.5,
                 hour_c = 22.5,
                 date_a = as.POSIXct('2018-12-21', tz = 'UTC'),
                 date_b = as.POSIXct('2018-06-21', tz = 'UTC'))

plot_power_flows(year = 2017,
                 gg_output_file = 'connected_five_storage/global_grid_',
                 hour_a = 4.5,
                 hour_b = 13.5,
                 hour_c = 22.5,
                 date_a = as.POSIXct('2017-12-21', tz = 'UTC'),
                 date_b = as.POSIXct('2017-06-21', tz = 'UTC'))

plot_power_flows(year = 2016,
                 gg_output_file = 'connected_five_storage/global_grid_',
                 hour_a = 4.5,
                 hour_b = 13.5,
                 hour_c = 22.5,
                 date_a = as.POSIXct('2016-12-21', tz = 'UTC'),
                 date_b = as.POSIXct('2016-06-21', tz = 'UTC'))


# plot the costs of alternative scenarios (Figure 4)
source('R/plot-costs-alternative-scenarios.R')
plot_costs_alternative_scenarios(
  years = c(2016:2018),
  global_grid_run_ID = 'connected_five_storage',
  storage_100_global_grid_run_ID = 'connected_five_one_hundred_cost_storage',
  storage_1_10th_global_grid_run_ID = 'connected_five_one_tenth_cost_storage',
  exclude_high_density_regions_global_grid_run_ID = 'connected_five_storage_exclude_top_5_wind_and_top_5_solar_density',
  exclude_high_gen_regions_global_grid_run_ID = 'connected_five_storage_exclude_top_5_wind_and_top_5_solar_generating',
  regional_grids_run_ID = 'separate',
  storage_100_regional_grids_run_ID = 'separate_one_hundred_cost_storage',
  storage_1_10th_regional_grids_run_ID = 'separate_one_tenth_cost_storage')


# Plot the cost and capacities for the runs with no cross ocean cross equator trans (SI figures)
plot_cost_capacity(year = 2018,
                   gg_output_file = 'connected_five_storage_no_cross_ocean_cross_equator_trans_lines/global_grid_',
                   rg_output_file = 'separate_part_',
                   plot_name = 'cost_capacity_no_cross_ocean_cross_equator_trans')

plot_cost_capacity(year = 2017,
                   gg_output_file = 'connected_five_storage_no_cross_ocean_cross_equator_trans_lines/global_grid_',
                   rg_output_file = 'separate_part_',
                   plot_name = 'cost_capacity_no_cross_ocean_cross_equator_trans')

plot_cost_capacity(year = 2016,
                   gg_output_file = 'connected_five_storage_no_cross_ocean_cross_equator_trans_lines/global_grid_',
                   rg_output_file = 'separate_part_',
                   plot_name = 'cost_capacity_no_cross_ocean_cross_equator_trans')

# Plot the capacities and import for the runs with no cross ocean cross equator trans (SI figures)
source('R/plot-gen-capacity-import-no-import-labels.R')
plot_wind_solar_capacity_import_no_import_labels(year = 2018,
                                                 gg_output_file = 'connected_five_storage_no_cross_ocean_cross_equator_trans_lines/global_grid_',
                                                 plot_name = 'capacity_import_no_cross_ocean_cross_equator_trans')

plot_wind_solar_capacity_import_no_import_labels(year = 2017,
                                                 gg_output_file = "connected_five_storage_no_cross_ocean_cross_equator_trans_lines/global_grid_",
                                                 plot_name = 'capacity_import_no_cross_ocean_cross_equator_trans')

plot_wind_solar_capacity_import_no_import_labels(year = 2016,
                                                 gg_output_file = "connected_five_storage_no_cross_ocean_cross_equator_trans_lines/global_grid_",
                                                 plot_name = 'capacity_import_no_cross_ocean_cross_equator_trans')


# plot the capacity factors and MEM installed capacities (SI figure)
source('R/plot-wind-solar-CFs-capacities.R')
plot_wind_solar_CFs_capacities(hour_multiple = 3,
                               years = c(2016:2018),
                               global_grid_run_ID = 'connected_five_storage')


# plot the capacity facotrs and MEM installed capacities (SI figure)
source('R/plot-wind-solar-CF-demand-cors.R')
plot_wind_solar_demand_cors(hour_multiple = 3,
                            years = c(2016:2018),
                            prototype_SREX_ID = 'ENA')

# plot the correlation of aggregate global demand with wind/solar and installed capacity
source('R/plot-agg-demand-wind-solar-capacities.R')
plot_agg_demand_wind_solar_capacities(hour_multiple = 3,
                                      years = c(2016:2018),
                                      prototype_SREX_ID = 'ENA',
                                      global_grid_run_ID = 'connected_five_storage')

# plot the tail dependence between wind/solar gen and demand and installed capacity
source('R/plot-tail-dep-capacities.R')
plot_tail_dep_capacities(hour_multiple = 3,
                         years = c(2016:2018),
                         prototype_SREX_ID = 'ENA',
                         global_grid_run_ID = 'connected_five_storage')

# plot the distance to the top 5 demand regions and installed wind/solar capacities
source('R/plot-dist-to-regions-capacities.R')
plot_dist_to_regions_capacities(proposed_grid = 'data/proposed_grid_coords.RData',
                                years = c(2016:2018),
                                global_grid_run_ID = 'connected_five_storage')

rm(list = ls())
###########################################################
## Part 8: Prepare other Macro Energy Model input sheets ##
###########################################################

# For the SI: How many storage's are needed to 
#             approximate total system cost
system("mkdir MEM/case_inputs/assess_how_many_storage")

load(file = 'data/MEM_inputs.RData')

source('R/find-top-n-demand-regions.R')
top_10_demand_regions =
  find_top_n_demand_regions(
    n_sites = 10,
    demand_file = 'data/SREX_demand_weightings.csv')

source('R/case-input-setup-connected-global-grid.R')

for(num_stor in c(2:4,6:8)){
  for(yyear in 2016:2018){
    MEM_case_input_setup_connected_global_grid(
      year = yyear,
      delta_t = 3,
      save_file_name = paste0('MEM/case_inputs/assess_how_many_storage/global_grid_connected_',
                              num_stor,'_storage_',yyear,'.csv'),
      data_path = 'Input_Data',
      output_path = paste0('Output_Data/global_grid/assess_how_many_storage/connected_',num_stor,'_storage'),
      demand_regions = 1:26,
      wind_regions = 1:26,
      solar_regions = 1:26,
      curtailment_regions = 1:26,
      lost_load_regions = 1:26,
      storage_regions = top_10_demand_regions$top_demand[1:num_stor],
      storage_efficiency = MEM_inputs$efficiency[MEM_inputs$Tech == 'Storage'] / 100,
      storage_decay_rate = MEM_inputs$decay_rate[MEM_inputs$Tech == 'Storage'],
      storage_charging_time = MEM_inputs$charge_time[MEM_inputs$Tech == 'Storage'],
      solar_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Solar'],
      wind_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Wind'],
      storage_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kWh[MEM_inputs$Tech == 'Storage'],
      lost_load_var_cost = 10,
      trans_var_cost = 1e-6,
      line_loss = MEM_inputs$loss_percent_per_km[MEM_inputs$Tech == 'Trans line (land)'] / 100,
      converter_pair_loss = MEM_inputs$loss_percent[MEM_inputs$Tech == 'Trans converter pair'] / 100,
      line_cost_land = MEM_inputs$capacity_fixed_cost_per_kW_per_km[MEM_inputs$Tech == 'Trans line (land)'],
      line_cost_sea = MEM_inputs$capacity_fixed_cost_per_kW_per_km[MEM_inputs$Tech == 'Trans line (sea)'],
      converter_pair_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Trans converter pair']
    )
  }
}


# For the SI: Is using the largest 5 demand regions as
#             for storage lead to a near minimum 
#             approximate total system cost?
system("mkdir MEM/case_inputs/assess_which_storage")



source('R/case-input-setup-connected-global-grid.R')

set.seed(123)
for(rand_storage in 1:10){
  
  five_storage_regions = sample(x = 1:26, replace = FALSE, size = 5)
  
  for(yyear in 2016:2018){
    MEM_case_input_setup_connected_global_grid(
      year = yyear,
      delta_t = 3,
      save_file_name = paste0('MEM/case_inputs/assess_which_storage/global_grid_connected_five_random_storage_iter_',
                              rand_storage,'_',yyear,'.csv'),
      data_path = 'Input_Data',
      output_path = paste0('Output_Data/global_grid/assess_which_storage/connected_five_random_storage_iter_',
                           rand_storage),
      demand_regions = 1:26,
      wind_regions = 1:26,
      solar_regions = 1:26,
      curtailment_regions = 1:26,
      lost_load_regions = 1:26,
      storage_regions = five_storage_regions,
      storage_efficiency = MEM_inputs$efficiency[MEM_inputs$Tech == 'Storage'] / 100,
      storage_decay_rate = MEM_inputs$decay_rate[MEM_inputs$Tech == 'Storage'],
      storage_charging_time = MEM_inputs$charge_time[MEM_inputs$Tech == 'Storage'],
      solar_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Solar'],
      wind_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Wind'],
      storage_fixed_cost = MEM_inputs$capacity_fixed_cost_per_kWh[MEM_inputs$Tech == 'Storage'],
      lost_load_var_cost = 10,
      trans_var_cost = 1e-6,
      line_loss = MEM_inputs$loss_percent_per_km[MEM_inputs$Tech == 'Trans line (land)'] / 100,
      converter_pair_loss = MEM_inputs$loss_percent[MEM_inputs$Tech == 'Trans converter pair'] / 100,
      line_cost_land = MEM_inputs$capacity_fixed_cost_per_kW_per_km[MEM_inputs$Tech == 'Trans line (land)'],
      line_cost_sea = MEM_inputs$capacity_fixed_cost_per_kW_per_km[MEM_inputs$Tech == 'Trans line (sea)'],
      converter_pair_cost = MEM_inputs$capacity_fixed_cost_per_kW[MEM_inputs$Tech == 'Trans converter pair']
    )
  }
}

rm(list = ls())
###########################################################
#### Part 9: Solve additional Macro Energy Model cases ####
###########################################################
setwd('MEM')

# make sure we are using the correct version of python
Sys.setenv(PATH = paste("/anaconda3/bin", Sys.getenv("PATH"), sep=":"))

# run the global grid
system("sh run_MEM_for_global_grid_batch_3.sh")

setwd('~/Google Drive/2021-global-grid')

###########################################################
###### Part 10: Process and plot SI figs re: storage ######
###########################################################
# Plot figures that support our use of the five storages
source('R/plot-which-and-how-many-storage.R')
plot_which_and_how_many_storage(years = c(2016:2018),
                                gg_output_file = 'connected_five_storage/global_grid_')

rm(list = ls())
