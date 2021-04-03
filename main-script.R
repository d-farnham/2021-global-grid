###########################################################
###### Part 0: Clear the workspace and load packages ######
###########################################################

rm(list = ls())
package.list = list('dplyr', 'plyr', 'rgdal', 'ggplot2', 'maptools', 'rgeos', 'cowplot', 
                    'readr', 'gt', 'lubridate', 'readxl', 'tidyr', 'data.table')
source('R/load-packages.R') # load packages
#webshot::install_phantomjs()

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

# change thet working directory back out of the MEM directory
setwd('~/Google Drive/2021-global-grid')

rm(list = ls())
