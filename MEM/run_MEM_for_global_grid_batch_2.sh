#############################################################
######## Cheap storage (1/10th of the baseline cost) ########
#############################################################
# 2016
python Run_Case_Example_default.py case_inputs/global_grid/global_grid_connected_five_one_tenth_cost_storage_2016.csv

# 2017
python Run_Case_Example_default.py case_inputs/global_grid/global_grid_connected_five_one_tenth_cost_storage_2017.csv

# 2018
python Run_Case_Example_default.py case_inputs/global_grid/global_grid_connected_five_one_tenth_cost_storage_2018.csv


#############################################################
#####                100 $ per kWh storage              #####
#############################################################
# 2016
python Run_Case_Example_default.py case_inputs/global_grid/global_grid_connected_five_one_hundred_cost_storage_2016.csv

# 2017
python Run_Case_Example_default.py case_inputs/global_grid/global_grid_connected_five_one_hundred_cost_storage_2017.csv

# 2018
python Run_Case_Example_default.py case_inputs/global_grid/global_grid_connected_five_one_hundred_cost_storage_2018.csv


###############################################################
# No high density (top 5 wind & top 5 solar regions) capacity #
###############################################################
# 2016
python Run_Case_Example_default.py case_inputs/global_grid/global_grid_connected_five_storage_exclude_top_5_wind_and_top_5_solar_density_2016.csv

# 2017
python Run_Case_Example_default.py case_inputs/global_grid/global_grid_connected_five_storage_exclude_top_5_wind_and_top_5_solar_density_2017.csv

# 2018
python Run_Case_Example_default.py case_inputs/global_grid/global_grid_connected_five_storage_exclude_top_5_wind_and_top_5_solar_density_2018.csv

###############################################################
### No high gen (top 5 wind & top 5 solar regions) capacity ###
###############################################################
# 2016
python Run_Case_Example_default.py case_inputs/global_grid/global_grid_connected_five_storage_exclude_top_5_wind_and_top_5_solar_generating_2016.csv

# 2017
python Run_Case_Example_default.py case_inputs/global_grid/global_grid_connected_five_storage_exclude_top_5_wind_and_top_5_solar_generating_2017.csv

# 2018
python Run_Case_Example_default.py case_inputs/global_grid/global_grid_connected_five_storage_exclude_top_5_wind_and_top_5_solar_generating_2018.csv
