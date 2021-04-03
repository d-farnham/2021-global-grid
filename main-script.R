###########################################################
###### Part 0: Clear the workspace and load packages ######
###########################################################

rm(list = ls())
package.list = list('dplyr', 'plyr', 'rgdal', 'ggplot2', 'maptools', 'rgeos', 'cowplot', 
                    'readr', 'gt')
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
