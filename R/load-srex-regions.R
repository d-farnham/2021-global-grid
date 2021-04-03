data(wrld_simpl)

# read in shapefiles
regions.rg <- readOGR(dsn = "data/referenceRegions", 
                      layer = 'referenceRegions')


proj4string(regions.rg) <- "+proj=longlat +datum=WGS84"


# let's exclude the "all" regions

regions.rg = regions.rg[!(regions.rg$LAB %in% c('ANT*', 'ARC*', 'CAR*', 'NTP*', 'STP*', 'ETP*', 'WIO*')),]
# note that readOGR will read the .prj file if it exists
print(proj4string(regions.rg))
# [1] " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# transform the data so that I can plot in ggplot


regions.rg@data$id = rownames(regions.rg@data)
regions.rg.points = fortify(regions.rg, region="id")
regions.rg.df = join(regions.rg.points, regions.rg@data, by="id")


# get SREX region id #
referenceRegions <- read.csv("data/referenceRegions.csv", header=FALSE) %>% setNames(c('name', 'LAB', 'SREX_id', 'V4', 'V5', 'V6', 'V7', 'V8', 'V9', 'V10')) %>%
  dplyr::select(c(LAB, SREX_id)) %>%
  dplyr::filter(!is.na(SREX_id))

SREX_regions = merge(regions.rg.df, referenceRegions, by = 'LAB') %>%
                    dplyr::select(-hole, -piece, -id, -group, -NAME, -USAGE)


# save this for MEM post-processing
save(SREX_regions, file = 'data/SREX_regions.RData')

rm(list = ls())

