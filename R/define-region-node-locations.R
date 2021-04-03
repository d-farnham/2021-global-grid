load(file = 'data/SREX_regions.RData')

# manually set the centroid for each region
region_centroid_manual = data.frame(SREX_id = c(1:5,
                                                6:10,
                                                11:15,
                                                16:20,
                                                21:26),
                                    center_lon = c(-130, -45, -115, -95, -75,
                                                   -85, -60, -45, -70, -60,
                                                   10, 25, 20, 5, 10,
                                                   35, 25, 110, 50, 65,
                                                   85, 110, 80, 110, 130, 145),
                                    center_lat = c(65, 60, 40, 40, 45,
                                                   15, 0, -10, -20, -30,
                                                   60, 50, 35, 20, 5,
                                                   5, -20, 60, 30, 35,
                                                   35, 30, 20, 5, -20, -35))

SREX_region_nodes_manual = merge(region_centroid_manual,
                                 SREX_regions,
                                 by = 'SREX_id') %>%
                           dplyr::arrange(order)

# find the centroids in order to place labels
SREX_region_nodes_manual = SREX_region_nodes_manual %>% dplyr::mutate(region_label = paste0(LAB,'(', SREX_id, ')'))

save(SREX_region_nodes_manual, file = 'data/SREX_region_nodes_manual.RData')