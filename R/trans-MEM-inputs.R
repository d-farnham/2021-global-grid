compute_trans_lengths_fraction_land = function(proposed_grid = 'data/proposed_grid_coords.RData'){
load(file = proposed_grid)


dist_calc = merge(proposed_grid_coords[c(seq(1, nrow(proposed_grid_coords)-1, by = 2)),],
                  proposed_grid_coords[c(seq(2, nrow(proposed_grid_coords), by = 2)),],
                  by = 'wire_IDs')

# find the distance of these wires

for(ii in 1:nrow(dist_calc)){
  dist_calc$dist[ii] = distGeo(c(dist_calc$center_lon.x[ii], dist_calc$center_lat.x[ii]), c(dist_calc$center_lon.y[ii], dist_calc$center_lat.y[ii]))/1000
}


# make the node to node transmission inputs for the MEM case input sheet
trans_case_inputs = dist_calc %>% dplyr::mutate(tech_name = paste0('node_', SREX_id.x, '_node_', SREX_id.y, '_trans'),
                                                tech_type = 'transmission',
                                                node_from = paste0('node_', SREX_id.x),
                                                node_to = paste0('node_', SREX_id.y),
                                                length = dist) %>%
                                  dplyr::select(tech_name, tech_type, node_from, node_to, length)


# let's estimate the fraction of land for each wire

lat_lon_wires = dist_calc %>% dplyr::select(wire_IDs, center_lon.x, center_lat.x, center_lon.y, center_lat.y)

lat_lon_wires = lat_lon_wires %>% dplyr::mutate(fraction_land = NA)
for(ii in 1:nrow(lat_lon_wires)){
  # Starting places
  p1 = lat_lon_wires[ii,] %>% dplyr::select(center_lon.x, center_lat.x)
  p2 = lat_lon_wires[ii,] %>% dplyr::select(center_lon.y, center_lat.y)

  
  intermediate_points = gcIntermediate(p1, p2, n = 100)
  
  # what fraction of points are over land
  # use a smaller subset of data first to play around with
  # let's just select the land-based cells
  land = maps::map("world",region = ".", plot = FALSE, fill = TRUE)
  land_IDs <- sapply(strsplit(land$names, ":"), function(x) x[1])
  land <- map2SpatialPolygons(land, IDs = land_IDs, proj4string = CRS("+proj=longlat +datum=WGS84"))
  land_pts <- SpatialPoints(intermediate_points[,c("lon","lat")],proj4string = CRS(proj4string(land)))
  land_ii = !is.na(over(land_pts,land))
  
  lat_lon_wires$fraction_land[ii] = (mean(land_ii))
}


trans_case_inputs = trans_case_inputs %>% dplyr::mutate(fraction_land = lat_lon_wires$fraction_land)
return(trans_case_inputs)
}