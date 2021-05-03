
data(wrld_simpl)
load(file = 'data/SREX_regions.RData')

ssrex_polys_land = NA
for(rregion in unique(SREX_regions$LAB)){

  region0 = SREX_regions %>% dplyr::filter(LAB == rregion)
  
  
  p = Polygon(region0[c('long','lat')])
  ps = Polygons(list(p),1)
  sps = SpatialPolygons(list(ps))
  
  
  # find the intersecting area between the world land map and the srex regions
  srex_polys_land0 <- gIntersection(sps,
                                     wrld_simpl, 
                                     byid=FALSE)  %>% 
                      broom::tidy() %>%
                      dplyr::mutate(LAB = rregion,
                                    srex_id = unique(region0$SREX_id))
  
  if(rregion == unique(SREX_regions$LAB)[1]){srex_polys_land = srex_polys_land0}
  if(rregion != unique(SREX_regions$LAB)[1]){srex_polys_land = bind_rows(srex_polys_land,
                                                                         srex_polys_land0)}
}

srex_polys_land = srex_polys_land %>% dplyr::mutate(piece_LAB = paste0(piece,'_',LAB))

save(srex_polys_land, file = 'data/srex_polys_land.RData')

rm(list = ls())
