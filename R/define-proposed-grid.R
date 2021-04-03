
load(file = 'data/SREX_region_nodes_manual.RData')

proposed_grid = 
  data.frame(wire_end1 = c('ALA','ALA','ALA','CGI','CGI','CGI','CGI','WNA','WNA','CNA','CNA','ENA','CAM','CAM','AMZ','AMZ','AMZ',
                           'NEB','NEB','WSA','SAU','NAU','SEA','SEA','SAS','SAS','SAS','SAS','EAS','EAS','TIB','TIB','CAS','CAS',
                           'WSA','WAS','WAS','WAS','WAS','NAS','NAS','SAF','SAF','EAF','EAF','WAF','SAH','MED','CEU','SAF'),
             wire_end2 = c('CGI','WNA','NAS','NEU','ENA','CNA','WNA','CNA','CAM','ENA','CAM','CAM','AMZ','WSA','WSA','NEB','SSA',
                           'SSA','WAF','SSA','NAU','SEA','EAS','SAS','EAS','TIB','CAS','WAS','NAS','TIB','NAS','CAS','WAS','NAS',
                           'SAU','CEU','MED','SAH','EAF','NEU','CEU','EAF','WAF','SAH','WAF','SAH','MED','CEU','NEU','NAU')) %>%
  dplyr::mutate(wire_IDs = 1:n(),
                wire_end1 = as.character(wire_end1),
                wire_end2 = as.character(wire_end2))


proposed_grid_long = reshape2::melt(proposed_grid, 
                                    id.vars = 'wire_IDs',
                                    value.name = 'wire_end') %>%
  dplyr::select(-variable)


proposed_grid_coords = merge(proposed_grid_long,
                             SREX_region_nodes_manual %>% dplyr::select(LAB, SREX_id, center_lon, center_lat) %>% unique(),
                             by.x = 'wire_end',
                             by.y = 'LAB')

proposed_grid_coords = proposed_grid_coords %>% dplyr::arrange(wire_IDs, SREX_id)

save(proposed_grid_coords, file = 'data/proposed_grid_coords.RData')

rm(list = ls())
