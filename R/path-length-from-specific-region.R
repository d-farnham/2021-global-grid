path_length_from_specific_region = function(g2, srex, num_srex, path_name){
  paths = all_shortest_paths(g2, srex, 1:num_srex)
  
  path_length = data.frame(srex = 1:num_srex)
  
  for(ii in 1:num_srex){
    path_temp = rep(as.numeric(paths$res[[ii]]), each=2)[-1]
    path_temp = path_temp[-length(path_temp)]
    path_length[ii,path_name] = sum(E(g2)$weight[get.edge.ids(g2, path_temp)])}
  
  return(path_length)
}