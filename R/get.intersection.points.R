get.intersection.points <- function(strata.shape, x.vals, y.start, y.end){
  #Find convex hull
  chull <- sf::st_convex_hull(strata.shape)
  #Make sf line object
  lines <- list()
  for(i in seq(along = x.vals)){
    lines[[i]] <- sf::st_linestring(matrix(c(rep(x.vals[i],2),y.start[i], y.end[i]), ncol = 2))
  }
  #Clip lines
  intersections <- lapply(lines, FUN = sf::st_intersection, y = chull)
  #Get new y value vectors
  new.y.start <- new.y.end <- rep(NA, length(x.vals))
  for(i in seq(along = intersections)){
    if(length(intersections[[i]]) > 0){
      bbox <- sf::st_bbox((intersections[[i]]))
      new.y.start[i] <- bbox[["ymin"]]
      new.y.end[i] <- bbox[["ymax"]]
    }
  }
  n.vals <- length(x.vals)
  new.y.start[1] <- new.y.start[2]
  new.y.start[n.vals] <- new.y.start[(n.vals-1)]
  new.y.end[1] <- new.y.end[2]
  new.y.end[n.vals] <- new.y.end[(n.vals-1)]
  #Return new y value vectors
  return(list(start.y = new.y.start, end.y = new.y.end))
}
