generate.systematic.points <- function(design){
  # Get strata names
  region <- design@region
  if(length(region@strata.name) > 0){
    strata.names <- region@strata.name
    strata.no <- length(region@strata.name)
  }else{
    strata.names <- region@region.name
    strata.no <- 1
  }
  # Check if the user specified the no.of samplers rather than spacing
  if(length(design@spacing) == 0){
    if(length(strata.names) > 1){
      if(length(design@no.samplers) == 1){
        #Need to allocate no.samplers to strata
        if(length(design@effort.allocation) == 0){
          #Use area
          effort.allocation <- region@area/sum(region@area)
        }else{
          effort.allocation <- design@effort.allocation
        }
        no.samplers <- design@no.samplers*effort.allocation
      }else{
        no.samplers <- design@no.samplers
      }
    }
    #Calculate spacing from the number of desired samplers
    spacing <- abs(region@area)^0.5 / no.samplers^0.5
  }else{
    spacing <- design@spacing
  }
  #Store all point in a list
  transects <- list()
  #Main grid generation
  for (strat in seq(along = region@region$geometry)) {
    #Get the current strata and spacing
    strata <- region@region$geometry[[strat]]
    sspace <- spacing[strat]
    #Spin round so design angle lies along x axis
    rot.angle.rad <- design@design.angle[strat]/180*pi
    theta <- ifelse(rot.angle.rad == 0, 0, 2*pi-rot.angle.rad)
    rot.mat <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), ncol = 2, byrow = FALSE)
    rot.strata <- strata*rot.mat
    #Buffer strata for plus sampling?
    if(design@edge.protocol[strat] == "plus"){
      rot.strata <- st_buffer(rot.strata, design@truncation)
    }
    #Find the minimum and maximum x and y values
    bbox <- st_bbox(rot.strata)
    start.x <- bbox[["xmin"]] + runif(1, 0, sspace)
    start.y <- bbox[["ymin"]] + runif(1, 0, sspace)
    x.vals <- seq(start.x, bbox[["xmax"]], by = sspace)
    y.vals <- seq(start.y, bbox[["ymax"]], by = sspace)
    temp.coords <- expand.grid(x.vals, y.vals)
    #keep everything within the polygon strata
    points <- st_multipoint(as.matrix(temp.coords))
    to.keep <- st_intersection(points, rot.strata)
    #Rotate back again
    reverse.theta <- rot.angle.rad
    rot.mat.rev <- matrix(c(cos(reverse.theta), sin(reverse.theta), -sin(reverse.theta), cos(reverse.theta)), ncol = 2, byrow = FALSE)
    points.unrotated <- to.keep*rot.mat.rev
    transects[[strat]] <- points.unrotated
  }
  all.transects <- transects[[1]]
  if(length(transects) > 1){
    for(strat in 2:length(transects)){
      all.transects <- st_union(all.transects, transects[[strat]])
    }
  }
  return(all.transects)
}
