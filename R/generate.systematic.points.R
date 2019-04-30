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
    #Check spacing is appropriate
    if(sspace > (bbox[["xmax"]]-bbox[["xmin"]]) || sspace > (bbox[["ymax"]]-bbox[["ymin"]])){
      warning(paste("The spacing allocated to strata ", strata.names[strat], " is larger than either or both of the x or y dimensions of the region. Cannot generate samplers in this strata.", sep = ""), call. = FALSE, immediate. = TRUE)
      transects[[strat]] <- NA
    }else{
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
  }
  #Put transects into a miltipart multipoint sf object defined by
  #the strata name
  counter <- 1
  transect.count <- 0
  #Find first strata where there are transects
  while(is.na(transects[[counter]]) && counter < length(transects)){
    counter <- counter + 1
    cat(counter)
  }
  #If there are some transects somewhere
  if(!is.na(transects[[counter]])){
    transect.count <- dim(transects[[counter]])[1]
    temp <- sf::st_sfc(transects[[counter]])
    #Now add in transects from other strata
    if(length(transects) > counter){
      for(strat in (counter+1):length(transects)){
        if(!is.na(transects[[strat]])){
          transect.count <- transect.count + dim(transects[[strat]])[1]
          temp <- c(temp, sf::st_sfc(transects[[strat]]))
        }
      }
    }
    all.transects <- st_sf(data.frame(strata = strata.names, geom = temp))
  }else{
    all.transects <- list()
  }
  #Make a survey object
  survey <- new(Class="Point.Transect.Survey", design = design@design, points = all.transects, no.samplers = transect.count, effort.allocation = design@effort.allocation, spacing = spacing, design.angle = design@design.angle, edge.protocol = design@edge.protocol)
  return(survey)
}
