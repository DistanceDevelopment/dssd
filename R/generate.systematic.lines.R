generate.systematic.lines <- function(design){
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
  by.spacing <- FALSE
  if(length(design@spacing) == 0){
    if(length(strata.names) > 1){
      #Work out effort allocation if we need to
      if(length(design@no.samplers) == 1 || length(design@line.length) == 1){
        #Need to allocate no.samplers/line length to strata
        if(length(design@effort.allocation) == 0){
          #Use area
          effort.allocation <- region@area/sum(region@area)
        }else{
          effort.allocation <- design@effort.allocation
        }
      }
      if(length(design@no.samplers) == 1){
        no.samplers <- design@no.samplers*effort.allocation
      }else if(length(design@no.samplers) > 1){
        no.samplers <- design@no.samplers
      }else if(length(design@line.length) == 1){
        line.length <- design@line.length*effort.allocation
        #Calculate spacing here!
      }else if(length(design@line.length) > 1){
        line.length <- design@line.length
        #Calculate spacing here!
      }
    }
    #Calculate spacing from the number of desired samplers
    #spacing <- abs(region@area)^0.5 / no.samplers^0.5
  }else{
    spacing <- design@spacing
    by.spacing <- TRUE
  }
  #Store all lines in a list
  transects <- list()
  #Main grid generation
  for (strat in seq(along = region@region$geometry)) {
    #Get the current strata and spacing
    strata <- region@region$geometry[[strat]]
    if(by.spacing){
      sspace <- spacing[strat]
    }else{
      samps <- no.samplers[strat]
    }
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
    if(!by.spacing){
      sspace <- (bbox[["xmax"]]-bbox[["xmin"]])/(samps+1)
    }
    start.x <- bbox[["xmin"]] + runif(1, 0, sspace)
    start.y <- bbox[["ymin"]]
    end.y <- bbox[["ymax"]]
    x.vals <- seq(start.x, bbox[["xmax"]], by = sspace)
    #Create transects lines
    lines <- list()
    for(i in seq(along = x.vals)){
      lines[[i]] <- st_linestring(matrix(c(rep(x.vals[i],2),start.y, end.y),,2))
    }
    #keep everything within the polygon strata
    to.keep <- lapply(lines, st_intersection, y = rot.strata)
    #Rotate back again
    reverse.theta <- rot.angle.rad
    rot.mat.rev <- matrix(c(cos(reverse.theta), sin(reverse.theta), -sin(reverse.theta), cos(reverse.theta)), ncol = 2, byrow = FALSE)
    mat.mult <- function(x,y){return(x*y)}
    lines.unrotated <- lapply(to.keep, mat.mult, y=rot.mat.rev)
    transects[[strat]] <- lines.unrotated
  }
  #Put transects into a multipart, linestring/multilinestring objects
  #Need to retain transect IDs as well as strata for lines
  transect.count <- 0
  strata.id <- character(0)
  for(strat in seq(along = transects)){
    for(i in seq(along = transects[[strat]])){
      if(strat == 1 && i == 1){
        temp <- sf::st_sfc(transects[[strat]][[i]])
        transect.count <- 1
        strata.id <- strata.names[strat]
      }else{
        temp <- c(temp, sf::st_sfc(transects[[strat]][[i]]))
        transect.count <- transect.count + 1
        strata.id <- c(strata.id, strata.names[strat])
      }
    }
  }
  all.transects <- st_sf(data.frame(transect = 1:transect.count, strata = strata.id, geom = temp))
  #Make a survey object
  survey <- new(Class="Line.Transect.Survey", design = design@design, lines = all.transects, no.samplers = dim(all.transects)[1], line.length = sum(st_length(all.transects)), effort.allocation = design@effort.allocation, spacing = spacing, design.angle = design@design.angle, edge.protocol = design@edge.protocol)
  return(survey)
}
