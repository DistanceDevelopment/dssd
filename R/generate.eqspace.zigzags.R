#' @importFrom stats runif
#' @importFrom methods new
generate.eqspace.zigzags <- function(design, strata.id, no.samplers, line.length, spacing, by.spacing, complement = FALSE){
  region <- design@region
  #Get the current strata and spacing
  strata <- region@region$geometry[[strata.id]]
  #Spin round so design angle lies along x axis
  rot.angle.rad <- design@design.angle[strata.id]/180*pi
  theta <- ifelse(rot.angle.rad == 0, 0, 2*pi-rot.angle.rad)
  rot.mat <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), ncol = 2, byrow = FALSE)
  rot.strata <- strata*rot.mat
  #Buffer strata for plus sampling?
  if(design@edge.protocol[strata.id] == "plus"){
    rot.strata <- st_buffer(rot.strata, design@truncation)
  }
  #Find the minimum and maximum x and y values
  bbox <- st_bbox(rot.strata)
  if(!by.spacing){
    spacing <- (bbox[["xmax"]]-bbox[["xmin"]])/(no.samplers+1)#Hmm this needs some re-thinking!
  }
  start.x <- bbox[["xmin"]] + runif(1, 0, spacing) - spacing
  x.vals <- seq(start.x, (bbox[["xmax"]] + spacing), by = spacing)
  if(design@bounding.shape == "rectangle"){
    start.y <- rep(bbox[["ymin"]], length(x.vals))
    end.y <- rep(bbox[["ymax"]], length(x.vals))
  }else if(design@bounding.shape == "convex.hull"){
    #Going to need to clip lines and get y values, y values for the lines outside the region will be the same as the adjacent lines.
  }else{
    stop("Bounding shape invalid.", call. = FALSE)
  }
  #Randomise zig or zag at start
  random.start <- rbinom(1, 1, 0.5)
  #Create the lines
  lines <- list()
  counter <- 1
  if(complement){
    for(i in 1:(length(x.vals)-1)){
      #Do zig
      lines[[counter]] <- st_linestring(matrix(c(x.vals[i], x.vals[i+1], start.y[i], end.y[i+1]), ncol = 2))
      counter <- counter + 1
      #and complementing zag
      lines[[counter]] <- st_linestring(matrix(c(x.vals[i], x.vals[i+1], end.y[i], start.y[i+1]), ncol = 2))
      counter <- counter + 1
    }
  }else{
    zig <- ifelse(random.start == 1, TRUE, FALSE)
    for(i in 1:(length(x.vals)-1)){
      #Do zig
      if(zig){
        lines[[i]] <- st_linestring(matrix(c(x.vals[i], x.vals[i+1], start.y[i], end.y[i+1]), ncol = 2))
      }else{
        #zag
        lines[[i]] <- st_linestring(matrix(c(x.vals[i], x.vals[i+1], end.y[i], start.y[i+1]), ncol = 2))
      }
      #reverse for next time
      zig <- ifelse(zig, FALSE, TRUE)
    }

  }
  #keep everything within the polygon strata
  to.keep <- lapply(lines, st_intersection, y = rot.strata)
  #Rotate back again
  reverse.theta <- rot.angle.rad
  rot.mat.rev <- matrix(c(cos(reverse.theta), sin(reverse.theta), -sin(reverse.theta), cos(reverse.theta)), ncol = 2, byrow = FALSE)
  mat.mult <- function(x,y){return(x*y)}
  lines.unrotated <- lapply(to.keep, mat.mult, y=rot.mat.rev)
  transects <- lines.unrotated
  return(transects)
}
