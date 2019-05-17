#' @importFrom stats runif
#' @importFrom methods new
generate.parallel.lines <- function(design, strata.id, no.samplers, line.length, spacing, by.spacing){
  #Generates either random or systematic parallel lines
  region <- design@region
  sf.column <- attr(region@region, "sf_column")
  #Get the current strata and spacing
  strata <- region@region[[sf.column]][[strata.id]]
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
  if(!by.spacing && design@design[strata.id] == "systematic"){
    spacing <- (bbox[["xmax"]]-bbox[["xmin"]])/(no.samplers)
  }
  start.y <- bbox[["ymin"]]
  end.y <- bbox[["ymax"]]
  if(design@design[strata.id] == "systematic"){
    start.x <- bbox[["xmin"]] + runif(1, 0, spacing)
    x.vals <- seq(start.x, bbox[["xmax"]], by = spacing)
  }else if(design@design[strata.id] == "random"){
    x.vals <- runif(no.samplers, bbox[["xmin"]], bbox[["xmax"]])
  }
  #Create transects lines
  lines <- list()
  for(i in seq(along = x.vals)){
    lines[[i]] <- st_linestring(matrix(c(rep(x.vals[i],2),start.y, end.y), ncol = 2))
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
