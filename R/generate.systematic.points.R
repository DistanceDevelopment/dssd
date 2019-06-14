generate.systematic.points <- function(design, strata.id, spacing, coverage.grid = FALSE, calc.cov.area = TRUE, clip.to.strata = TRUE, silent = FALSE){
  #Generates either random or systematic parallel lines
  region <- design@region
  #Get the current strata and spacing
  sf.column <- attr(region@region, "sf_column")
  strata <- region@region[[sf.column]][[strata.id]]
  sspace <- spacing
  #Spin round so design angle lies along x axis
  rot.angle.rad <- design@design.angle[strata.id]/180*pi
  theta <- ifelse(rot.angle.rad == 0, 0, 2*pi-rot.angle.rad)
  rot.mat <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), ncol = 2, byrow = FALSE)
  rot.strata <- strata*rot.mat
  #Buffer strata for plus sampling?
  if(design@edge.protocol[strata.id] == "plus"){
    rot.strata <- sf::st_buffer(rot.strata, design@truncation)
  }
  #Find the minimum and maximum x and y values
  bbox <- sf::st_bbox(rot.strata)
  #Check spacing is appropriate
  if(sspace > (bbox[["xmax"]]-bbox[["xmin"]]) || sspace > (bbox[["ymax"]]-bbox[["ymin"]])){
    if(!silent){
      warning(paste("The spacing allocated to strata ", strata.id, " is larger than either one or both of the x / y dimensions of the region. Cannot generate samplers in this strata.", sep = ""), call. = FALSE, immediate. = TRUE)
    }
    return(NULL)
  }else{
    if(coverage.grid){
      x.diff <- bbox[["xmax"]]-bbox[["xmin"]]
      y.diff <- bbox[["ymax"]]-bbox[["ymin"]]
      start.x <- bbox[["xmin"]] + (x.diff - floor(x.diff/sspace)*sspace)/2
      start.y <- bbox[["ymin"]] + (y.diff - floor(y.diff/sspace)*sspace)/2
    }else{
      start.x <- bbox[["xmin"]] + runif(1, 0, sspace)
      start.y <- bbox[["ymin"]] + runif(1, 0, sspace)
    }
    x.vals <- seq(start.x, bbox[["xmax"]], by = sspace)
    y.vals <- seq(start.y, bbox[["ymax"]], by = sspace)
    temp.coords <- expand.grid(x.vals, y.vals)
    #keep everything within the polygon strata
    points <- list()
    for(p in seq(along = temp.coords[,1])){
      points[[p]] <- sf::st_point(as.matrix(temp.coords)[p,])
    }
    #points <- sf::st_multipoint(as.matrix(temp.coords))
    to.keep <- lapply(points, FUN = sf::st_intersection, y = rot.strata)
    points.inside <- list()
    count <- 1
    for(p in seq(along = to.keep)){
      if(length(to.keep[[p]]) > 0){
        points.inside[[count]] <- to.keep[[p]]
        count <- count + 1
      }
    }
    #Rotate back again
    reverse.theta <- rot.angle.rad
    rot.mat.rev <- matrix(c(cos(reverse.theta), sin(reverse.theta), -sin(reverse.theta), cos(reverse.theta)), ncol = 2, byrow = FALSE)
    mat.mult <- function(x,y){return(x*y)}
    points.unrotated <- lapply(points.inside, mat.mult, y=rot.mat.rev)
    transects <- points.unrotated
  }
  if(calc.cov.area && !coverage.grid){
    cov.area.polys <- lapply(transects, FUN = sf::st_buffer, dist = design@truncation)
    if(clip.to.strata){
      cov.area.polys <- lapply(cov.area.polys, sf::st_intersection, y = strata)
    }
    return(list(transects = transects, cover.polys = cov.area.polys))
  }
  return(list(transects = transects, cover.polys = list()))
}
