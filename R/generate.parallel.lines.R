#' @importFrom stats runif
#' @importFrom methods new
generate.parallel.lines <- function(design, strata.id, no.samplers, line.length, spacing, by.spacing, return.cov.area, clip.to.strata){
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
  bbox <- sf::st_bbox(rot.strata)
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
    x.vals <- sort(x.vals)
  }
  #Create transects lines
  lines <- list()
  for(i in seq(along = x.vals)){
    lines[[i]] <- sf::st_linestring(matrix(c(rep(x.vals[i],2),start.y, end.y), ncol = 2))
  }
  #keep everything within the polygon strata
  to.keep <- lapply(lines, sf::st_intersection, y = rot.strata)
  #Calculate covered region - do it here as easier before unrotating!
  cover.polys <- list()
  if(return.cov.area){
    trunc <- design@truncation
    for(tr in seq(along = to.keep)){
      if(any(class(to.keep[[tr]]) == "LINESTRING")){
        bbox <- sf::st_bbox(to.keep[[1]])
        x.vals <- c(rep((bbox$xmin - trunc),2), rep((bbox$xmax + trunc),2), (bbox$xmin - trunc))
        y.vals <- c(bbox$ymin, rep(bbox$ymax,2), rep(bbox$ymin,2))
        cover.polys[[tr]] <- sf::st_polygon(list(matrix(c(x.vals, y.vals), ncol = 2)))
      }else if(any(class(to.keep[[tr]] = "MULTILINESTRING"))){
        #Need to iterate along the list
        temp <- list()
        for(part in seq(along = to.keep[[tr]])){
          line.mat <- to.keep[[tr]][[part]]
          lx <- line.mat[,1]
          ly <- line.mat[,2]
          px <- c(rep((lx[1] - trunc), 2), rep((lx[2] + trunc), 2), (lx[1] - trunc))
          py <- c(ly[1], rep(ly[2],2), rep(ly[1],2))
          temp[[part]] <- matrix(c(px, py), ncol = 2)
        }
        cover.polys[[tr]] <- sf::st_polygon(temp)
      }
    }
  }
  if(clip.to.strata){
    cover.polys <- lapply(cover.polys, sf::st_intersection, y = rot.strata)
  }
  #Rotate back again
  reverse.theta <- rot.angle.rad
  rot.mat.rev <- matrix(c(cos(reverse.theta), sin(reverse.theta), -sin(reverse.theta), cos(reverse.theta)), ncol = 2, byrow = FALSE)
  mat.mult <- function(x,y){return(x*y)}
  lines.unrotated <- lapply(to.keep, mat.mult, y=rot.mat.rev)
  transects <- lines.unrotated
  #Also rotate covered region
  if(return.cov.area){
    cover.polys.unrot <- lapply(cover.polys, mat.mult, y = rot.mat.rev)
  }
  return(list(transects = transects, cover.polys = cover.polys.unrot)
}
