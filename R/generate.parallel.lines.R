#' @importFrom stats runif
#' @importFrom methods new
generate.parallel.lines <- function(design, strata.id, samplers, line.length, spacing, by.spacing, quiet = FALSE, calc.cov.area = TRUE, clip.to.strata = TRUE){
  #Generates either random or systematic parallel lines
  region <- design@region
  sf.column <- attr(region@region, "sf_column")
  #Get the current strata and spacing
  strata <- region@region[[sf.column]][[strata.id]]
  #Spin round so design angle lies along x axis
  rot.angle.rad <- design@design.angle[strata.id]/180*pi
  theta <- ifelse(rot.angle.rad == 0, 0, 2*pi-rot.angle.rad)
  rot.mat <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), ncol = 2, byrow = FALSE)
  rot.strata <- mat.mult(strata, rot.mat)
  #Buffer strata for plus sampling?
  if(design@edge.protocol[strata.id] == "plus"){
    rot.strata <- st_buffer(rot.strata, design@truncation)
  }
  #Find the minimum and maximum x and y values
  bbox <- sf::st_bbox(rot.strata)
  #Find the number of samplers
  if(!by.spacing && is.na(samplers)){
    #Calculate from line.length
    width <- bbox$xmax - bbox$xmin
    names(width) <- NULL
    ave.line.height <- sf::st_area(rot.strata)/width
    samplers <- line.length/ave.line.height
    if(samplers < 1){
      if(!quiet){
        warning(paste("Line length is less than the average transect length cannot generate samplers in strata ", strata.id, sep = ""), immediate. = T, call. = F)
      }
      return(NULL)
    }
  }else if(!by.spacing && !is.na(samplers)){
    if(samplers < 1){
      if(!quiet){
        warning(paste("Number of samplers < 1, cannot allocate samplers in strata ", strata.id, sep = ""), immediate. = T, call. = F)
      }
      return(NULL)
    }
  }
  if(!by.spacing && design@design[strata.id] == "systematic"){
    spacing <- (bbox[["xmax"]]-bbox[["xmin"]])/(samplers)
  }
  start.y <- bbox[["ymin"]]
  end.y <- bbox[["ymax"]]
  if(design@design[strata.id] == "systematic"){
    if(spacing > (bbox[["xmax"]]-bbox[["xmin"]])){
      if(!quiet){
        warning(paste("Spacing larger than x-range cannot generate samplers in strata ", strata.id, sep = ""), immediate. = T, call. = F)
      }
      return(NULL)
    }
    start.x <- bbox[["xmin"]] + runif(1, 0, spacing)
    x.vals <- seq(start.x, bbox[["xmax"]], by = spacing)
  }else if(design@design[strata.id] == "random"){
    x.vals <- runif(samplers, bbox[["xmin"]], bbox[["xmax"]])
    #sorting important for calculating trackline
    x.vals <- sort(x.vals)
  }
  #Create transects lines
  lines <- list()
  for(i in seq(along = x.vals)){
    lines[[i]] <- sf::st_linestring(matrix(c(rep(x.vals[i],2),start.y, end.y), ncol = 2))
  }
  #keep everything within the polygon strata
  to.keep <- lapply(lines, sf::st_intersection, y = rot.strata)
  #Only keep lines - sometimes points are generated for extremely small intersections
  test.line <- function(x){ifelse(inherits(x, "LINESTRING") || inherits(x, "MULTILINESTRING"), TRUE, FALSE)}
  is.line <- which(unlist(lapply(to.keep, FUN = test.line)))
  to.keep <- to.keep[is.line]
  #Calculate covered region - do it here as easier before unrotating!
  cover.polys <- list()
  if(calc.cov.area){
    trunc <- design@truncation
    for(tr in seq(along = to.keep)){
      if(inherits(to.keep[[tr]], "LINESTRING")){
        bbox <- sf::st_bbox(to.keep[[tr]])
        x.vals <- c(rep((bbox$xmin - trunc),2), rep((bbox$xmax + trunc),2), (bbox$xmin - trunc))
        y.vals <- c(bbox$ymin, rep(bbox$ymax,2), rep(bbox$ymin,2))
        cover.polys[[tr]] <- sf::st_polygon(list(matrix(c(x.vals, y.vals), ncol = 2)))
      }else if(inherits(to.keep[[tr]], "MULTILINESTRING")){
        #Need to iterate along the list
        temp <- list()
        for(part in seq(along = to.keep[[tr]])){
          line.mat <- to.keep[[tr]][[part]]
          lx <- line.mat[,1]
          ly <- line.mat[,2]
          px <- c(rep((lx[1] - trunc), 2), rep((lx[2] + trunc), 2), (lx[1] - trunc))
          py <- c(ly[1], rep(ly[2],2), rep(ly[1],2))
          temp[[part]] <- list(matrix(c(px, py), ncol = 2))
        }
        cover.polys[[tr]] <- sf::st_multipolygon(temp)
      }
    }
  }
  #Check if any polygons are invalid - sometimes tiny pieces of line are generated on the boundaries which lead to overlapping multi polygons
  invalid <- which(!unlist(lapply(cover.polys, sf::st_is_valid)))
  for(i in seq(along = invalid)){
    tmp <- cover.polys[[invalid[i]]]
    polys.tmp <- list()
    mat.tmp <- list()
    for(poly in seq(along = tmp)){
      polys.tmp[[poly]] <- sf::st_polygon(tmp[[poly]])
      mat.tmp[[poly]] <- tmp[[poly]]
    }
    to.rem <- numeric(0)
    for(poly in seq(along = polys.tmp)){
      intsec <- which(unlist(lapply(polys.tmp, sf::st_intersects, polys.tmp[poly][[1]], sparse = FALSE)))[-poly]
      if(length(intsec) > 0){
        intsec <- sort(c(intsec, poly))
        areas <- unlist(lapply(polys.tmp[intsec], sf::st_area))
        to.rem <- c(to.rem, intsec[which(areas == min(areas))])
        #if(min(areas) > sf::st_area(rot.strata)/50000){
        #  warning("Removing covered area greater than 50,000th of the strata area.", immediate. = TRUE, call. = FALSE)
        #}
      }
    }
    to.rem <- unique(to.rem)
    new.polys <- mat.tmp[-to.rem]
    if(length(new.polys) == 1){
      cover.polys[[invalid[i]]] <- sf::st_polygon(new.polys[[1]])
    }else{
      cover.polys[[invalid[i]]] <- sf::st_multipolygon(new.polys)
    }
    #Also remove strange corresponding transect part
    tmp <- to.keep[[invalid[i]]]
    if(length(tmp[-to.rem]) == 1){
      to.keep[[invalid[i]]] <- sf::st_linestring(tmp[-to.rem][[1]])
    }else{
      to.keep[[invalid[i]]] <- sf::st_multilinestring(tmp[-to.rem])
    }
  }
  if(clip.to.strata){
    cover.polys <- lapply(cover.polys, sf::st_intersection, y = rot.strata)
  }
  #Rotate back again
  reverse.theta <- rot.angle.rad
  rot.mat.rev <- matrix(c(cos(reverse.theta), sin(reverse.theta), -sin(reverse.theta), cos(reverse.theta)), ncol = 2, byrow = FALSE)
  lines.unrotated <- lapply(to.keep, mat.mult, y=rot.mat.rev)
  transects <- lines.unrotated
  #Also rotate covered region
  if(calc.cov.area){
    cover.polys.unrot <- lapply(cover.polys, mat.mult, y = rot.mat.rev)
    return(list(transects = transects, cover.polys = cover.polys.unrot, spacing = spacing))
  }
  return(list(transects = transects, cover.polys = list(), spacing = spacing))
}
