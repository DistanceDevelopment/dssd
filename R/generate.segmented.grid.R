#' @importFrom stats runif
#' @importFrom methods new
generate.segmented.grid <- function(design, strata.id, samplers, line.length, spacing, by.spacing, seg.length, seg.threshold, quiet = FALSE, calc.cov.area = TRUE, clip.to.strata = TRUE){
  #Generates a grid of line segments
  #NOTE if you change how segments are generated this may invalidate the calculate.trackline.segl function!!!
  region <- design@region
  sf.column <- attr(region@region, "sf_column")
  #Get the current strata and spacing
  strata <- region@region[[sf.column]][[strata.id]]
  #Spin round so design angle lies along x axis
  rot.angle <- design@design.angle[strata.id]
  rot.angle.rad <- rot.angle/180*pi
  theta <- ifelse(rot.angle.rad == 0, 0, 2*pi-rot.angle.rad)
  rot.mat <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), ncol = 2, byrow = FALSE)
  rot.strata <- strata*rot.mat
  #Buffer strata for plus sampling?
  if(design@edge.protocol[strata.id] == "plus"){
    rot.strata <- st_buffer(rot.strata, design@truncation)
  }
  #Find the minimum and maximum x and y values
  bbox <- sf::st_bbox(rot.strata)
  #Find the number of samplers
  if(!by.spacing && is.na(samplers)){
    #Calculate from line.length
    samplers <- line.length/seg.length
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
  width <- bbox$xmax - bbox$xmin
  names(width) <- NULL
  ave.height <- sf::st_area(rot.strata)/width
  #Get spacing
  if(by.spacing){
    spacing.x <- spacing
  }else{
    spacing.x <- ((width*ave.height/samplers)+(seg.length/2)^2)^0.5 - seg.length/2
  }
  spacing.y <- spacing.x + seg.length
  #Check spacings are reasonable
  if(spacing.x > (bbox[["xmax"]]-bbox[["xmin"]])){
    if(!quiet){
      warning(paste("Spacing larger than x-range cannot generate samplers in strata ", strata.id, sep = ""), immediate. = T, call. = F)
    }
    return(NULL)
  }
  if(spacing.x > (bbox[["ymax"]]-bbox[["ymin"]])){
    if(!quiet){
      warning(paste("Spacing larger than y-range not generating samplers in strata ", strata.id, sep = ""), immediate. = T, call. = F)
    }
    return(NULL)
  }
  #Create grid of points for centre points of line segments.
  x.start <- bbox[["xmin"]] + runif(1, 0, spacing.x)
  x.vals <- seq(x.start, bbox[["xmax"]], by = spacing.x)
  #Some parts of segment may be inside even if centre points are not!
  y.start <- (bbox[["ymin"]]-seg.length/2) + runif(1, 0, (spacing.y))
  y.vals <- seq(y.start, (bbox[["ymax"]]+seg.length/2), by = (spacing.y))
  #Create transects lines
  lines <- list()
  count <- 1
  listnames <- list()
  for(i in seq(along = x.vals)){
    for(j in seq(along = y.vals)){
      lines[[count]] <- sf::st_linestring(matrix(c(rep(x.vals[i],2), y.vals[j]-seg.length/2, y.vals[j]+seg.length/2), ncol = 2))
      listnames[[count]] <- paste0(c(i, j), collapse = ".")
      count <- count + 1
    }
  }
  #Name the list so the trackline length can be calculated later
  ulnames <- unlist(listnames)
  names(lines) <- ulnames
  #keep everything within the polygon strata
  to.keep <- lapply(lines, sf::st_intersection, y = rot.strata)
  #Only keep lines - sometimes points are generated for extremely small intersections
  test.line <- function(x){ifelse(any(class(x) %in% c("LINESTRING", "MULTILINESTRING")), TRUE, FALSE)}
  is.line <- which(unlist(lapply(to.keep, FUN = test.line)))
  to.keep <- to.keep[is.line]
  #Check which lengths are over the threshold
  min.seg.length <- seg.threshold/100*seg.length
  is.over.threshold <- which(unlist(lapply(to.keep, sf::st_length)) >= min.seg.length)
  to.keep <- to.keep[is.over.threshold]
  #Check there are some transects
  if(length(to.keep) == 0){
    warning(paste("No transects generated in stratum ", strata.id, sep = ""), immediate. = TRUE, call. = FALSE)
    return(NULL)
  }
  #Calculate covered region - do it here as easier before unrotating!
  cover.polys <- list()
  if(calc.cov.area){
    trunc <- design@truncation
    for(tr in seq(along = to.keep)){
      if(any(class(to.keep[[tr]]) == "LINESTRING")){
        bbox <- sf::st_bbox(to.keep[[tr]])
        x.vals <- c(rep((bbox$xmin - trunc),2), rep((bbox$xmax + trunc),2), (bbox$xmin - trunc))
        y.vals <- c(bbox$ymin, rep(bbox$ymax,2), rep(bbox$ymin,2))
        cover.polys[[tr]] <- sf::st_polygon(list(matrix(c(x.vals, y.vals), ncol = 2)))
      }else if(any(class(to.keep[[tr]]) == "MULTILINESTRING")){
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
  mat.mult <- function(x,y){return(x*y)}
  lines.unrotated <- lapply(to.keep, mat.mult, y=rot.mat.rev)
  transects <- lines.unrotated
  #Also rotate covered region
  if(calc.cov.area){
    cover.polys.unrot <- lapply(cover.polys, mat.mult, y = rot.mat.rev)
    return(list(transects = transects, cover.polys = cover.polys.unrot, spacing = spacing.x))
  }
  return(list(transects = transects, cover.polys = list(), spacing = spacing.x))
}
