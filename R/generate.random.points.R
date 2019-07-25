generate.random.points <- function(design, strata.id, samplers, calc.cov.area = TRUE, clip.to.strata = TRUE, quiet = FALSE){
  #Check positive number of samplers
  if(samplers <= 0){
    if(!quiet){
      warning(paste("No samplers allocated to strata ", strata.id, ". Cannot generate samplers.", sep = ""), call. = FALSE, immediate. = TRUE)
    }
    return(NULL)
  }
  #Generates random points
  region <- design@region
  #Get the current strata and spacing
  sf.column <- attr(region@region, "sf_column")
  strata <- region@region[[sf.column]][[strata.id]]
  #Spin round so design angle lies along x axis
  rot.angle.rad <- design@design.angle[strata.id]/180*pi
  theta <- ifelse(rot.angle.rad == 0, 0, 2*pi-rot.angle.rad)
  rot.mat <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), ncol = 2, byrow = FALSE)
  rot.strata <- strata*rot.mat
  #Buffer strata for plus sampling?
  if(design@edge.protocol[strata.id] == "plus"){
    rot.strata <- sf::st_buffer(rot.strata, design@truncation)
  }
  random_pt <- sf::st_sample(rot.strata , size = samplers, type = "random")
  while (length(random_pt) < samplers) {
    diff <- samplers - length(random_pt)
    random_pt_new <- sf::st_sample(rot.strata , size = diff, type = "random")
    random_pt <- c(random_pt, random_pt_new)
  }
  #Rotate back again
  reverse.theta <- rot.angle.rad
  rot.mat.rev <- matrix(c(cos(reverse.theta), sin(reverse.theta), -sin(reverse.theta), cos(reverse.theta)), ncol = 2, byrow = FALSE)
  mat.mult <- function(x,y){return(x*y)}
  points.unrotated <- lapply(random_pt, mat.mult, y=rot.mat.rev)
  transects <- points.unrotated
  if(calc.cov.area){
    cov.area.polys <- lapply(transects, FUN = sf::st_buffer, dist = design@truncation)
    if(clip.to.strata){
      cov.area.polys <- lapply(cov.area.polys, sf::st_intersection, y = strata)
    }
    return(list(transects = transects, cover.polys = cov.area.polys))
  }
  return(list(transects = transects, cover.polys = list()))
}
