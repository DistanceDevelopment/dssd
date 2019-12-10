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
  #Buffer strata for plus sampling?
  if(design@edge.protocol[strata.id] == "plus"){
    strata <- sf::st_buffer(strata, design@truncation)
  }
  #Generate random points
  random_pt <- sf::st_sample(strata , size = samplers, type = "random")
  while (length(random_pt) < samplers) {
    diff <- samplers - length(random_pt)
    random_pt_new <- sf::st_sample(strata , size = diff, type = "random")
    random_pt <- c(random_pt, random_pt_new)
  }
  #Rotate back again
  transects <- random_pt
  if(calc.cov.area){
    cov.area.polys <- lapply(transects, FUN = sf::st_buffer, dist = design@truncation)
    if(clip.to.strata){
      cov.area.polys <- lapply(cov.area.polys, sf::st_intersection, y = strata)
    }
    return(list(transects = transects, cover.polys = cov.area.polys))
  }
  return(list(transects = transects, cover.polys = list()))
}
