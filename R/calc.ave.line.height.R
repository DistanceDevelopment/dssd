calc.ave.line.height <- function(design, strata.id = NULL){
  region <- design@region
  sf.column <- attr(region@region, "sf_column")
  width <- 0
  if(is.null(strata.id)){
    index <- seq(along = region@region[[sf.column]])
  }else{
    index <- strata.id
  }
  #For each strat
  for(strat in seq(along = index)){
    strata <- region@region[[sf.column]][[index[strat]]]
    #Rotate by design angle
    rot.angle.rad <- design@design.angle[index[strat]]/180*pi
    theta <- ifelse(rot.angle.rad == 0, 0, 2*pi-rot.angle.rad)
    rot.mat <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), ncol = 2, byrow = FALSE)
    rot.strata <- strata*rot.mat
    #Find the width of the region
    bbox <- sf::st_bbox(rot.strata)
    width <- width + (bbox$xmax - bbox$xmin)
  }
  return(sum(region@area[index])/width)
}
