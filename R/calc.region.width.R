calc.region.width <- function(design, strata.id = NULL){
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
    if(design@design[strat] %in% c("eszigzag", "eszigzagcom")){
      #apply angle correction for zigzag designs add 90 degrees
      rot.angle.rad <- rot.angle.rad + (90/180*pi)
    }
    theta <- ifelse(rot.angle.rad == 0, 0, 2*pi-rot.angle.rad)
    rot.mat <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), ncol = 2, byrow = FALSE)
    rot.strata <- strata*rot.mat
    # if we are using atlas and the shape is not valid
    if(grepl("atlas", sessionInfo()$BLAS) && is.na(sf::st_is_valid(rot.strata))){
      # turn it into and sfc shape
      tmp <- sf::st_sfc(rot.strata)
      # make valid with setting the precision
      tmp <- sf::st_make_valid(sf::st_set_precision(tmp,1e8))
      # extract shape again
      rot.strata <- tmp[[1]]
    }
    #Find the width of the region
    bbox <- sf::st_bbox(rot.strata)
    width <- width + (bbox$xmax - bbox$xmin)
  }
  names(width) <- NULL
  return(width)
}
