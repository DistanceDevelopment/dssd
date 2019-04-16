#' @title Creates a Region object
#' @description This creates an instance of the Region class. If the
#' \code{shapefile} argument is supplied, all information will be extracted from
#' there. Otherwise, the a list of polygons describing the areas of interest needs
#' to be supplied (\code{coords}) and optionally a list of polygons describing the
#' areas to be excluded (\code{gaps}). If \code{area} is not specified it will be
#' calculated.
#' @param region.name the region name
#' @param strata.name the stratum names (character vector, same length as the
#'   number of areas in the \code{shapefile} or \code{coords} arguments). If not supplied "A", "B", "C", ... will be assigned.
#' @param units measurement units; either \code{"m"} for metres or \code{"km"} for
#'   kilometres.
#' @param shape sf object, sp object, shapefile path to .shp file or a
#' data.frame of coordinates.
#' @return object of class Region
#' @export
#' @author Laura Marshall
#' @examples
#' # A basic study region of 2000m by 500m is created using the defaults
#' region <- make.region()
#' plot(region)
#'
#' # Here is an example of a 1000 x 1000 study region with a gap
#' coords <- gaps <- list()
#' coords[[1]] <- list(data.frame(x = c(0,1000,1000,0,0), y = c(0,0,
#'  1000,1000,0)))
#' gaps[[1]] <- list(data.frame(x = c(400,600,500,350,400), y = c(100,
#'  250,600,120,100)))
#'
#' region <- make.region(region.name = "study.area", units = "m",
#'  coords = coords, gaps = gaps)
#' plot(region)
#'
make.region <- function(region.name = "region",
                        strata.name = character(0),
                        shape = NULL){
  #Process shape
  if("sf" %in% class(shape)){
    sf.shape = shape
  }else if(any(class(shape) %in% c("Polygon", "Polygons", "SpatialPolygons", "SpatialPolygonsDataFrame"))){
    stop("The sp data type is not currently supported.")
  }else if(class(shape) == "character"){
    sf.shape <- sf::read_sf(shape)
  }else if(class(shape) == "data.frame"){
    stop("The data.frame data type is not currently supported.")
  }else{
    stop("This data type is not currently supported.")
  }
  # Check the format of the shape
  sf.shape <- check.shape(sf.shape)
  #Check the correct number of strata names have been supplied
  if(length(sf.shape$geometry) != length(strata.name) && length(strata.name) > 0){
    warning("Number of strata names does not match the number of strata assiging default strata names", call. = FALSE, immediate. = TRUE)
    strata.name = character(0)
  }
  # If there is more than one strata and the user has not specified strata.name
  if(length(sf.shape$geometry) > 1 & length(strata.name) == 0){
    no.strata <- length(sf.shape$geometry)
    if(no.strata <= 26){
      strata.name <- LETTERS[1:no.strata]
    }else{
      stop("Too many strata (>26) for strata names to be assigned default names.", call. = FALSE)
    }
  }
  # Call to make the region object
  region <- new(Class="Region", region.name = region.name, strata.name = strata.name, sf.shape = sf.shape)
  return(region)
}
