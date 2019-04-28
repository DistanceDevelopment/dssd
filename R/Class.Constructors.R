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
  }else if(is.null(shape)){
    #Make a default shape (same as in DSsim currently)
    sfc.shape <- sf::st_sfc(sf::st_polygon(list(matrix(c(0,0,0,500,2000,500,2000,0,0,0), ncol = 2, byrow = TRUE))))
    sf.shape <- sf::st_sf(data.frame(region="study_ar", geom=sfc.shape))
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

#' @title  Creates a Survey.Design object
#' @description Creates a description of a survey design. Designs may use different
#' types of either point or line transect designs across strata but cannot mix
#' point and line transect design types within a single design object.
#'
#' @details For point transect designs the user may either specify "random" or
#' "systematic" for the design argument. If the user specifies "random", they
#' should also provide a value for effort detailing the number of point transects
#' they wish their survey to have (for stratified designs they may specify a vector
#' of numbers detailing the number of transects per strata or alternatively use the
#' effort.allocation argument to allocate effort proportionally). If the user specified
#' "systematic" they may either provide their desired number of samplers or a value
#' for spacing which defines the gap between each of the points (again a vector
#' of spacing values can be provided for each strata). Optionally the user may
#' select a design.angle. For both random and systematic point transect designs
#' the user may select either a minus or plus sampling edge protocol.
#'
#' For line transect designs the user may either specify "random" (randomly
#' placed full width lines), "systematic" (systematically placed full width lines)
#' or "ESzigzag" (equally placed zigzag lines). If the user specifies "random", they
#' should provide the either the number of samplers they wish the design to generate
#' or the total line length they wish to achieve. If the user specifies "systematic"
#' they should specify either the number of samplers, the desired total line length
#' or the spacing between lines. The design angle for these parallel line designs
#' refers to the angle of the lines. If the user specifies the zigzag design they
#' should specify the systematic spacing value to be used and should choose between
#' generating the design in a minimum bounding rectangle or a convex hull. The
#' designs may be generated using plus or minus sampling protocols. Similar to
#' the point transect designs different values may be specified for each strata
#' for all of the above options.
#'
#' @param transect.type character variable specifying either "line" or "point"
#' @param design a character variable describing the type of design. Either "random",
#' "systematic" or "ESzigzag" (equal-spaced zigzag). See details for more information.
#' @param no.samplers the number of samplers you wish the design to generate.
#' @param line.length the total line length you desire.
#' @param effort.allocation numeric values used to indicate the proportion of effort
#' to be allocated to each strata from number of samplers or line length. If length 0,
#' effort allocated based on stratum area.
#' @param design.angle numeric value detailing the angle of the design. Can provide
#' multiple values relating to strata. The use of the angle varies with design, it
#' can be either the angle of the grid of points, the angle of lines or the design
#' axis for the zigzag design.
#' @param spacing used by systematic designs, numeric value to define spacing
#' between transects.
#' @param edge.protocol character value indicating whether a "plus" sampling or
#' "minum" sampling protocol is used.
#' @param bounding.shape only applicable to zigzag designs. A character value saying
#' whether the zigzag transects should be generated using a minimum bounding
#' "rectangle" or a "convex hull".
#' @param truncation A numeric value describing the longest distance at which an
#' object may be observed.
#' @return object of a class which inherits from class Survey.Design
#' @export
#' @author Laura Marshall
#' @examples
#' design <- make.design(transect.type = "point", no.samplers = 25, design.angle = 45)
make.design <- function(region = make.region(), transect.type = "line", design = "systematic", no.samplers = numeric(0), line.length = numeric(0), effort.allocation = numeric(0), design.angle =  0, spacing = numeric(0), edge.protocol = "minus", bounding.shape = "rectangle", truncation = 1){
  #Check design arguments
  if(transect.type %in% c("Line", "line", "Line Transect", "line transect")){
    if(design == "random"){
      if(length(no.samplers) > 0 && length(line.length) > 0){
        warning("You have supplied both the number of samplers and the line length, only the number of samplers will be used to create the surveys.", call. = FALSE)
        line.length <- numeric(0)
      }else if(length(no.samplers) == 0 && length(line.length) == 0){
        no.samplers = 20
      }
    }else if(design == "systematic"){
      if((length(no.samplers) > 0 || length(line.length) > 0) && length(spacing) > 0){
        warning("You have supplied multiple effort definitions, the sampler spacing will be used.", call. = FALSE)
        line.length <- numeric(0)
        no.samplers <- numeric(0)
      }else if(length(no.samplers) == 0 && length(line.length) == 0 && length(spacing) == 0){
        no.samplers = 20
      }
    }else if(design %in% c("ESzigzag", "eszigzag")){
      if((length(no.samplers) > 0 || length(line.length) > 0) && length(spacing) > 0){
        warning("You have supplied multiple effort definitions, the sampler spacing will be used.", call. = FALSE)
        line.length <- numeric(0)
        no.samplers <- numeric(0)
      }else if(length(no.samplers) == 0 && length(line.length) == 0 && length(spacing) == 0){
        no.samplers = 20
      }
      if(!bounding.shape %in% c("rectangle", "convex hull")){
        warning("Bounding shape option not recognised using a bounding rectangle", call. = FALSE)
      }
    }else{
      stop("Line transect design not recognised, please choose from 'random', 'systematic, or 'ESzigzag'", call. = FALSE)
    }
    #Check values
    if(any(any(no.samplers < 0) || any(line.length < 0) || any(spacing < 0))){
      stop("Negative values were used to specify effort.", call. = FALSE)
    }
    #Create line transect object
    design <- new(Class="Line.Transect.Design", region, truncation, design, line.length, effort.allocation, spacing, no.samplers, design.angle, edge.protocol, bounding.shape)
  }else if(transect.type %in% c("Point", "point", "Point Transect", "point transect")){
    if(design == "random"){
      if(length(no.samplers) == 0){
        no.samplers = 20
        spacing = numeric(0)
      }
    }else if(design == "systematic"){
      if(length(no.samplers) > 0  && length(spacing) > 0){
        warning("You have supplied multiple effort definitions, the sampler spacing will be used.", call. = FALSE)
        no.samplers <- numeric(0)
      }else if(length(no.samplers) == 0 && length(spacing) == 0){
        no.samplers = 20
      }
    }else{
      stop("Point transect design not recognised, please choose from 'random' or 'systematic", call. = FALSE)
    }
    #Check values
    if(any(any(no.samplers < 0) || any(spacing < 0))){
      stop("Negative values were used to specify effort.", call. = FALSE)
    }
    #Create line transect object
    design <- new(Class="Point.Transect.Design", region, truncation, design, spacing, no.samplers, effort.allocation, design.angle, edge.protocol)
  }
  return(design)
}

