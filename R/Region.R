#' @include Transect.R
#' @importFrom methods validObject
#' @import sf
NULL

#' @title  Class "Region"
#'
#' @description Class \code{"Region"} is an S4 class containing descriptions of the
#' study area. Uses an object of class
#'
#' @name Region-class
#' @title S4 Class "Region"
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{make.region(region.name = "region.name", shapefile = region.shapefile)}
#' @slot region.name Object of class \code{"character"}; giving the
#'  name of the region.
#' @slot strata.name Object of class \code{"character"}; character
#'  vector giving the names of the strata.
#' @slot units Object of class \code{"character"}; character describing
#'  the coordinate units ("km" or "m")
#' @slot area Object of class \code{"numeric"}; the area of the survey
#'  region
#' @slot region Object of class \code{"sf"} defining the survey region
#' @section Methods:
#' \describe{
#'  \item{\code{get.area}}{\code{signature(obj = "Region")}: retrieves the area
#'  element }
#'  \item{\code{plot}}{\code{signature(x = "Region", y = "missing")}: plots
#'  the survey region defined by the object.}
#' }
#' @keywords classes
#' @seealso \code{\link{make.region}}
#' @export
setClass(Class = "Region",
         representation(region.name = "character",
                        strata.name = "character",
                        units = "character",
                        area = "numeric",
                        region = "list")
)

setMethod(
  f="initialize",
  signature="Region",
  definition=function(.Object, region.name = character(0), strata.name = character(0), units = character(0), sf.shape = NULL){
    #calculates the strata areas
    area <- as.numeric(sf::st_area(sf.shape))
    if(length(units) == 0){
      tmp <- sf::st_crs(sf.shape)
      if(!is.na(tmp)){
        if(is.null(tmp$units)){
          units <- units
          warning("Coordinate reference system detected but no units can be found. Has this shape been projected - shapefiles must be projected on to a flat plane before surveys are designed. dssd is unstable and may generate errors when working with unprojected regions.", call. = FALSE, immediate. = TRUE)
        }else
          units <- tmp$units
      }else{
        units <- units
      }
    }
    #Set slots
    .Object@region.name <- region.name
    .Object@strata.name <- strata.name
    .Object@units       <- units
    .Object@area        <- area
    .Object@region      <- sf.shape
    #Check object is valid
    valid <- try(validObject(.Object), silent = TRUE)
    if(class(valid) == "try-error"){
      stop(attr(valid, "condition")$message, call. = FALSE)
    }
    # return object
    return(.Object)
  }
)

setValidity("Region",
            function(object){
              if(length(object@strata.name) > 0){
                strata.name <- object@strata.name
                # Check that none are Total
                if(any(strata.name == "Total")){
                  return("'Total' is not an accepted strata name, please ammend it.")
                }
                #check that they are all unique
                for(i in seq(along = strata.name)){
                  temp <- strata.name[-i]
                  for(j in seq(along = temp)){
                    if(strata.name[i] == temp[j]){
                      return("Stratum names must be unique.")
                    }
                  }
                }
              }
              if(length(which(object@area <= 0)) > 0){
                return("All areas must be greater than 0. ")
              }
              return(TRUE)
            }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' Returns the area of the region
#'
#' @param object object of class \code{Region}
#' @return numeric value specifying the area of the region
#' @rdname get.area-methods
setGeneric(name = "get.area", def = function(object){standardGeneric ("get.area")})

#' @rdname get.area-methods
setMethod(
  f="get.area",
  signature="Region",
  definition=function(object){
    return(object@area)
  }
)

#' Plot
#'
#' Plot's an object of class Region with optionally a set of transects or
#' the points of a coverage grid if supplied as the y argument.
#' @param x object of class Region
#' @param main the main title for the plot
#' @param region.col colours for the strata
#' @param strata the strata name or number to be plotted. By default
#' all strata will be plotted.
#' @param line.col sets the line colour for the shapefile
#' @param legend.params depricated since implementation of ggplot2
#' @rdname plot.Region-methods
#' @return returns a ggplot object
#' @importFrom ggplot2 ggplot geom_sf ggtitle aes theme_set theme_bw scale_fill_manual
#' @exportMethod plot
setMethod(
  f="plot",
  signature="Region",
  definition=function(x, y, main = "", region.col = "default", strata = "all", line.col = gray(.2), legend.params = list()){
    # Warn of depreications
    if(length(legend.params) > 0){
      warning("legend.params argument is deprecated since version 0.2.3", immediate. = TRUE, call. = FALSE)
    }
    # Tidy up space to keep ggplot happy
    suppressWarnings(invisible(gc()))
    # Check strata selection
    # Extract strata names
    strata.names <- x@strata.name
    # Extract plot data
    if(is.character(strata)){
      if(!strata %in% c(x@strata.name, "all")){
        stop("You have provided an unrecognised strata name.", call. = FALSE)
      }
    }
    # Extract region data
    sf.region <- x@region
    # For backwards compatibility (this info should be stored inside the sf object)
    if(!strata %in% names(sf.region)){
      sf.region <- cbind(strata = strata.names, sf.region)
    }
    # Extract strata data and set title
    if(strata != "all"){
      sf.region <- sf.region[sf.region$strata == strata,]
      title <- strata
    }else{
      title <- x@region.name
    }
    # Get plotting colours
    cols <- region.col
    if(any(cols == "default")){
      if(length(x@strata.name) <= 15){
        cols <-  c("lavender","lemonchiffon", "thistle1", "lightsteelblue1", "paleturquoise1", "palegreen", "wheat1", "salmon1", "ivory1", "olivedrab1", "slategray1", "seashell1", "plum1", "khaki1", "snow1")[1:(length(x@strata.name))]
      }else{
        cols <-  rep(c("lavender","lemonchiffon", "thistle1", "lightsteelblue1", "paleturquoise1", "palegreen", "wheat1", "salmon1", "ivory1", "olivedrab1", "slategray1", "seashell1", "plum1", "khaki1", "snow1"),ceiling(length(x@strata.name)/15))[1:(length(x@strata.name))]
      }
    }
    # Add names so can use in gglot
    if(length(cols) != length(x@strata.name)) cols <- rep(cols[1],length(x@strata.name))
    names(cols) <- x@strata.name
    # Check if user has supplied title
    if(main != "") title <- main
    # Create the plot object
    if(length(x@strata.name) == 1){
      ggplot.obj <- ggplot() + theme_bw() +
        geom_sf(data = sf.region, fill = cols, lwd = 0.2) +
        ggtitle(title)
    }else{
      ggplot.obj <- ggplot() + theme_bw() +
        geom_sf(data = sf.region, aes(fill = strata), lwd = 0.2) +
        scale_fill_manual(values = cols) +
        ggtitle(title)
    }
    # return the plot object incase the user wants to modify
    return(ggplot.obj)
  }
)

#' @param y an object inheriting from class Transect or an object of class
#' Coverage.Grid
#' @param main the main title for the plot
#' @param region.col colours for the strata
#' @param strata the strata name or number to be plotted. By default
#' all strata will be plotted.
#' @param line.col sets the line colour for the lines around the survey region.
#' @param col sets the colour of the transects / coverage grid points
#' @param lwd sets the line width of the transects
#' @param covered.area boolean value saying whether the covered area should be plotted.
#' @param legend.params depricated since implementation of ggplot2
#' @rdname plot.Region-methods
#' @importFrom graphics legend mtext
#' @exportMethod plot
setMethod(
  f="plot",
  signature=c("Region", "Transect"),
  definition=function(x, y, main = "", region.col = "default", strata = "all", line.col = gray(.2), col = "blue", lwd = 1, covered.area = FALSE, legend.params = list()){
    # Warn of depreications
    if(length(legend.params) > 0){
      warning("legend.params argument is deprecated since version 0.2.3", immediate. = TRUE, call. = FALSE)
    }
    # Tidy up space to keep ggplot happy
    suppressWarnings(invisible(gc()))
    # Call plot method for Region object and store ggplot object
    ggplot.obj <- plot(x, main = main, region.col = region.col, strata = strata,
                       line.col = line.col)
    # Get sampler data
    sf.samplers <- y@samplers
    if(strata != "all"){
      sf.samplers <- sf.samplers[sf.samplers$strata == strata,]
    }
    if(covered.area){
      sf.cov.area <- y@cov.area.polys
      if(strata != "all"){
        sf.cov.area <- sf.cov.area[sf.cov.area$strata == strata,]
      }
    }
    # Add the transects on to the plot
    ggplot.obj <- ggplot.obj +
      geom_sf(data = sf.samplers, col = col, lwd = lwd)
    # if requested add in covered areas
    if(covered.area){
      ggplot.obj <- ggplot.obj +
        geom_sf(data = sf.cov.area, fill = NA, lwd = 0.2)
    }
    # return the plot object incase the user wants to modify
    return(ggplot.obj)
  }
)


#' @param x object of class Region or inheriting from Survey
#' @param main the main title for the plot
#' @param region.col colours for the strata
#' @param strata the strata name or number to be plotted. By default
#' all strata will be plotted.
#' @param line.col sets the line colour for the lines around the survey region.
#' @param cex affects the size of the point on the coverage grid
#' @rdname plot.Region-methods
#' @importFrom graphics legend mtext
#' @importFrom grDevices gray
#' @exportMethod plot
setMethod(
  f="plot",
  signature=c("Region", "Coverage.Grid"),
  definition=function(x, y, main = "", region.col = "default", strata = "all", line.col = gray(.2), col = "black", cex = 1){
    # Tidy up space to keep ggplot happy
    suppressWarnings(invisible(gc()))
    # Call plot method for Region object and store ggplot object
    ggplot.obj <- plot(x, main = main, region.col = region.col, strata = strata,
                       line.col = line.col)
    # Get point data
    sf.cover <- y@grid
    # Add the transects on to the plot
    ggplot.obj <- ggplot.obj +
      geom_sf(data = sf.cover, col = col, cex = cex)
    # return the plot object incase the user wants to modify
    return(ggplot.obj)
  }
)







