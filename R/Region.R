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
#' @param x object of class Region or inheriting from Survey
#' @param y optionally a Survey object to plot with the Region
#' @param main the main title for the plot
#' @param region.col colours for the strata
#' @param strata the strata name or number to be plotted. By default
#' all strata will be plotted.
#' @param scale used to scale the x and y values in the plot (warning may give
#' unstable results when a projection is defined for the study area!)
#' @param line.col sets the line colour for the shapefile
#' @param legend.params depricated since implementation of ggplot2
#' @rdname plot.Region-methods
#' @importFrom ggplot2 ggplot geom_sf ggtitle aes theme_set theme_bw scale_fill_manual
#' @exportMethod plot
setMethod(
  f="plot",
  signature="Region",
  definition=function(x, y, main = "", region.col = "default", strata = "all", scale = 1, line.col = gray(.2), legend.params = list()){
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
    sf.column <- attr(sf.region, "sf_column")
    # Scaling plot
    sf.region[, sf.column] <- sf.region[, sf.column]*scale
    # Extract strata data and set title
    if(strata != "all"){
      sf.region <- sf.region[sf.region$strata == strata,]
      title <- strata
    }else{
      title <- x@region.name
    }
    # Get plotting colours
    additional.args <- list(...)
    cols <- ifelse("cols" %in% names(additional.args), additional.args$cols, region.col)
    if(any(cols == "default")){
      if(length(x@strata.name) <= 15){
        cols <-  c("lavender","lemonchiffon", "thistle1", "lightsteelblue1", "paleturquoise1", "palegreen", "wheat1", "salmon1", "ivory1", "olivedrab1", "slategray1", "seashell1", "plum1", "khaki1", "snow1")[1:(length(x@strata.name))]
      }else{
        cols <-  rep(c("lavender","lemonchiffon", "thistle1", "lightsteelblue1", "paleturquoise1", "palegreen", "wheat1", "salmon1", "ivory1", "olivedrab1", "slategray1", "seashell1", "plum1", "khaki1", "snow1"),ceiling(length(x@strata.name)/15))[1:(length(x@strata.name))]
      }
    }
    # Add names so can use in gglot
    names(cols) <- x@strata.name
    # Check if user has supplied title
    if(main != "") title <- main
    # Create the plot object
    ggplot.obj <- ggplot() + theme_bw() +
      geom_sf(data = sf.region, aes(fill = strata), lwd = 0.2) +
      scale_fill_manual(values = cols) +
      ggtitle(title)
    # return the plot object incase the user wants to modify
    return(ggplot.obj)
  }
)

#' Plot
#'
#' Plots an S4 object of class 'Region'
#' @param subtitle a subtitle for the plot
#' @param covered.area boolean value saying whether the covered area should be plotted.
#' @param ... Additional plot arguments passed to the plot method for the y argument.
#' @rdname plot.Region-methods
#' @exportMethod plot
#' @importFrom graphics mtext
setMethod(
  f="plot",
  signature=c("Region", "Transect"),
  definition=function(x, y, main = "", region.col = "default", subtitle = "", covered.area = FALSE, ...){
    # If main is not supplied then take it from the object
    if(main == ""){
      main <- x@region.name
    }
    additional.args <- list(...)
    if(region.col == "default"){
      if(length(x@strata.name) <= 15){
        region.col <-  c("lavender","lemonchiffon", "thistle1", "lightsteelblue1", "paleturquoise1", "palegreen", "wheat1", "salmon1", "ivory1", "olivedrab1", "slategray1", "seashell1", "plum1", "khaki1", "snow1")[1:(length(x@strata.name))]
      }else{
        region.col <-  rep(c("lavender","lemonchiffon", "thistle1", "lightsteelblue1", "paleturquoise1", "palegreen", "wheat1", "salmon1", "ivory1", "olivedrab1", "slategray1", "seashell1", "plum1", "khaki1", "snow1"),3)[1:(length(x@strata.name))]
      }
    }
    cov.area <- covered.area
    region <- x@region
    sf.column <- attr(region, "sf_column")
    #Set up bounding box for samplers (necessary when plus sampling used and extent of samplers is greater than the region)
    bbox.samps <- sf::st_bbox(y@samplers)
    bbox.region <- sf::st_bbox(x@region)
    if(subtitle != ""){
      pmar <- par(mar=c(4, 4, 5, 1), xpd=TRUE)
      on.exit(par(mar = pmar))
    }
    plot(c(0,0), col = "white", xlim = c(min(bbox.samps$xmin, bbox.region$xmin), max(bbox.samps$xmax, bbox.region$xmax)), ylim = c(min(bbox.samps$ymin, bbox.region$ymin), max(bbox.samps$ymax, bbox.region$ymax)), main = main, xlab = "x-coordinates", ylab = "y-coordinates")
    for(i in seq(along = region[[sf.column]])){
      plot(region[[sf.column]][[i]], add = TRUE, col = region.col[i])
    }
    if(cov.area){
      #plot the covered areas
      cov.area.shape <- y@cov.area.polys
      sf.col.ca <- attr(cov.area.shape, "sf_column")
      cov.area.shape <- cov.area.shape[[sf.col.ca]]
      for(i in seq(along = cov.area.shape)){
        plot(cov.area.shape[[i]], add = T)
      }
    }
    plot(y, add = TRUE, ...)
    mtext(subtitle, side = 3, line = 0.5, outer = FALSE)
    invisible(x)
  }
)

#' Plot
#' @rdname plot.Region-methods
#' @exportMethod plot
#' @importFrom graphics mtext
setMethod(
  f="plot",
  signature=c("Region", "Coverage.Grid"),
  definition=function(x, y, main = "", region.col = "default", subtitle = "", ...){
    # If main is not supplied then take it from the object
    if(main == ""){
      main <- x@region.name
    }
    if(region.col == "default"){
      if(length(x@strata.name) <= 15){
        region.col <-  c("lavender","lemonchiffon", "thistle1", "lightsteelblue1", "paleturquoise1", "palegreen", "wheat1", "salmon1", "ivory1", "olivedrab1", "slategray1", "seashell1", "plum1", "khaki1", "snow1")[1:(length(x@strata.name))]
      }else{
        region.col <-  rep(c("lavender","lemonchiffon", "thistle1", "lightsteelblue1", "paleturquoise1", "palegreen", "wheat1", "salmon1", "ivory1", "olivedrab1", "slategray1", "seashell1", "plum1", "khaki1", "snow1"),3)[1:(length(x@strata.name))]
      }
    }
    region <- x@region
    sf.column <- attr(region, "sf_column")
    #Set up bounding box for samplers (necessary when plus sampling used and extent of samplers is greater than the region)
    #bbox.samps <- sf::st_bbox(y@samplers)
    bbox.region <- sf::st_bbox(x@region)
    if(subtitle != ""){
      pmar <- par(mar=c(4, 4, 5, 1), xpd=TRUE)
      on.exit(par(mar = pmar))
    }
    plot(c(0,0), col = "white", xlim = c(min(bbox.region$xmin), max(bbox.region$xmax)), ylim = c(min( bbox.region$ymin), max(bbox.region$ymax)), main = main, xlab = "x-coordinates", ylab = "y-coordinates")
    for(i in seq(along = region[[sf.column]])){
      plot(region[[sf.column]][[i]], add = TRUE, col = region.col[i])
    }
    plot(y, add = TRUE, ...)
    mtext(subtitle, side = 3, line = 0.5, outer = FALSE)
    invisible(x)
  }
)

