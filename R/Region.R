#' @import sf
#' @include Survey.R
#' @importFrom methods validObject
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
  definition=function(.Object, region.name = character(0), strata.name = character(0), units = "m", sf.shape = NULL){
    #calculates the strata areas
    area <- sf::st_area(sf.shape)
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
#' Plots an S4 object of class 'Region'
#'
#' @param x object of class Region or inheriting from Survey
#' @param y optionally a Survey object to plot with the Region
#' @param main the main title for the plot
#' @param cols colours for the strata
#' @param ... other general plot parameters
#' @rdname plot.Region-methods
#' @exportMethod plot
setMethod(
  f="plot",
  signature="Region",
  definition=function(x, y, main = "", cols = c(2,4,5,6,7,8,3), ...){
    # If main is not supplied then take it from the object
    if(main == ""){
      main <- x@region.name
    }
    region <- x@region
    bbox <- st_bbox(region)
    plot(c(0,0), col = "white", xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax), main = main, xlab = "x-coordinates", ylab = "y-coordinates")
    for(i in seq(along = region$geometry)){
      plot(region$geometry[[i]], add = TRUE, col = cols[i])
    }
    #plot(x@region, main = main, ...)
    invisible(x)
  }
)

#' Plot
#'
#' Plots an S4 object of class 'Region'
#' @param region.cols fill colours for strata
#' @rdname plot.Region-methods
#' @exportMethod plot
setMethod(
  f="plot",
  signature=c("Region", "Survey"),
  definition=function(x, y, main = "", region.col = c(2,4,5,6,7,8,3), ...){
    # If main is not supplied then take it from the object
    if(main == ""){
      main <- x@region.name
    }
    region <- x@region
    #Set up bounding box for samplers (necessary when plus sampling used and extent of samplers is greater than the region)
    bbox.samps <- st_bbox(y@samplers)
    bbox.region <- st_bbox(x@region)
    plot(c(0,0), col = "white", xlim = c(min(bbox.samps$xmin, bbox.region$xmin), max(bbox.samps$xmax, bbox.region$xmax)), ylim = c(min(bbox.samps$ymin, bbox.region$ymin), max(bbox.samps$ymax, bbox.region$ymax)), main = main, xlab = "x-coordinates", ylab = "y-coordinates")
    for(i in seq(along = region$geometry)){
      plot(region$geometry[[i]], add = TRUE, col = region.col[i])
    }
    plot(y, add = TRUE, ...)
    invisible(x)
  }
)

