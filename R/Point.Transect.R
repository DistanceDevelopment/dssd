#' @include Transect.R
#' @importFrom methods validObject

#' @title Class "Point.Transect" extends Class "Survey"
#'
#' @description Virtual Class \code{"Point.Transect"} is an S4 class
#' detailing a set of transects from a point transect design.
#' @name Point.Transect-class
#' @title S4 Class "Point.Transect"
#' @keywords classes
#' @seealso \code{\link{make.design}}
#' @export
setClass(Class = "Point.Transect",
         representation = representation(),
         contains = "Transect")


setMethod(
  f="initialize",
  signature="Point.Transect",
  definition=function(.Object, design, points, samp.count, effort.allocation,
                      spacing, design.angle, edge.protocol, cov.area = numeric(0), cov.area.polys = list()){
    #Set slots
    .Object@design        <- design
    .Object@samplers      <- points
    .Object@cov.area      <- cov.area
    .Object@cov.area.polys <- cov.area.polys
    .Object@samp.count    <- samp.count
    .Object@effort.allocation <- effort.allocation
    .Object@spacing       <- spacing
    .Object@design.angle  <- design.angle
    .Object@edge.protocol <- edge.protocol
    #Check object is valid
    valid <- try(validObject(.Object), silent = TRUE)
    if(class(valid) == "try-error"){
      stop(attr(valid, "condition")$message, call. = FALSE)
    }
    # return object
    return(.Object)
  }
)

setValidity("Point.Transect",
            function(object){
              return(TRUE)
            }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' Plot
#'
#' Plots an S4 object of class 'Transect'
#'
#' @param x object of class Transect
#' @param y not used
#' @param ... other general plot parameters
#' @rdname plot.Transect-methods
#' @exportMethod plot
setMethod(
  f="plot",
  signature="Point.Transect",
  definition=function(x, y, ...){
    # If main is not supplied then take it from the object
    additional.args <- list(...)
    add <- ifelse("add" %in% names(additional.args), additional.args$add, FALSE)
    col <- ifelse("col" %in% names(additional.args), additional.args$col, 5)
    pch <- ifelse("pch" %in% names(additional.args), additional.args$pch, 20)
    if(length(x@samplers) > 0){
      plot(x@samplers, add = add, col = col, pch = pch)
    }else{
      warning("No samplers to plot", call. = F, immediate. = F)
    }
    invisible(x)
  }
)
