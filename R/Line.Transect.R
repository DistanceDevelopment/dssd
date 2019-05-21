#' @include Transect.R
#' @importFrom methods validObject

#' @title Class "Line.Transect" extends Class "Transect"
#'
#' @description Virtual Class \code{"Line.Transect"} is an S4 class
#' detailing a set of transects from a point transect design.
#' @name Line.Transect-class
#' @title S4 Class "Line.Transect"
#' @slot line.length the total line length for the transect set
#' @keywords classes
#' @seealso \code{\link{make.design}}
#' @export
setClass(Class = "Line.Transect",
         representation = representation(line.length = "numeric"),
         contains = "Transect")

setMethod(
  f="initialize",
  signature="Line.Transect",
  definition=function(.Object, design, lines, no.samplers, line.length, effort.allocation,
                      spacing, design.angle, edge.protocol, cov.area = numeric(0), cov.area.polys = list()){
    #Set slots
    .Object@design        <- design
    .Object@samplers      <- lines
    .Object@cov.area      <- cov.area
    .Object@cov.area.polys <- cov.area.polys
    .Object@line.length   <- line.length
    .Object@no.samplers   <- no.samplers
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

setValidity("Line.Transect",
            function(object){
              return(TRUE)
            }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' Plot
#'
#' Plots an S4 object of class 'Survey'
#'
#' @rdname plot.Transect-methods
#' @exportMethod plot
setMethod(
  f="plot",
  signature="Line.Transect",
  definition=function(x, y, ...){
    # If main is not supplied then take it from the object
    additional.args <- list(...)
    add <- ifelse("add" %in% names(additional.args), additional.args$add, FALSE)
    col <- ifelse("col" %in% names(additional.args), additional.args$col, 5)
    lwd <- ifelse("lwd" %in% names(additional.args), additional.args$lwd, 2)
    if(length(x@samplers) > 0){
      plot(x@samplers$geometry, add = add, col = col, lwd = lwd)
    }else{
      warning("No samplers to plot", call. = F, immediate. = F)
    }
    invisible(x)
  }
)

