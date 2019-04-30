#' @include Survey.R

#' @title Class "Line.Transect.Survey" extends Class "Survey"
#'
#' @description Virtual Class \code{"Line.Transect.Survey"} is an S4 class
#' detailing a set of transects from a point transect design.
#' @name Point.Transect.Survey-class
#' @title S4 Class "Line.Transect.Survey"
#' @slot line.length the total line length for the transect set
#' @keywords classes
#' @seealso \code{\link{make.design}}
#' @export
setClass(Class = "Line.Transect.Survey",
         representation = representation(line.length = "numeric"),
         contains = "Survey")

setMethod(
  f="initialize",
  signature="Line.Transect.Survey",
  definition=function(.Object, design, lines, no.samplers, line.length, effort.allocation,
                      spacing, design.angle, edge.protocol){
    #Set slots
    .Object@design        <- design
    .Object@samplers      <- lines
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

setValidity("Line.Transect.Survey",
            function(object){
              return(TRUE)
            }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

setMethod(
  f="plot",
  signature="Line.Transect.Survey",
  definition=function(x, y, ...){
    # If main is not supplied then take it from the object
    additional.args <- list(...)
    add <- ifelse("add" %in% names(additional.args), additional.args$add, FALSE)
    col <- ifelse("col" %in% names(additional.args), additional.args$col, 5)
    pch <- ifelse("pch" %in% names(additional.args), additional.args$pch, 20)
    if(length(x@samplers) > 0){
      plot(x@samplers$geometry, add = add, col = col, pch = pch)
    }else{
      warning("No samplers to plot", call. = F, immediate. = F)
    }
    invisible(x)
  }
)

