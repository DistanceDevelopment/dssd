#' @include generic.functions.R
#' @include Survey.Design.R

#' @title Virtual Class "Line.Transect.Design" extends Class "Survey.Design"
#'
#' @description Virtual Class \code{"Line.Transect.Design"} is an S4 class detailing
#' the type of line transect design.
#' @name Line.Transect.Design-class
#' @title S4 Class "Line.Transect.Design"
#' @slot design Character value describing the name of the design.
#' @slot line.length Numeric value defining the total line length to be generated (may be
#' multiple values relating to each stratum).
#' @slot spacing Numeric value defining the systematic spacing (may be multiple values
#' relating to each stratum).
#' @section Methods:
#' \describe{
#'  \item{\code{generate.transects}}{\code{signature=(object = "Line.Transect.Design", ...)}:
#'  generates a set of transects from a shapefile.}
#' }
#' @keywords classes
#' @seealso \code{\link{make.design}}
#' @export
setClass(Class = "Line.Transect.Design",
         representation = representation(line.length = "numeric",
                                         bounding.shape = "character"),
         contains = "Survey.Design"
)


setMethod(
  f="initialize",
  signature="Line.Transect.Design",
  definition=function(.Object, truncation, design, line.length, effort.allocation,
                      spacing, no.samplers, design.angle, edge.protocol,
                      bounding.shape){
    #Set slots
    .Object@truncation    <- truncation
    .Object@design        <- design
    .Object@line.length   <- line.length
    .Object@effort.allocation <- effort.allocation
    .Object@spacing       <- spacing
    .Object@no.samplers   <- no.samplers
    .Object@design.angle  <- design.angle
    .Object@edge.protocol <- edge.protocol
    .Object@bounding.shape <- bounding.shape
    #Check object is valid
    valid <- try(validObject(.Object), silent = TRUE)
    if(class(valid) == "try-error"){
      stop(attr(valid, "condition")$message, call. = FALSE)
    }
    # return object
    return(.Object)
  }
)

setValidity("Line.Transect.Design",
            function(object){
              return(TRUE)
            }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' @rdname generate.transects-methods
#' @export
setMethod(
  f="generate.transects",
  signature="Line.Transect.Design",
  definition=function(object, region = NULL, silent = FALSE){
      return(NULL)
  }
)
