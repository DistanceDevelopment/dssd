#' @include generic.functions.R
#' @include Survey.Design.R
#' @include Region.R
#' @importFrom methods validObject

#' @title Virtual Class "Line.Transect.Design" extends Class "Survey.Design"
#'
#' @description Virtual Class \code{"Line.Transect.Design"} is an S4 class detailing
#' the type of line transect design.
#' @name Line.Transect.Design-class
#' @title S4 Class "Line.Transect.Design"
#' @slot line.length Numeric value defining the total line length to be generated (may be
#' multiple values relating to each stratum).
#' @slot bounding.shape relevant for zigzag designs, either a minimum bounding "rectangle"
#' or a "convex hull".
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
  definition=function(.Object, region, truncation, design, line.length, effort.allocation,
                      spacing, no.samplers, design.angle, edge.protocol,
                      bounding.shape){
    #Set slots
    .Object@region        <- region
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
  definition=function(object, silent = FALSE){
    if(object@design == "systematic"){
      transects <- generate.systematic.lines(object)
    }else{
      message("This design is not supported at present")
      transects = NULL
    }
    return(transects)
  }
)
