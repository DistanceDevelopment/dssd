#' @include generic.functions.R
#' @include Survey.Design.R
#' @include Region.R
#' @importFrom methods validObject

#' @title Virtual Class "Point.Transect.Design" extends Class "Survey.Design"
#'
#' @description Virtual Class \code{"Point.Transect.Design"} is an S4 class detailing
#' the type of point transect design.
#' @name Point.Transect.Design-class
#' @title S4 Class "Point.Transect.Design"
#' @section Methods:
#' \describe{
#'  \item{\code{generate.transects}}{\code{signature=(object = "Point.Transect.Design", ...)}:
#'  generates a set of transects from a shapefile.}
#' }
#' @keywords classes
#' @seealso \code{\link{make.design}}
#' @export
setClass(Class = "Point.Transect.Design",
         representation = representation(),
         contains = "Survey.Design"
)


setMethod(
  f="initialize",
  signature="Point.Transect.Design",
  definition=function(.Object, region, truncation, design, spacing, no.samplers, effort.allocation, design.angle, edge.protocol){
    #Set slots
    .Object@region        <- region
    .Object@truncation    <- truncation
    .Object@design        <- design
    .Object@spacing       <- spacing
    .Object@no.samplers   <- no.samplers
    .Object@effort.allocation <- effort.allocation
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

setValidity("Point.Transect.Design",
            function(object){
              return(TRUE)
            }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' @rdname generate.transects-methods
#' @export
setMethod(
  f="generate.transects",
  signature="Point.Transect.Design",
  definition=function(object, silent = FALSE){
    if(object@design == "systematic"){
      transects <- generate.systematic.points(object)
    }else{
      message("This design is not supported at present")
      transects = NULL
    }
    return(transects)
  }
)
