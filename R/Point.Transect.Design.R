#' @include generic.functions.R
#' @include Survey.Design.R

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
         representation = representation(design = "character",
                                         spacing = "numeric",
                                         design.angle = "numeric"),
         contains = "Survey.Design"
)


setMethod(
  f="initialize",
  signature="Point.Transect.Design",
  definition=function(.Object, truncation, design, spacing, no.samplers, design.angle, edge.protocol){
    #Set slots
    .Object@truncation    <- truncation
    .Object@design        <- design
    .Object@spacing       <- spacing
    .Object@no.samplers   <- no.samplers
    .Object@design.angle  <- design.angle
    .Object@edge.protocol <- edge.protocol
    #Check object is valid
    validObject(.Object)
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
  definition=function(object, region = NULL, silent = FALSE){
    return(NULL)
  }
)
