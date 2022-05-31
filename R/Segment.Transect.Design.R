#' @include generic.functions.R
#' @include Line.Transect.Design.R
#' @importFrom methods validObject

#' @title Class "Segment.Transect.Design" extends Class "Survey.Design"
#'
#' @description Class \code{"Segment.Transect.Design"} is an S4 class detailing
#' the a segmented line transect design.
#' @name Segment.Transect.Design-class
#' @title S4 Class "Segment.Transect.Design"
#' @slot seg.length length of the transect segment.
#' @slot seg.threshold this is a percentage threshold value applicable to segmented
#' grid designs controlling which partial segments are discarded around the survey
#' region boundary. By default, the value of 50, means that only segments that are
#' more than half inside the survey region will be retained. To retain all segments,
#' no matter how small they are when clipped to the survey region boundary set this
#' value to 0.
#' @slot offset a value to offset a return transect by so segments become pairs of
#' segments (not yet implemented).
#' @section Methods:
#' \describe{
#'  \item{\code{generate.transects}}{\code{signature=(object = "Line.Transect.Design", quiet = FALSE, ...)}:
#'  generates a set of transects from the design.}
#' }
#' @keywords classes
#' @seealso \code{\link{make.design}}
#' @export
setClass(Class = "Segment.Transect.Design",
         representation = representation(seg.length = "numeric",
                                         seg.threshold = "numeric",
                                         offset = "numeric"),
         contains = "Line.Transect.Design"
)

setMethod(
  f="initialize",
  signature="Segment.Transect.Design",
  definition=function(.Object, region, truncation, design, line.length, seg.length, effort.allocation, spacing, samplers, design.angle, edge.protocol, seg.threshold, bounding.shape, coverage.grid){
    #Set slots
    .Object@region        <- region
    .Object@truncation    <- truncation
    .Object@design        <- design
    .Object@line.length   <- line.length
    .Object@seg.length    <- seg.length
    .Object@effort.allocation <- effort.allocation
    .Object@spacing       <- spacing
    .Object@samplers      <- samplers
    .Object@design.angle  <- design.angle
    .Object@edge.protocol <- edge.protocol
    .Object@seg.threshold <- seg.threshold
    .Object@bounding.shape <- bounding.shape
    .Object@coverage.grid <- coverage.grid
    .Object@coverage.scores <- numeric(0)
    .Object@design.statistics <- data.frame()
    #Check object is valid (testing now done in Class constructor)
    valid <- try(validObject(.Object), silent = TRUE)
    if(inherits(valid, "try-error")){
      stop(attr(valid, "condition")$message, call. = FALSE)
    }
    # return object
    return(.Object)
  }
)

