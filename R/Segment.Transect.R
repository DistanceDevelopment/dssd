#' @include Segment.Transect.R
#' @importFrom methods validObject

#' @title Class "Segmemt.Transect" extends Class "Line.Transect"
#'
#' @description Class \code{"Segment.Transect"} is an S4 class
#' detailing a set of transects from a point transect design.
#' @name Segment.Transect-class
#' @title S4 Class "Segment.Transect"
#' @slot seg.length length of the transect segment.
#' @slot seg.threshold this is a percentage threshold value applicable to segmented
#' grid designs controlling which partial segments are discarded around the survey
#' region boundary. By default, the value of 50, means that only segments that are
#' more than half inside the survey region will be retained. To retain all segments,
#' no matter how small they are when clipped to the survey region boundary set this
#' value to 0.
#' @slot offset a value to offset a return transect by so segments become pairs of
#' segments (not yet implemented).
#' @keywords classes
#' @seealso \code{\link{make.design}}
#' @export
setClass(Class = "Segment.Transect",
         representation = representation(seg.length = "numeric",
                                         seg.threshold = "numeric",
                                         offset = "numeric"),
         contains = "Line.Transect")

setMethod(
  f="initialize",
  signature="Segment.Transect",
  definition=function(.Object, design, lines, samp.count, line.length, seg.length, effort.allocation,
                      spacing, design.angle, edge.protocol, cov.area = numeric(0),
                      cov.area.polys = list(), strata.area, strata.names, trackline,
                      cyclictrackline, seg.threshold){
    #Set slots
    .Object@strata.names  <- strata.names
    .Object@design        <- design
    .Object@samplers      <- lines
    .Object@strata.area   <- strata.area
    .Object@cov.area      <- cov.area
    .Object@cov.area.polys <- cov.area.polys
    .Object@line.length   <- line.length
    .Object@seg.length    <- seg.length
    .Object@trackline     <- trackline
    .Object@cyclictrackline <- cyclictrackline
    .Object@samp.count    <- samp.count
    .Object@effort.allocation <- effort.allocation
    .Object@spacing       <- spacing
    .Object@design.angle  <- design.angle
    .Object@edge.protocol <- edge.protocol
    .Object@seg.threshold <- seg.threshold
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
