#' @include Transect.R
#' @importFrom methods validObject

#' @title Class "Line.Transect" extends Class "Transect"
#'
#' @description Class \code{"Line.Transect"} is an S4 class
#' detailing a set of transects from a point transect design.
#' @name Line.Transect-class
#' @title S4 Class "Line.Transect"
#' @slot line.length the total line length for the transect set
#' @slot trackline the total on and off effort trackline length from
#' the start of the first transect to the end of the last
#' @slot cyclictrackline the trackline distance plus the distance
#' required to return from the end of the last transect to the
#' beginning of the first
#' @keywords classes
#' @seealso \code{\link{make.design}}
#' @export
setClass(Class = "Line.Transect",
         representation = representation(line.length = "numeric",
                                         trackline = "numeric",
                                         cyclictrackline = "numeric"),
         contains = "Transect")

setMethod(
  f="initialize",
  signature="Line.Transect",
  definition=function(.Object, design, lines, samp.count, line.length, seg.length, 
                      effort.allocation, spacing, design.angle, edge.protocol, 
                      cov.area = numeric(0), cov.area.polys = list(), strata.area, 
                      strata.names, trackline, cyclictrackline){
    #Set slots
    .Object@strata.names  <- strata.names
    .Object@design        <- design
    .Object@samplers      <- lines
    .Object@strata.area   <- strata.area
    .Object@cov.area      <- cov.area
    .Object@cov.area.polys <- cov.area.polys
    .Object@line.length   <- line.length
    .Object@trackline     <- trackline
    .Object@cyclictrackline <- cyclictrackline
    .Object@samp.count    <- samp.count
    .Object@effort.allocation <- effort.allocation
    .Object@spacing       <- spacing
    .Object@design.angle  <- design.angle
    .Object@edge.protocol <- edge.protocol
    #Check object is valid
    valid <- try(validObject(.Object), silent = TRUE)
    if(inherits(valid, "try-error")){
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
#' Plots an S4 object of class 'Transect'
#'
#' @param x object of class transect
#' @param y not used
#' @param ... Additional arguments: add (TRUE/FALSE) whether to add to existing
#' plot, col colour, lwd line width (for line transects) and pch point symbols
#' (for point transects).
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
    #add.cover.area <- ifelse("add.cover.area" %in% names(additional.args), additional.args$add.cover.area, FALSE)
    sf.column.poly <- attr(x@cov.area.polys, "sf_column")
    sf.column.samps <- attr(x@samplers, "sf_column")
    if(length(x@samplers) > 0){
      #if(add.cover.area){
      #  plot(x@cov.area.polys[[sf.column.poly]], add = add, col = 4)
      #}
      plot(x@samplers[[sf.column.samps]], add = add, col = col, lwd = lwd)
    }else{
      warning("No samplers to plot", call. = F, immediate. = F)
    }
    invisible(x)
  }
)


#' Show
#'
#' Displays details of an S4 object of class 'Transect'
#' @param object an object of class Transect
#' @rdname show.Transect-methods
#' @exportMethod show
setMethod(
  f="show",
  signature="Line.Transect",
  definition=function(object){
    strata.names <- object@strata.names
    for(strat in seq(along = object@design)){
      title <- paste("\n   Strata ", strata.names[strat], ":", sep = "")
      len.title <- nchar(title)
      underline <- paste("   ", paste(rep("_", (len.title-3)), collapse = ""), sep = "")
      cat(title, fill = T)
      cat(underline, fill = T)
      design <- switch(object@design[strat],
                       "random" = "randomly located transects",
                       "systematic" = "systematically spaced parallel transects",
                       "eszigzag" = "equal spaced zigzag",
                       "eszigzagcom" = "complementaty equal spaced zigzags",
                       "segmentedgrid" = "segmented grid")
      cat("Design: ", design, fill = T)
      if(object@design[strat] %in% c("systematic", "eszigzag", "eszigzagcom", "segmentedgrid")){
        cat("Spacing: ", object@spacing[strat], fill = T)
      }
      cat("Line length:", object@line.length[strat], fill = T)
      if(object@design[strat] == "segmentedgrid"){
        cat("Segment length: ", object@seg.length[strat], fill = T)
        cat("Segment threshold: ", object@seg.threshold[strat], fill = T)
      }
      cat("Trackline length:", object@trackline[strat], fill = T)
      cat("Cyclic trackline length:", object@cyclictrackline[strat], fill = T)
      cat("Number of samplers: ", object@samp.count[strat], fill = T)
      cat("Design angle: ", object@design.angle[strat], fill = T)
      cat("Edge protocol: ", object@edge.protocol[strat], fill = T)
      cat("Covered area: ", object@cov.area[strat], fill = T)
      cat("Strata coverage: ", round((object@cov.area[strat]/object@strata.area[strat])*100,2), "%", fill = T, sep = "")
      cat("Strata area: ", object@strata.area[strat], fill = T)
    }
    #Now print totals
    cat("\n   Study Area Totals:", fill = T)
    cat("   _________________", fill = T)
    cat("Line length:", sum(object@line.length, na.rm = T), fill = T)
    cat("Trackline length:", sum(object@trackline, na.rm = T), fill = T)
    cat("Cyclic trackline length:", sum(object@cyclictrackline, na.rm = T), fill = T)
    cat("Number of samplers: ", sum(object@samp.count, na.rm = T), fill = T)
    if(length(object@effort.allocation) > 0){
      cat("Effort allocation: ", paste(object@effort.allocation*100, collapse = "%, "), "%", fill = T, sep = "")
    }
    cat("Covered area: ", sum(object@cov.area, na.rm = T), fill = T)
    index <- which(!is.na(object@cov.area))
    cat("Average coverage: ", round((sum(object@cov.area[index])/sum(object@strata.area))*100,2), "%", fill = T, sep = "")
    invisible(object)
  }
)
