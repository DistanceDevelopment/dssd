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
                      spacing, design.angle, edge.protocol, cov.area = numeric(0),
                      cov.area.polys = list(), strata.area, strata.names){
    #Set slots
    .Object@strata.names  <- strata.names
    .Object@design        <- design
    .Object@samplers      <- points
    .Object@strata.area   <- strata.area
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
    col <- ifelse("col" %in% names(additional.args), additional.args$col, 4)
    pch <- ifelse("pch" %in% names(additional.args), additional.args$pch, 20)
    sf.column <- attr(x@samplers, "sf_column")
    if(length(x@samplers) > 0){
      plot(x@samplers[[sf.column]], add = add, col = col, pch = pch)
    }else{
      warning("No samplers to plot", call. = F, immediate. = F)
    }
    invisible(x)
  }
)

#' Show
#'
#' Displays details of an S4 object of class 'Transect'
#'
#' @rdname show.Transect-methods
#' @exportMethod show
setMethod(
  f="show",
  signature="Point.Transect",
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
                       "systematic" = "systematically spaced transects")
      cat("Design: ", design, fill = T)
      if(object@design[strat] == "systematic"){
        cat("Spacing: ", object@spacing[strat], fill = T)
      }
      cat("Number of samplers: ", object@samp.count[strat], fill = T)
      cat("Design angle: ", object@design.angle[strat], fill = T)
      cat("Edge protocol: ", object@edge.protocol[strat], fill = T)
      cat("Covered area: ", object@cov.area[strat], fill = T)
      cat("Strata coverage: ", round((object@cov.area[strat]/object@strata.area[strat])*100,2), "%", fill = T)
      cat("Strata area: ", object@strata.area[strat], fill = T)
    }
    #Now print totals
    cat("\n   Study Area Totals:", fill = T)
    cat("   _________________", fill = T)
    cat("Number of samplers: ", sum(object@samp.count, na.rm = T), fill = T)
    if(length(object@effort.allocation) > 0){
      cat("Effort allocation: ", paste(object@effort.allocation*100, collapse = "%, "), "%", fill = T)
    }
    cat("Covered area: ", sum(object@cov.area, na.rm = T), fill = T)
    index <- which(!is.na(object@cov.area))
    cat("Average coverage: ", round((sum(object@cov.area[index])/sum(object@strata.area))*100,2), "%", fill = T)
    invisible(object)
  }
)
