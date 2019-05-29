#' Virtual Class "Survey.Design"
#'
#' Virtual Class \code{"Survey.Design"} is an S4 class detailing the survey
#' design.
#'
#' @name Survey.Design-class
#' @title S4 Class "Survey.Design"
#' @slot region An object of class 'Region' defining the study area.
#' @slot design Character value describing the name of the design.
#' @slot samplers Numeric values defining the number of samplers in each
#' stratum.
#' @slot effort.allocation numeric values used to indicate the proportion of effort
#' to be allocated to each strata from number of samplers or line length. If length 0,
#' effort allocated based on stratum area.
#' @slot spacing used by systematic designs, numeric value to define spacing
#' between transects.
#' @slot design.angle numeric value detailing the angle of the design. Can provide
#' multiple values relating to strata. The use of the angle varies with design, it
#' can be either the angle of the grid of points, the angle of lines or the design
#' axis for the zigzag design.
#' @slot edge.protocol Character value defining whether a "minus" or "plus"
#' sampling strategy should be used.
#' @slot truncation Object of class \code{"numeric"}; The maximum distance
#' at which observations can be made. This is used to determine the covered
#' area during the coverage calculations.
#' @keywords classes
#' @export
#' @importFrom methods validObject
#' @seealso \code{\link{make.design}}
setClass(Class = "Survey.Design",
         representation = representation(region = "Region",
                                         design = "character",
                                         samplers = "numeric",
                                         effort.allocation  = "numeric",
                                         spacing = "numeric",
                                         design.angle = "numeric",
                                         edge.protocol = "character",
                                         truncation = "numeric",
                                         coverage.grid = "Coverage.Grid",
                                         coverage.scores = "numeric",
                                         design.statistics = "list", "VIRTUAL")
)

setValidity("Survey.Design",
            function(object){
              #Check how many strata there are
              strata.count <- length(object@region@strata.name)
              #EFFORT ALLOCATION
              if(length(object@effort.allocation) > 0){
                if(sum(object@effort.allocation, na.rm = T) != 1){
                  return("Effort allocation should either be omitted or sum to 1")
                }
                if(any(is.na(object@effort.allocation))){
                  return("Sorry, effort allocation is only applied across all strata at present. NA values are not permitted.")
                }
              }
              #TRUNCATION
              if(length(object@truncation) > 1){
                warning("You have supplied more than one truncation value. Currently the same truncation value must be applied across the entire study region. Using only the first value supplied.")
                object@truncation <- object@truncation[1]
              }else if(object@truncation <= 0){
                return("The truncation distance must be > 0.")
              }
              #Check edge protocol
              if(length(object@edge.protocol) == 1){
                object@edge.protocol <- rep(object@edge.protocol, strata.count)
              }else if(length(object@edge.protocol) > 1 && length(object@edge.protocol) != strata.count){
                warning("Edge protocol argument has a different number of values than there are strata, only using the 1st value.")
                object@edge.protocol <- rep(object@edge.protocol[1], strata.count)
              }
              if(!all(object@edge.protocol %in% c("minus", "plus"))){
                warning("Some edge protocol option(s) not recognised using minus sampling for these strata.", call. = FALSE)
                index <- which(!(object@edge.protocol %in% c("minus", "plus")))
                object@edge.protocol[index] <- "minus"
              }
              #DESIGN ANGLE
              #Check the design angle
              if(length(object@design.angle) == 1){
                object@design.angle <- rep(object@design.angle, strata.count)
              }else if(length(object@design.angle) > 1 && length(object@design.angle) != strata.count){
                warning("Design angle argument has a different number of values than there are strata, only using the 1st value.")
                object@design.angle <- rep(object@design.angle[1], strata.count)
              }
              if(any(object@design.angle < 0 || object@design.angle >= 180)){
                return("The design angle should be >= 0 and < 180 degrees.")
              }
              return(TRUE)
            }
)


# GENERIC METHODS DEFINITIONS --------------------------------------------

#' Plot
#'
#' Plots an S4 object of class 'Survey.Design'
#'
#' @param x object of class Survey.Design
#' @param y not used
#' @param ... other general plot parameters
#' @rdname plot.Survey.Design-methods
#' @exportMethod plot
setMethod(
  f="plot",
  signature="Survey.Design",
  definition=function(x, y, ...){
    #Check coverage has been run
    if(all(is.na(x@coverage.scores))){
      stop("Design has not been run yet, all coverage scores are NA.", call. = FALSE)
    }
    # If main is not supplied then take it from the object
    additional.args <- list(...)
    col.breaks <- ifelse("col.breaks" %in% names(additional.args), additional.args$col.breaks, 10)
    coverage.scores <- x@coverage.scores
    pmar <- par(mar = c(1, 1, 4, 5))
    on.exit(par(mar = pmar))
    plot(x@region@region$geometry, main = "Coverage Scores", cex.main = 1.5)
    cols <- heat.colors(col.breaks)[as.numeric(cut(coverage.scores, breaks = col.breaks))]
    plot(x@coverage.grid@grid$geometry, pch = 20, col = cols, add = T)
    plot3D::colkey(side = 4, clim = range(coverage.scores), col = heat.colors(col.breaks), add = TRUE, length = 0.7)
    invisible(x)
  }
)


#' show
#'
#' Summarises an S4 object of class 'Survey.Design'
#'
#' @param x object of class Survey.Design
#' @param y not used
#' @param ... other general plot parameters
#' @rdname show.Survey.Design-methods
#' @exportMethod show
setMethod(
  f="show",
  signature="Survey.Design",
  definition=function(object){
    print(object@design.statistics)
  }
)


