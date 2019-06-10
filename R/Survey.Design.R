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
#' @slot coverage.grid The coverage grid used to assess the uniformity
#' of coverage during simulations.
#' @slot coverage.scores The average number of times each point in the
#' coverage grid is included in a survey.
#' @slot design.statistics A list of values obtained when investigating
#' coverage. This includes the minimum, maximum, mean and median
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
#' @importFrom plot3D colkey
#' @importFrom graphics par
#' @importFrom grDevices heat.colors
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
    plot(x@region@region$geometry, add = T)
    plot3D::colkey(side = 4, clim = range(coverage.scores), col = heat.colors(col.breaks), add = TRUE, length = 0.7)
    invisible(x)
  }
)


#' show
#'
#' Summarises and displays an S4 object of class 'Survey.Design'
#'
#' @param object an object which inherits from the Survey.Design class
#' @param ... other general plot parameters
#' @rdname show.Survey.Design-methods
#' @exportMethod show
setMethod(
  f="show",
  signature="Survey.Design",
  definition=function(object){
    design.stats <- object@design.statistics
    names.stats <- names(design.stats)
    for(i in seq(along = design.stats)){
     title <- switch(names.stats[i],
                     "sampler.count" = "Number of samplers:",
                     "cov.area" = "Covered area:",
                     "p.cov.area" = "% of region covered:",
                     "line.length" = "Line length:")
     cat("\n   ", title, fill = T)
     underline <- paste(rep("", (nchar(title)-3)), collapse = "")
     cat("   ", underline, fill = T)
     print(design.stats[[i]])
    }
  }
)


