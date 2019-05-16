#' @importFrom methods validObject
NULL

#' @title  Class "Coverage.Grid"
#'
#' @description Class \code{"Coverage.Grid"} is an S4 class containing descriptions of a
#' grid used to assess the coverage scores of different designs.
#'
#' @name Coverage.Grid-class
#' @title S4 Class "Coverage.Grid"
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{make.grid(region = make.region(), no.points = 1000, spacing = numeric(0)}
#' @slot grid sf multipoint object
#' @slot spacing the spacing used to create the coverage grid
#' @section Methods:
#' \describe{
#'  \item{\code{plot}}{\code{signature(x = "Coverage.Grid", y = "missing")}: plots
#'  the grid of points.}
#' }
#' @keywords classes
#' @seealso \code{\link{make.grid}}
setClass(Class = "Coverage.Grid",
         representation(grid = "list",
                        spacing = "numeric")
)

setMethod(
  f="initialize",
  signature="Coverage.Grid",
  definition=function(.Object, grid, spacing){
    #Set slots
    .Object@grid      <- grid
    .Object@spacing   <- spacing
    #Check object is valid
    valid <- try(validObject(.Object), silent = TRUE)
    if(class(valid) == "try-error"){
      stop(attr(valid, "condition")$message, call. = FALSE)
    }
    # return object
    return(.Object)
  }
)
setValidity("Coverage.Grid",
            function(object){
              return(TRUE)
            }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' Plot
#'
#' Plots an S4 object of class 'Survey'
#'
#' @param x object of class Survey
#' @param y not used
#' @param ... other general plot parameters
#' @rdname plot.Survey-methods
#' @exportMethod plot
setMethod(
  f="plot",
  signature="Coverage.Grid",
  definition=function(x, y, ...){
    # If main is not supplied then take it from the object
    additional.args <- list(...)
    add <- ifelse("add" %in% names(additional.args), additional.args$add, FALSE)
    col <- ifelse("col" %in% names(additional.args), additional.args$col, 5)
    pch <- ifelse("pch" %in% names(additional.args), additional.args$pch, 20)
    if(length(x@grid) > 0){
      plot(x@grid, add = add, col = col, pch = pch)
    }else{
      warning("No grid points to plot", call. = F, immediate. = F)
    }
    invisible(x)
  }
)
