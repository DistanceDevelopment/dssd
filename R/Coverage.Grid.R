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
