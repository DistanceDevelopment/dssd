#' Virtual Class "Transect"
#'
#' Virtual Class \code{"Transect"} is an S4 class detailing a single survey, a
#' single set of transects.
#'
#' @name Transect-class
#' @title S4 Class "Transect"
#' @slot design Describes the design algorithm used to create the survey.
#' @slot samplers Contains the survey transects
#' @slot no.samplers Numeric value(s) giving the number of realised transects.
#' @slot effort.allocation a vector of probabilities determining how effort is
#' allocated between strata. Effort allocated based on area if left empty.
#' @slot spacing determines the spacing of systematic samplers
#' @slot design.angle numeric value detailing the angle of the design. Can provide
#' multiple values relating to strata. The use of the angle varies with design, it
#' can be either the angle of the grid of points, the angle of lines or the design
#' axis for the zigzag design.
#' @slot edge.protocol character value indicating whether a "plus" sampling or
#' "minum" sampling protocol is used.
#' @slot cover.poly polygons outlining the covered area
#' @keywords classes
#' @export
#' @importFrom methods validObject
#' @seealso \code{\link{make.design}}
setClass(Class = "Transect",
         representation = representation(design = "character",
                                         samplers = "list",
                                         cov.area = "numeric",
                                         cov.area.polys = "list",
                                         no.samplers = "numeric",
                                         effort.allocation  = "numeric",
                                         spacing = "numeric",
                                         design.angle = "numeric",
                                         edge.protocol = "character", "VIRTUAL")
)

setValidity("Transect",
            function(object){
              return(TRUE)
            }
)

