#' Virtual Class "Survey"
#'
#' Virtual Class \code{"Survey"} is an S4 class detailing a single survey, a
#' single set of transects.
#'
#' @name Survey-class
#' @title S4 Class "Survey"
#' @slot design Describes the design algorithm used to create the survey.
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
#' @keywords classes
#' @export
#' @seealso \code{\link{make.design}}
setClass(Class = "Survey.Design",
         representation = representation(design = "character",
                                         no.samplers = "numeric",
                                         effort.allocation  = "numeric",
                                         spacing = "numeric",
                                         design.angle = "numeric",
                                         edge.protocol = "character", "VIRTUAL")
)


setValidity("Survey.Design",
            function(object){
              return(TRUE)
            }
)

