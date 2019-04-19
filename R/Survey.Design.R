#' Virtual Class "Survey.Design"
#'
#' Virtual Class \code{"Survey.Design"} is an S4 class detailing the survey
#' design.
#'
#' @name Survey.Design-class
#' @title S4 Class "Survey.Design"
#' @slot truncation Object of class \code{"numeric"}; The maximum distance
#' at which observations can be made. This is used to determine the covered
#' area during the coverage calculations.
#' @slot no.samplers Numeric values defining the number of samplers in each
#' stratum.
#' @slot edge.protocol Character value defining whether a "minus" or "plus"
#' sampling strategy should be used.
#' @keywords classes
#' @export
#' @seealso \code{\link{make.design}}
setClass(Class = "Survey.Design",
         representation = representation(truncation = "numeric",
                                         no.samplers = "numeric",
                                         edge.protocol = "character", "VIRTUAL")
)


