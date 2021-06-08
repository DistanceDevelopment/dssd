#' Survey effort for distance sampling
#'
#' @description Computes the predicted required survey effort to achieve a range of target CV's (coefficient of variation) values given pilot survey information. This information is displayed in a plot if the number of cv.values is greater than or equal to 5. The plot values for the target CV versus effort are returned as a data.frame (invisibly for 5 or more CV values).
#'
#' @param L0 effort deployed in pilot study (line length or number points)
#' @param n0 number of detections during pilot study
#' @param q approximate variance in density from pilot study. Default value 3.
#' @param line.point switch indicating whether intended survey is line or point
#' @param cv.values CV values you wish the function to calculate the effort for. Defaults to a range of 100 values between 0.075 and 0.3.
#' @author Eric Rexstad (aut), Laura Marshall (ctb)
#'
#' @details
#' Horizontal and vertical lines are added to the plot at approximately every
#' 0.1 CV interval. The exact values displayed will be the closest point equal
#' to these values or the next smallest CV value and its corresponding effort
#' from the data.frame.
#'
#' Computations based on formulas for points and lines found in
#' Section 2.4.2 of Buckland et al. (2015).
#' @references
#' Buckland, S. T., Rexstad, E. A., Marques, T. A., & Oedekoven, C. S.
#' (2015). Distance Sampling: Methods and Applications.
#' Springer International Publishing.
#' https://doi.org/10.1007/978-3-319-19219-2
#' @export
#' @return Plots the target cv vs effort (if the length of cv.values is >= 5)
#' Returns a data.frame (invisibly if the length of cv.values is >= 5) containing two fields
#' \itemize{
#'   \item L - effort, either length of line transect or number points
#'   \item cv - precision (cv) expected from given effort
#' }
#'
#' @examples
#' # Line transect pilot survey with 20 sightings on a line of length
#' # 5 units.
#' calculate.effort(L0 = 5, n0 = 20)
#'
#' # Point transect pilot with 20 sightings over 5 points
#' calculate.effort(L0 = 5, n0 = 20, line.point="point")
#' # To find a single value for a target CV of 0.15
#' calculate.effort(L0 = 5, n0 = 20, line.point="point", cv.values = 0.15)
#'
calculate.effort <- function(L0, n0, q=3, line.point="line",
                             cv.values = seq(0.075, 0.30, length=100)) {
  # Input validation
  if(L0 <= 0 || n0 <= 0 || any(cv.values <= 0)){
    stop("Values for L0, n0 and all cv.values must be positive.", call. = FALSE)
  }
  if(!line.point %in% c("line", "point")){
    stop("The value of line.point must either be 'line' or 'point'", call. = FALSE)
  }
  # Function
  which.kind <- ifelse(line.point=="line", "Line length", "Number of points")
  # Sort cv.values
  cv.values <- sort(cv.values)
  # Effort calculations
  encrate.inv <- L0/n0
  E <- q/cv.values^2 * encrate.inv
  result <- data.frame(Effort = E, CV = cv.values)
  # If there are less than 5 cv.values then no plot just return the data.frame
  if(length(cv.values) < 5){
    return(result)
  }
  plot(result, pch=20,
       main=paste(which.kind, "to achieve target CV",
                  "\nL0=", L0, "n0=", n0),
       xlab=which.kind, ylab="Target CV", xaxt="none", yaxt="none")
  axis(1, seq(0, max(result$Effort), by=10))
  axis(2, seq(min(cv.values)+.005, max(cv.values), by=0.01), las=2)

  drawlines <- function(cv, result) {
    Lvalue <- result[max(which(result$CV <= cv)),1]
    cvvalue <- result[max(which(result$CV <= cv)),2]
    segments(-5, cvvalue, Lvalue, cvvalue, col="red")
    segments(Lvalue, cvvalue, Lvalue, 0)
    text(Lvalue, min(cv.values), round(Lvalue,1), cex=0.8, pos=4, offset=0.25)
  }
  # Find the points for the lines
  cv.points <- seq(0.1, 0.9, by = 0.1)
  cv.points <- cv.points[cv.points > min(cv.values) & cv.points < max(cv.values)]
  # Add lines to plot
  sapply(cv.points, FUN = drawlines, result = result)
  # Invisibly return data.frame plot values
  invisible(result)
}
