#' @include Transect.R
#' @importFrom methods validObject
#' @import sf
NULL

#' @title  Class "Region"
#'
#' @description Class \code{"Region"} is an S4 class containing descriptions of the
#' study area. Uses an object of class
#'
#' @name Region-class
#' @title S4 Class "Region"
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{make.region(region.name = "region.name", shapefile = region.shapefile)}
#' @slot region.name Object of class \code{"character"}; giving the
#'  name of the region.
#' @slot strata.name Object of class \code{"character"}; character
#'  vector giving the names of the strata.
#' @slot units Object of class \code{"character"}; character describing
#'  the coordinate units ("km" or "m")
#' @slot area Object of class \code{"numeric"}; the area of the survey
#'  region
#' @slot region Object of class \code{"sf"} defining the survey region
#' @section Methods:
#' \describe{
#'  \item{\code{get.area}}{\code{signature(obj = "Region")}: retrieves the area
#'  element }
#'  \item{\code{plot}}{\code{signature(x = "Region", y = "missing")}: plots
#'  the survey region defined by the object.}
#' }
#' @keywords classes
#' @seealso \code{\link{make.region}}
setClass(Class = "Region",
         representation(region.name = "character",
                        strata.name = "character",
                        units = "character",
                        area = "numeric",
                        region = "list")
)

setMethod(
  f="initialize",
  signature="Region",
  definition=function(.Object, region.name = character(0), strata.name = character(0), units = character(0), sf.shape = NULL){
    #calculates the strata areas
    area <- as.numeric(sf::st_area(sf.shape))
    if(length(units) == 0){
      tmp <- sf::st_crs(sf.shape)
      if(!is.na(tmp)){
        if(is.null(tmp$units)){
          units <- units
          warning("Coordinate reference system detected but no units can be found. Has this shape been projected - shapefiles must be projected on to a flat plane before surveys are designed. dssd is unstable and may generate errors when working with unprojected regions.", call. = FALSE, immediate. = TRUE)
        }else
          units <- tmp$units
      }else{
        units <- units
      }
    }
    #Set slots
    .Object@region.name <- region.name
    .Object@strata.name <- strata.name
    .Object@units       <- units
    .Object@area        <- area
    .Object@region      <- sf.shape
    #Check object is valid
    valid <- try(validObject(.Object), silent = TRUE)
    if(class(valid) == "try-error"){
      stop(attr(valid, "condition")$message, call. = FALSE)
    }
    # return object
    return(.Object)
  }
)

setValidity("Region",
            function(object){
              if(length(object@strata.name) > 0){
                strata.name <- object@strata.name
                # Check that none are Total
                if(any(strata.name == "Total")){
                  return("'Total' is not an accepted strata name, please ammend it.")
                }
                #check that they are all unique
                for(i in seq(along = strata.name)){
                  temp <- strata.name[-i]
                  for(j in seq(along = temp)){
                    if(strata.name[i] == temp[j]){
                      return("Stratum names must be unique.")
                    }
                  }
                }
              }
              if(length(which(object@area <= 0)) > 0){
                return("All areas must be greater than 0. ")
              }
              return(TRUE)
            }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' Returns the area of the region
#'
#' @param object object of class \code{Region}
#' @return numeric value specifying the area of the region
#' @rdname get.area-methods
setGeneric(name = "get.area", def = function(object){standardGeneric ("get.area")})

#' @rdname get.area-methods
setMethod(
  f="get.area",
  signature="Region",
  definition=function(object){
    return(object@area)
  }
)

#' Plot
#' @param x object of class Region or inheriting from Survey
#' @param y optionally a Survey object to plot with the Region
#' @param main the main title for the plot
#' @param cols colours for the strata
#' @param legend.params a list of parameters which affect the location and appearance
#' of the legend. 'inset' affects the location of the legend, 'cex' affects the text
#' size and 'wrap' is the number of character in a line before the text is wrapped on
#' to the next line.
#' @param ... other general plot parameters
#' @rdname plot.Region-methods
#' @importFrom graphics legend mtext
#' @exportMethod plot
setMethod(
  f="plot",
  signature="Region",
  definition=function(x, y, main = "", cols = "default", legend.params = list(inset = c(-0.2,0), cex = 0.75, wrap = 15), ...){
    # If main is not supplied then take it from the object
    if(main == ""){
      main <- x@region.name
    }
    additional.args <- list(...)
    subtitle <- ifelse("subtitle" %in% names(additional.args), additional.args$subtitle, "")
    if(length(x@strata.name) > 0){
      strata.names <- x@strata.name
    }else{
      strata.names <- x@region.name
    }
    if(!"inset" %in% names(legend.params)){
      legend.params$inset <- c(-0.2,0)
    }
    if(!"cex" %in% names(legend.params)){
      legend.params$cex <- 0.75
    }
    if(!"wrap" %in% names(legend.params)){
      legend.params$wrap <- 15
    }

    if(any(cols == "default")){
      if(length(x@strata.name) <= 15){
        cols <-  c("lavender","lemonchiffon", "thistle1", "lightsteelblue1", "paleturquoise1", "palegreen", "wheat1", "salmon1", "ivory1", "olivedrab1", "slategray1", "seashell1", "plum1", "khaki1", "snow1")[1:(length(x@strata.name))]
      }else{
        cols <-  rep(c("lavender","lemonchiffon", "thistle1", "lightsteelblue1", "paleturquoise1", "palegreen", "wheat1", "salmon1", "ivory1", "olivedrab1", "slategray1", "seashell1", "plum1", "khaki1", "snow1"),3)[1:(length(x@strata.name))]
      }
    }
    label_wrap <- function(text, width) {
      lapply(strwrap(as.character(text), width=width, simplify=FALSE),
             paste, collapse="\n")
    }
    strata.names <- unlist(label_wrap(strata.names, legend.params$wrap))
    region <- x@region
    sf.column <- attr(region, "sf_column")
    bbox <- sf::st_bbox(region)
    if(length(strata.names) > 1 && subtitle == ""){
      pmar <- par(mar=c(4, 4, 1, 8), xpd=TRUE)
      on.exit(par(mar = pmar))
    }else if(length(strata.names) > 1 && subtitle != ""){
      pmar <- par(mar=c(4, 4, 4, 8), xpd=TRUE)
      on.exit(par(mar = pmar))
    }else if(length(strata.names) == 1 && subtitle != ""){
      pmar <- par(mar=c(4, 4, 4, 1), xpd=TRUE)
      on.exit(par(mar = pmar))
    }
    if(length(x@units) > 0){
      x.label <- paste("x-coords (", x@units, ")", sep = "")
      y.label <- paste("y-coords (", x@units, ")", sep = "")
    }else{
      x.label <- "x.coordinates"
      y.label <- "y.coordinates"
    }
    plot(c(0,0), col = "white", xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax), main = main, xlab = x.label, ylab = y.label)
    for(i in seq(along = region[[sf.column]])){
      plot(region[[sf.column]][[i]], add = TRUE, col = cols[i])
    }
    if(length(strata.names) > 1){
      legend("topright", inset=legend.params$inset,
             legend=strata.names,
             pch = 20, col=cols, horiz=FALSE, bty='n',
             pt.cex = 3, cex = legend.params$cex)
    }
    mtext(subtitle, side = 3, line = 0.5, outer = FALSE)
    invisible(x)
  }
)



#' Plot
#'
#' Plots an S4 object of class 'Region'
#' @param region.col fill colours for strata
#' @rdname plot.Region-methods
#' @exportMethod plot
#' @importFrom graphics mtext
setMethod(
  f="plot",
  signature=c("Region", "Transect"),
  definition=function(x, y, main = "", region.col = "default", ...){
    # If main is not supplied then take it from the object
    if(main == ""){
      main <- x@region.name
    }
    additional.args <- list(...)
    subtitle <- ifelse("subtitle" %in% names(additional.args), additional.args$subtitle, "")
    if(region.col == "default"){
      if(length(x@strata.name) <= 15){
        region.col <-  c("lavender","lemonchiffon", "thistle1", "lightsteelblue1", "paleturquoise1", "palegreen", "wheat1", "salmon1", "ivory1", "olivedrab1", "slategray1", "seashell1", "plum1", "khaki1", "snow1")[1:(length(x@strata.name))]
      }else{
        region.col <-  rep(c("lavender","lemonchiffon", "thistle1", "lightsteelblue1", "paleturquoise1", "palegreen", "wheat1", "salmon1", "ivory1", "olivedrab1", "slategray1", "seashell1", "plum1", "khaki1", "snow1"),3)[1:(length(x@strata.name))]
      }
    }
    #Check additional attributes
    add.attrs <- list(...)
    cov.area <- ifelse("covered.area" %in% names(add.attrs), add.attrs$covered.area, FALSE)
    region <- x@region
    sf.column <- attr(region, "sf_column")
    #Set up bounding box for samplers (necessary when plus sampling used and extent of samplers is greater than the region)
    bbox.samps <- sf::st_bbox(y@samplers)
    bbox.region <- sf::st_bbox(x@region)
    if(subtitle != ""){
      pmar <- par(mar=c(4, 4, 5, 1), xpd=TRUE)
      on.exit(par(mar = pmar))
    }
    plot(c(0,0), col = "white", xlim = c(min(bbox.samps$xmin, bbox.region$xmin), max(bbox.samps$xmax, bbox.region$xmax)), ylim = c(min(bbox.samps$ymin, bbox.region$ymin), max(bbox.samps$ymax, bbox.region$ymax)), main = main, xlab = "x-coordinates", ylab = "y-coordinates")
    for(i in seq(along = region[[sf.column]])){
      plot(region[[sf.column]][[i]], add = TRUE, col = region.col[i])
    }
    if(cov.area){
      #plot the covered areas
      cov.area.shape <- y@cov.area.polys
      sf.col.ca <- attr(cov.area.shape, "sf_column")
      cov.area.shape <- cov.area.shape[[sf.col.ca]]
      for(i in seq(along = cov.area.shape)){
        plot(cov.area.shape[[i]], add = T)
      }
    }
    plot(y, add = TRUE, ...)
    mtext(subtitle, side = 3, line = 0.5, outer = FALSE)
    invisible(x)
  }
)

#' Plot
#' @rdname plot.Region-methods
#' @exportMethod plot
#' @importFrom graphics mtext
setMethod(
  f="plot",
  signature=c("Region", "Coverage.Grid"),
  definition=function(x, y, main = "", region.col = "default", ...){
    # If main is not supplied then take it from the object
    if(main == ""){
      main <- x@region.name
    }
    additional.args <- list(...)
    subtitle <- ifelse("subtitle" %in% names(additional.args), additional.args$subtitle, "")
    if(region.col == "default"){
      if(length(x@strata.name) <= 15){
        region.col <-  c("lavender","lemonchiffon", "thistle1", "lightsteelblue1", "paleturquoise1", "palegreen", "wheat1", "salmon1", "ivory1", "olivedrab1", "slategray1", "seashell1", "plum1", "khaki1", "snow1")[1:(length(x@strata.name))]
      }else{
        region.col <-  rep(c("lavender","lemonchiffon", "thistle1", "lightsteelblue1", "paleturquoise1", "palegreen", "wheat1", "salmon1", "ivory1", "olivedrab1", "slategray1", "seashell1", "plum1", "khaki1", "snow1"),3)[1:(length(x@strata.name))]
      }
    }
    #Check additional attributes
    add.attrs <- list(...)
    cov.area <- ifelse("covered.area" %in% names(add.attrs), add.attrs$covered.area, FALSE)
    region <- x@region
    sf.column <- attr(region, "sf_column")
    #Set up bounding box for samplers (necessary when plus sampling used and extent of samplers is greater than the region)
    #bbox.samps <- sf::st_bbox(y@samplers)
    bbox.region <- sf::st_bbox(x@region)
    if(subtitle != ""){
      pmar <- par(mar=c(4, 4, 5, 1), xpd=TRUE)
      on.exit(par(mar = pmar))
    }
    plot(c(0,0), col = "white", xlim = c(min(bbox.region$xmin), max(bbox.region$xmax)), ylim = c(min( bbox.region$ymin), max(bbox.region$ymax)), main = main, xlab = "x-coordinates", ylab = "y-coordinates")
    for(i in seq(along = region[[sf.column]])){
      plot(region[[sf.column]][[i]], add = TRUE, col = region.col[i])
    }
    plot(y, add = TRUE, ...)
    mtext(subtitle, side = 3, line = 0.5, outer = FALSE)
    invisible(x)
  }
)

