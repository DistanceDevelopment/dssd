#' @include generic.functions.R
#' @include Survey.Design.R
#' @include Region.R
#' @importFrom methods validObject

#' @title Virtual Class "Line.Transect.Design" extends Class "Survey.Design"
#'
#' @description Virtual Class \code{"Line.Transect.Design"} is an S4 class detailing
#' the type of line transect design.
#' @name Line.Transect.Design-class
#' @title S4 Class "Line.Transect.Design"
#' @slot line.length Numeric value defining the total line length to be generated (may be
#' multiple values relating to each stratum).
#' @slot bounding.shape relevant for zigzag designs, either a minimum bounding "rectangle"
#' or a "convex hull".
#' @section Methods:
#' \describe{
#'  \item{\code{generate.transects}}{\code{signature=(object = "Line.Transect.Design", ...)}:
#'  generates a set of transects from a shapefile.}
#' }
#' @keywords classes
#' @seealso \code{\link{make.design}}
#' @export
setClass(Class = "Line.Transect.Design",
         representation = representation(line.length = "numeric",
                                         bounding.shape = "character"),
         contains = "Survey.Design"
)


setMethod(
  f="initialize",
  signature="Line.Transect.Design",
  definition=function(.Object, region, truncation, design, line.length, effort.allocation, spacing, samplers, design.angle, edge.protocol, bounding.shape, coverage.grid){
    #Set slots
    .Object@region        <- region
    .Object@truncation    <- truncation
    .Object@design        <- design
    .Object@line.length   <- line.length
    .Object@effort.allocation <- effort.allocation
    .Object@spacing       <- spacing
    .Object@samplers      <- samplers
    .Object@design.angle  <- design.angle
    .Object@edge.protocol <- edge.protocol
    .Object@bounding.shape <- bounding.shape
    .Object@coverage.grid <- coverage.grid
    .Object@coverage.scores <- numeric(0)
    .Object@design.statistics <- data.frame()
    #Check object is valid
    valid <- try(validObject(.Object), silent = TRUE)
    if(class(valid) == "try-error"){
      stop(attr(valid, "condition")$message, call. = FALSE)
    }
    # return object
    return(.Object)
  }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' @rdname generate.transects-methods
#' @export
setMethod(
  f="generate.transects",
  signature="Line.Transect.Design",
  definition=function(object, silent = FALSE){
#This function separates the design generation by strata so different strata can have different designs in them. Assumes that the validation method called when the class is initialised checks that all design options either have length 1 or length equal to the number of strata. Also assumes that the region object has been checked and confimred to have the correct number of strata names for the size of the geometry.
    # Get strata names
    region <- object@region
    sf.column <- attr(region@region, "sf_column")
    if(length(region@strata.name) > 0){
      strata.names <- region@strata.name
      strata.no <- length(region@strata.name)
    }else{
      strata.names <- region@region.name
      strata.no <- 1
    }
    #Get a vector of designs
    if(length(object@design) == 1){
      design <- rep(object@design, strata.no)
    }else{
      design <- object@design
    }
    #Calculate effort allocation if only only one design or if using total line length and only if spacing has not been specified.
    if((length(object@design) == 1 || length(object@line.length) == 1 || length(object@samplers) == 1) && length(object@spacing) == 0){
      if(length(object@effort.allocation) == 0){
        #Use area
        effort.allocation <- region@area/sum(region@area)
      }else{
        effort.allocation <- object@effort.allocation
      }
    }
    #Extract design parameters
    spacing <- object@spacing
    samplers <- object@samplers
    line.length <- object@line.length
    #Check if only has one has been
    if(length(spacing) == 1){
      spacing <- rep(spacing, strata.no)
      by.spacing <- rep(TRUE, strata.no)
    }else if(length(spacing) == strata.no){
      by.spacing <- ifelse(is.na(spacing), FALSE, TRUE)
    }else if(length(spacing) == 0){
      by.spacing = rep(FALSE, strata.no)
    }
    #If spacing has not been provided for any
    if(all(!by.spacing)){
      #If only a total number of samplers has been provided (and there is only one design)
      if(length(samplers) == 1){
        if(!"random" %in% object@design && length(object@effort.allocation) == 0){
          #Calculate spacing across entire study region for more equal effort (it will only be truly equal if the same design is used across strata)
          width <- calc.region.width(object)
          spacing <- width/samplers
          spacing <- ifelse(design == "eszigzagcom", spacing*2, spacing)
          by.spacing <- rep(TRUE, strata.no)
        }else{
          #Have to allocate number of samplers per strata
          samplers <- samplers*effort.allocation
        }
      }
      #If only a total line.length has been supplied
      if(length(line.length) == 1){
        width <- calc.region.width(object)
        ave.line.height <- (sum(object@region@area)/width)
        if(length(object@effort.allocation) == 0){
          if(all(design == "systematic")){
            tot.samplers <- line.length/ave.line.height
            spacing <- rep(width/tot.samplers, strata.no)
            by.spacing <- rep(TRUE, strata.no)
          }else if(all(design == "eszigzag")){
            spacing = (width * ave.line.height) / sqrt(line.length^2 - width^2)
            spacing <- rep(spacing, strata.no)
            by.spacing <- rep(TRUE, strata.no)
          }else if(all(design == "eszigzagcom")){
            spacing = (width * ave.line.height) / sqrt((line.length/2)^2 - width^2)
            spacing <- rep(spacing, strata.no)
            by.spacing <- rep(TRUE, strata.no)
          }else{
            #there is a mix of designs or they are random
            line.length <- line.length*effort.allocation
          }
        }else{
          #Have to allocate line.length / number of samplers per strata
          line.length <- line.length*effort.allocation
        }
      }
    #If spacing has been provided for some but not all - deal with this in validation function!
    #Need to check only one option supplied for each strata
    # }else if(any(!by.spacing) && !all(!by.spacing)){
    #   for(strat in seq(along = strata.names)){
    #     if(!by.spacing[strat]){
    #     }
    #   }
    }
    #Store all lines in a list
    transects <- list()
    polys <- list()
    #Iterate over strata calling the appropriate method for the design.
    #Main grid generation
    for (strat in seq(along = region@region[[sf.column]])) {
      if(design[strat] %in% c("systematic","random")){
        temp <- generate.parallel.lines(object, strat, samplers[strat], line.length[strat], spacing[strat], by.spacing[strat])
        transects[[strat]] <- temp$transects
        polys[[strat]] <- temp$cover.polys
      }else if(design[strat] == "eszigzag" || design[strat] == "eszigzagcom"){
        temp <-  generate.eqspace.zigzags(object, strat, samplers[strat], line.length[strat], spacing[strat], by.spacing[strat])
        transects[[strat]] <- temp$transects
        polys[[strat]] <- temp$cover.polys
      }else{
        message("This design is not supported at present")
        transects[[strat]] = NULL
      }
    }
    #Put transects into a multipart, linestring/multilinestring objects
    #Need to retain transect IDs as well as strata for lines
    cov.areas <- line.length <- sampler.count <- numeric(0)
    transect.count <- 0
    strata.id <- character(0)
    for(strat in seq(along = transects)){
      for(i in seq(along = transects[[strat]])){
        if(strat == 1 && i == 1){
          temp <- sf::st_sfc(transects[[strat]][[i]])
          temp.poly <- sf::st_sfc(polys[[strat]][[i]])
          transect.count <- 1
          strata.id <- strata.names[strat]
        }else{
          temp <- c(temp, sf::st_sfc(transects[[strat]][[i]]))
          temp.poly <- c(temp.poly, sf::st_sfc(polys[[strat]][[i]]))
          transect.count <- transect.count + 1
          strata.id <- c(strata.id, strata.names[strat])
        }
      }
      line.length[strat] <- sum(unlist(lapply(transects[[strat]], FUN = sf::st_length)))
      cov.areas[strat] <- sum(unlist(lapply(polys[[strat]], FUN = sf::st_area)))
      sampler.count[strat] <- length(transects[[strat]])
    }
    all.transects <- sf::st_sf(data.frame(transect = 1:transect.count, strata = strata.id, geom = temp))
    all.polys <- sf::st_sf(data.frame(transect = 1:transect.count, strata = strata.id, geom = temp.poly))
    #Make a survey object
    transect <- new(Class="Line.Transect", design = object@design, lines = all.transects, samp.count = sampler.count, line.length = line.length, effort.allocation = object@effort.allocation, spacing = spacing, design.angle = object@design.angle, edge.protocol = object@edge.protocol, cov.area = cov.areas, cov.area.polys = all.polys, strata.area = region@area)
    return(transect)
  }
)
