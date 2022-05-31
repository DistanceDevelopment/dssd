#' @include generic.functions.R

#' @title Virtual Class "Survey.Design"
#'
#' @description Virtual Class \code{"Survey.Design"} is an S4 class detailing the survey
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
#' @slot coverage.reps The number of times the coverage simulation was repeated.
#' @slot design.statistics A list of values obtained when investigating
#' coverage. This includes the minimum, maximum, mean and median
#' @section Methods:
#' \describe{
#'  \item{\code{generate.transects}}{\code{signature 'Survey.Design'}:
#'  Generates a set of transects from the design.}
#'  \item{\code{plot}}{\code{signature 'Survey.Design,ANY'}:
#'  Plots the coverage scores contained within an object of class 'Survey.Design' and
#'  provides a colour key relating to the coverage scores. This allows the user to
#'  assess how even the coverage is across the survey region.}
#'  \item{\code{show}}{\code{signature 'Survey.Design'}:
#'  Gives a summary of the design description, stratum areas and coverage scores
#'  if the coverage simulation has been run on the design. The coverage score summary
#'  details the minimum, maximum, mean and medium coverage scores across the study
#'  region. Also gives summaries of other design measures such as the number of
#'  samplers, line length, trackline length, cyclic trackline length, covered area
#'  and percentage of region covered.}
#' }
#' @keywords classes
#' @export
#' @importFrom methods validObject
#' @importFrom stats sd
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
                                         coverage.reps = "numeric",
                                         design.statistics = "list", "VIRTUAL")
)


# GENERIC METHODS DEFINITIONS --------------------------------------------

#' Plot
#'
#' Plots the coverage scores contained within an object of class 'Survey.Design' and
#' provides a colour key relating to the coverage scores. This allows the user to
#' assess how even the coverage is across the survey region.
#'
#' @param x object of class Survey.Design
#' @param y not used
#' @param strata.id a numeric value indicating the index of the strata you wish to plot.
#' @param col.breaks the number of break point in the colour scale representing the
#' coverage scores.
#' @param subtitle a subtitle for the plot.
#' @param ... not implemented for this class.
#' @rdname plot.Survey.Design-methods
#' @exportMethod plot
#' @importFrom graphics par mtext
#' @importFrom grDevices heat.colors
#' @importFrom ggplot2 ggplot geom_sf scale_color_viridis_c ggtitle
setMethod(
  f="plot",
  signature="Survey.Design",
  definition=function(x, y, strata.id = numeric(0), col.breaks = NULL, subtitle = "", ...){
    #Check coverage has been run
    if(all(is.na(x@coverage.scores))){
      warning("Design has not been run yet, all coverage scores are NA.", call. = FALSE)
      return(invisible(NULL))
    }
    # If main is not supplied then take it from the object
    additional.args <- list(...)
    strata.id <- ifelse(length(strata.id) == 0, "all", strata.id)
    # Get shape column names
    sf.column.region <- attr(x@region@region, "sf_column")
    sf.column.grid <- attr(x@coverage.grid@grid, "sf_column")
    # Extract coverage scores, coverage grid and region coords
    coverage.scores <- x@coverage.scores
    coverage.grid <- x@coverage.grid@grid
    coverage.grid$coverage.scores <- coverage.scores
    region.coords <- x@region@region[[sf.column.region]]
    
    # If not plotting all strata extract values for specific strata
    if(strata.id != "all"){
      strata.coords <- region.coords[[strata.id]]
      strata.coords <- sf::st_sfc(strata.coords, crs = sf::st_crs(x@region@region))
      coverage.grid <- suppressWarnings(sf::st_intersection(coverage.grid, strata.coords))
      plot.coords <- strata.coords
    }else{
      plot.coords <- region.coords
    }
    
    # Plot code
    gplot <- ggplot()+
      geom_sf(data = coverage.grid, aes(col = coverage.scores), size = 1.2)+
      scale_color_viridis_c(n.breaks = col.breaks)+
      geom_sf(data = plot.coords, fill = NA, lwd = 0.5, col = 1)+
      ggtitle("Coverage Scores")
    plot(gplot)
    
    invisible(gplot)
  }
)

#' show
#'
#' Summarises and displays an S4 object of class 'Survey.Design'
#'
#' @param object an object which inherits from the Survey.Design class
#' @rdname show.Survey.Design-methods
#' @exportMethod show
setMethod(
  f="show",
  signature="Survey.Design",
  definition=function(object){
    strata.names <- object@region@strata.name
    for(strat in seq(along = strata.names)){
      title <- paste("\n   Strata ", strata.names[strat], ":", sep = "")
      len.title <- nchar(title)
      underline <- paste("   ", paste(rep("_", (len.title-3)), collapse = ""), sep = "")
      cat(title, fill = T)
      cat(underline, fill = T)
      design <- switch(object@design[strat],
                       "random" = "randomly located transects",
                       "systematic" = "systematically spaced transects",
                       "eszigzag" = "equal spaced zigzag",
                       "eszigzagcom" = "complementaty equal spaced zigzags",
                       "segmentedgrid" = "segmented grid")
      cat("Design: ", design, fill = T)
      if(object@design[strat] %in% c("systematic", "eszigzag", "eszigzagcom", "segmentedgrid")){
        cat("Spacing: ", object@spacing[strat], fill = T)
      }
      if(length(object@samplers) == 1){
        cat("Number of samplers: ", object@samplers, " (shared across strata)", fill = T)
      }else{
        cat("Number of samplers: ", object@samplers[strat], fill = T)
      }
      line.length <- try(object@line.length, silent = TRUE)
      if(!inherits(line.length, "try-error")){
        if(length(line.length) == 1){
          cat("Line length: ", line.length, " (shared across strata)", fill = T)
        }else if(length(line.length) == length(strata.names)){
          cat("Line length: ", line.length[strat], fill = T)
        }else{
          cat("Line length: NA", fill = T)
        }
      }
      if(object@design[strat] %in% c("segmentedgrid")){
        cat("Segment length: ", object@seg.length[strat], fill = T)
        cat("Segment threshold: ", object@seg.threshold[strat], fill = T)
      }
      cat("Design angle: ", object@design.angle[strat], fill = T)
      cat("Edge protocol: ", object@edge.protocol[strat], fill = T)
    }
    dp <- ifelse(any(object@region@area < 10), 3, 0)
    cat("\nStrata areas: ", paste(round(object@region@area, dp), collapse = ", "), fill = T)
    if(length(object@region@units) > 0){
      if(!inherits(line.length, "try-error")){
        cat("Region and effort units: ", object@region@units, fill = T)
      }else{
        cat("Region units: ", object@region@units, fill = T)
      }
    }
    if(length(object@effort.allocation) > 0){
      cat("Effort allocation across strata: ", paste(object@effort.allocation*100, collapse = "%, "), "%", sep = "", fill = T)
    }
    if(length(object@coverage.scores) > 0){
      cat("Coverage Simulation repetitions: ", object@coverage.reps, fill = T)
    }

    design.stats <- object@design.statistics
    names.stats <- names(design.stats)
    for(i in seq(along = design.stats)){
     title <- switch(names.stats[i],
                     "sampler.count" = "Number of samplers:",
                     "cov.area" = "Covered area:",
                     "p.cov.area" = "% of region covered:",
                     "line.length" = "Line length:",
                     "trackline" = "Trackline length:",
                     "cyclictrackline" = "Cyclic trackline length:")
     cat("\n   ", title, fill = T)
     underline <- paste(rep("", (nchar(title)-3)), collapse = "")
     cat("   ", underline, fill = T)
     print(design.stats[[i]])
    }
    if(!all(is.na(object@coverage.scores))){
      title <- "Coverage Score Summary:"
      cat("\n   ", title, fill = T)
      underline <- paste(rep("", (nchar(title)-3)), collapse = "")
      cat("   ", underline, fill = T)
      cov.scores <- array(NA, dim = c(5, (length(strata.names)+1)), dimnames = list(c("Minimum", "Mean", "Median", "Maximum", "sd"), c(strata.names, "Total")))
      for(i in seq(along = strata.names)){
        cov.strat <- get.coverage(object, i)
        cov.scores["Minimum",i] <- min(cov.strat, na.rm = T)
        cov.scores["Mean",i] <- mean(cov.strat, na.rm = T)
        cov.scores["Median",i] <- median(cov.strat, na.rm = T)
        cov.scores["Maximum",i] <- max(cov.strat, na.rm = T)
        cov.scores["sd",i] <- sd(cov.strat, na.rm = T)
      }
      #Add in total column
      cov.scores["Minimum","Total"] <- min(object@coverage.scores, na.rm = T)
      cov.scores["Mean","Total"] <- mean(object@coverage.scores, na.rm = T)
      cov.scores["Median","Total"] <- median(object@coverage.scores, na.rm = T)
      cov.scores["Maximum","Total"] <- max(object@coverage.scores, na.rm = T)
      cov.scores["sd","Total"] <- sd(object@coverage.scores, na.rm = T)
      print(cov.scores)
    }
  }
)


#' @rdname get.coverage-methods
#' @exportMethod get.coverage
setMethod(
  f="get.coverage",
  signature="Survey.Design",
  definition=function(object, strata.id = "all"){
    #Check coverage has been run
    if(all(is.na(object@coverage.scores))){
      stop("Design has not been run yet, all coverage scores are NA.", call. = FALSE)
    }else{
      if(strata.id != "all"){
        # Get shape column names
        sf.column.region <- attr(object@region@region, "sf_column")
        sf.column.grid <- attr(object@coverage.grid@grid, "sf_column")
        # Extract coverage scores and region coords
        coverage.scores <- object@coverage.scores
        region.coords <- object@region@region[[sf.column.region]]
        coverage.grid <- object@coverage.grid@grid
        region.coords <- region.coords[[strata.id]]
        region.coords <- sf::st_sfc(region.coords, crs = sf::st_crs(object@region@region))
        coverage.grid <- object@coverage.grid@grid
        coverage.grid$coverage.scores <- coverage.scores
        #For backwards compatibility as the crs didn't use to be stored for the coverage grid
        if(sf::st_crs(coverage.grid) != sf::st_crs(region.coords)){
          if(is.na(sf::st_crs(coverage.grid))){
            warning("Coverage grid has no coordinate reference system (crs), it may have been made in a previous verson of dssd. dssd will assume crs is the same as the region crs.", call. = FALSE)
            sf::st_crs(coverage.grid) <- sf::st_crs(object@region@region)
          }else{
            stop("Coverage grid and region have different coordinate reference systems.")
          }
        }
        coverage.grid <- suppressWarnings(sf::st_intersection(coverage.grid, region.coords))
        coverage.scores <- coverage.grid$coverage.scores
        return(coverage.scores)
      }
      return(object@coverage.scores)
    }
  }
)


