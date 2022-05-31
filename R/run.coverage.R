#' @title run.coverage
#' @description This function can be used to assess the coverage of
#' a design and also assess design statistics, such as how the number
#' of samplers, the line length, trackline length or percentage
#' coverage varies between surveys generated from the same design.
#' It generates the specified number of surveys from the
#' design and looks to see which of the coverage grid points, a
#' systematic grid of points across the survey region, are included
#' in each survey. When calculating coverage scores if more than
#' one sampler falls on a grid point then that grid point gets
#' allocated the appropriate count. These counts are then averaged
#' over the number of surveys which have been generated. At the same
#' time it records the relevant statistics for the design. While 100
#' repetitions may be sufficient to get an idea of design statistics
#' 1000 or even more repetitions may be needed to gain a good
#' representation of the coverage scores across the study region.
#' @details See ?make.design for example code.
#' @param design an object which inherits from the Survey.Design
#' class.
#' @param reps the number of times you wish the coverage simulation
#' to be carried out.
#' @param save.transects a directory where the shapefiles for the
#' transects can be saved. The shapefile names will be S1, S2, ...
#' existing files in the directory will not be overwritten.
#' @param quiet when TRUE no progress counter is displayed.
#' @return this function returns the survey design object passed in
#' and it will now include the coverage and design statistics.
#' @seealso \link{make.design}
#' @export
#' @importFrom stats median sd
run.coverage <- function(design, reps = 10, save.transects = "", quiet = FALSE){
#Calculates the coverage scores for the design supplied
#Also stores summary statistics
#All values are returned within the design object
  inout <- function(poly, pts){
    inside <- sf::st_intersects(pts, poly, sparse = FALSE)
    inside <- ifelse(inside, 1, 0)
    return(inside)
  }
  #Check that the coverage grid has a grid!
  if(length(design@coverage.grid@grid) == 0){
    warning("No coverage grid, generating a default grid with 1000 points.", immediate. = TRUE, call. = FALSE)
    design@coverage.grid <- make.coverage(region = design@region)
  }
  coverage <- design@coverage.grid
  #Get region
  region <- design@region
  if(length(region@strata.name) > 0){
    strata.names <- region@strata.name
    strata.count <- length(region@strata.name)
  }else{
    strata.names <- region@region.name
    strata.count <- 1
  }
  #Get coverage grid points and a count
  pts <- coverage@grid$geometry
  grid.count <- length(coverage@grid$geometry)
  #Store values
  cov.area <- transect.count <- line.length <- trackline <- cyclictrackline <- matrix(rep(NA, reps*strata.count), ncol = strata.count, dimnames = list(1:reps, strata.names))
  total.hits <- rep(0, grid.count)
  for(rep in 1:reps){
    #Generate transects
    transects <- generate.transects(design, quiet = TRUE)
    #if the user wants the transects saved write them to file
    if(save.transects != ""){
      suppressMessages(write.transects(transects, paste(save.transects, "/S", rep, ".shp", sep = "")))
    }
    if(is.null(transects)){
      warning("No transects generated, coverage run cancelled. Please check your design.", immediate. = T, call. = FALSE)
      return(design)
    }
    #Check coverage hits
    polys <- transects@cov.area.polys$geometry
    hits <- lapply(polys, FUN = inout, pts = pts)
    hits <- matrix(unlist(hits), nrow = grid.count)
    hits <- apply(hits, FUN = sum, MARGIN = 1)
    #Allows the user to switch between coverage assessment methods
    #if(method == "inclusion"){
    #  hits <- ifelse(hits > 1, 1, hits)
    #}
    total.hits <- total.hits + hits
    #Harvest statistics
    #Coverered Area
    cov.area[rep,] <- transects@cov.area
    #Number of transects
    transect.count[rep,] <- transects@samp.count
    #Transect Length
    if(inherits(design, "Line.Transect.Design")){
      line.length[rep,] <- transects@line.length
      trackline[rep,] <- transects@trackline
      cyclictrackline[rep,] <- transects@cyclictrackline
    }
    if(!quiet){
      percent.complete <- round((rep/reps)*100, 1)
      message("\r  ", percent.complete, "% complete      \r", appendLF = FALSE)
    }
  }
  #Calculate summary statistics
  sampler.summary <- matrix(rep(NA, 5*(strata.count+1)), ncol = (strata.count+1), dimnames = list(c("Minimum", "Mean", "Median", "Maximum", "sd"), c(strata.names, "Total")))
  sampler.summary[1,1:strata.count] <- apply(transect.count, 2, min)
  sampler.summary[2,1:strata.count] <- apply(transect.count, 2, mean)
  sampler.summary[3,1:strata.count] <- apply(transect.count, 2, median)
  sampler.summary[4,1:strata.count] <- apply(transect.count, 2, max)
  sampler.summary[5,1:strata.count] <- apply(transect.count, 2, sd)
  sampler.totals <- apply(transect.count, 1, FUN = sum, na.rm = T)
  sampler.summary[1,(strata.count+1)] <- min(sampler.totals)
  sampler.summary[2,(strata.count+1)] <- mean(sampler.totals)
  sampler.summary[3,(strata.count+1)] <- median(sampler.totals)
  sampler.summary[4,(strata.count+1)] <- max(sampler.totals)
  sampler.summary[5,(strata.count+1)] <- sd(sampler.totals)
  sampler.summary <- round(sampler.summary, 1)


  cov.area.summary <- matrix(rep(NA, 5*(strata.count+1)), ncol = (strata.count+1), dimnames = list(c("Minimum", "Mean", "Median", "Maximum", "sd"), c(strata.names, "Total")))
  cov.area.summary[1,1:strata.count] <- apply(cov.area, 2, min)
  cov.area.summary[2,1:strata.count] <- apply(cov.area, 2, mean)
  cov.area.summary[3,1:strata.count] <- apply(cov.area, 2, median)
  cov.area.summary[4,1:strata.count] <- apply(cov.area, 2, max)
  cov.area.summary[5,1:strata.count] <- apply(cov.area, 2, sd)
  cov.area.totals <- apply(cov.area, 1, FUN = sum, na.rm = T)
  cov.area.summary[1,(strata.count+1)] <- min(cov.area.totals)
  cov.area.summary[2,(strata.count+1)] <- mean(cov.area.totals)
  cov.area.summary[3,(strata.count+1)] <- median(cov.area.totals)
  cov.area.summary[4,(strata.count+1)] <- max(cov.area.totals)
  cov.area.summary[5,(strata.count+1)] <- sd(cov.area.totals)
  cov.area.summary <- round(cov.area.summary, 2)

  areas <- region@area
  cov.area.percent <- matrix(rep(NA, 5*(strata.count+1)), ncol = (strata.count+1), dimnames = list(c("Minimum", "Mean", "Median", "Maximum", "sd"), c(strata.names, "Total")))
  cov.area.percent[1,1:strata.count] <- (apply(cov.area, 2, min)/areas)*100
  cov.area.percent[2,1:strata.count] <- (apply(cov.area, 2, mean)/areas)*100
  cov.area.percent[3,1:strata.count] <- (apply(cov.area, 2, median)/areas)*100
  cov.area.percent[4,1:strata.count] <- (apply(cov.area, 2, max)/areas)*100
  cov.area.percent[5,1:strata.count] <- (apply(cov.area, 2, sd)/areas)*100
  cov.area.totals <- apply(cov.area, 1, FUN = sum, na.rm = T)/sum(areas)*100
  cov.area.percent[1,(strata.count+1)] <- min(cov.area.totals)
  cov.area.percent[2,(strata.count+1)] <- mean(cov.area.totals)
  cov.area.percent[3,(strata.count+1)] <- median(cov.area.totals)
  cov.area.percent[4,(strata.count+1)] <- max(cov.area.totals)
  cov.area.percent[5,(strata.count+1)] <- sd(cov.area.totals)
  cov.area.percent <- round(cov.area.percent, 2)

  summary.stats <- list(sampler.count = sampler.summary,
                        cov.area = cov.area.summary,
                        p.cov.area = cov.area.percent)

  if(inherits(design, "Line.Transect.Design")){
    line.len.summary <- matrix(rep(NA, 5*(strata.count+1)), ncol = (strata.count+1), dimnames = list(c("Minimum", "Mean", "Median", "Maximum", "sd"), c(strata.names, "Total")))
    line.len.summary[1,1:strata.count] <- apply(line.length, 2, min)
    line.len.summary[2,1:strata.count] <- apply(line.length, 2, mean)
    line.len.summary[3,1:strata.count] <- apply(line.length, 2, median)
    line.len.summary[4,1:strata.count] <- apply(line.length, 2, max)
    line.len.summary[5,1:strata.count] <- apply(line.length, 2, sd)
    line.len.totals <- apply(line.length, 1, FUN = sum, na.rm = T)
    line.len.summary[1,(strata.count+1)] <- min(line.len.totals)
    line.len.summary[2,(strata.count+1)] <- mean(line.len.totals)
    line.len.summary[3,(strata.count+1)] <- median(line.len.totals)
    line.len.summary[4,(strata.count+1)] <- max(line.len.totals)
    line.len.summary[5,(strata.count+1)] <- sd(line.len.totals)
    line.len.summary <- round(line.len.summary, 2)
    summary.stats$line.length <- line.len.summary

    trackline.summary <- matrix(rep(NA, 5*(strata.count+1)), ncol = (strata.count+1), dimnames = list(c("Minimum", "Mean", "Median", "Maximum", "sd"), c(strata.names, "Total")))
    trackline.summary[1,1:strata.count] <- apply(trackline, 2, min)
    trackline.summary[2,1:strata.count] <- apply(trackline, 2, mean)
    trackline.summary[3,1:strata.count] <- apply(trackline, 2, median)
    trackline.summary[4,1:strata.count] <- apply(trackline, 2, max)
    trackline.summary[5,1:strata.count] <- apply(trackline, 2, sd)
    trackline.totals <- apply(trackline, 1, FUN = sum, na.rm = T)
    trackline.summary[1,(strata.count+1)] <- min(trackline.totals)
    trackline.summary[2,(strata.count+1)] <- mean(trackline.totals)
    trackline.summary[3,(strata.count+1)] <- median(trackline.totals)
    trackline.summary[4,(strata.count+1)] <- max(trackline.totals)
    trackline.summary[5,(strata.count+1)] <- sd(trackline.totals)
    trackline.summary <- round(trackline.summary, 2)
    summary.stats$trackline <- trackline.summary

    cyclictrackline.summary <- matrix(rep(NA, 5*(strata.count+1)), ncol = (strata.count+1), dimnames = list(c("Minimum", "Mean", "Median", "Maximum", "sd"), c(strata.names, "Total")))
    cyclictrackline.summary[1,1:strata.count] <- apply(cyclictrackline, 2, min)
    cyclictrackline.summary[2,1:strata.count] <- apply(cyclictrackline, 2, mean)
    cyclictrackline.summary[3,1:strata.count] <- apply(cyclictrackline, 2, median)
    cyclictrackline.summary[4,1:strata.count] <- apply(cyclictrackline, 2, max)
    cyclictrackline.summary[5,1:strata.count] <- apply(cyclictrackline, 2, sd)
    cyclictrackline.totals <- apply(cyclictrackline, 1, FUN = sum, na.rm = T)
    cyclictrackline.summary[1,(strata.count+1)] <- min(cyclictrackline.totals)
    cyclictrackline.summary[2,(strata.count+1)] <- mean(cyclictrackline.totals)
    cyclictrackline.summary[3,(strata.count+1)] <- median(cyclictrackline.totals)
    cyclictrackline.summary[4,(strata.count+1)] <- max(cyclictrackline.totals)
    cyclictrackline.summary[5,(strata.count+1)] <- sd(cyclictrackline.totals)
    cyclictrackline.summary <- round(cyclictrackline.summary, 2)
    summary.stats$cyclictrackline <- cyclictrackline.summary
  }
  design@coverage.scores <- total.hits/reps
  design@design.statistics <- summary.stats
  design@coverage.reps <- reps
  return(design)
}


