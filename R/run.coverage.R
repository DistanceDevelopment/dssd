#' @export
run.coverage <- function(design, reps = 10){
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
  cov.area <- transect.count <- line.length <- matrix(rep(NA, reps*strata.count), ncol = strata.count, dimnames = list(1:reps, strata.names))
  total.hits <- rep(0, grid.count)
  for(rep in 1:reps){
    #Generate transects
    transects <- generate.transects(design)
    #Check coverage hits
    polys <- transects@cov.area.polys$geometry
    hits <- lapply(polys, FUN = inout, pts = pts)
    hits <- matrix(unlist(hits), nrow = grid.count)
    hits <- apply(hits, FUN = sum, MARGIN = 1)
    total.hits <- total.hits + hits
    #Harvest statistics
    #Coverered Area
    cov.area[rep,] <- transects@cov.area
    #Number of transects
    transect.count[rep,] <- transects@samp.count
    #Transect Length
    if(class(design) == "Line.Transect.Design"){
      line.length[rep,] <- transects@line.length
    }
    percent.complete <- round(rep/reps*100, 1)
    cat("\r", percent.complete, "% complete \r")
  }
  #Calculate summary statistics
  summary.stats <- list(sampler.count = summary(transect.count),
                        line.length = summary(line.length),
                        cov.area = summary(cov.area))
  design@coverage.grid@grid$coverage.scores <- total.hits/reps
  design@design.statistics <- summary.stats
  return(design)
}


