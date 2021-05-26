#' @importFrom methods new
#' @importFrom stats na.omit

#' @title Creates a Region object
#' @description This creates an instance of the Region class which defines the study
#' area for the survey.
#' @param region.name the region name
#' @param strata.name the stratum names (character vector, same length as the
#'   number of areas in the shapefile / sf object). If not supplied "A", "B",
#'   "C", ... will be assigned.
#' @param units measurement units; either \code{"m"} for metres or \code{"km"} for
#'   kilometres. If the shapefile has a projection file associated with it the units
#'   will be taken from there.
#' @param shape shapefile path to .shp file or an sf object of class sf, sfc or sfg.
#' @return object of class Region
#' @export
#' @author Laura Marshall
#' @examples
#' # A basic study rectangular study region
#' region <- make.region()
#' plot(region)
#'
#' #Load the region from a projected shapefile
#' shapefile.name <- system.file("extdata", "TrackExample.shp", package = "dssd")
#' region <- make.region(region.name = "study area",
#'                       shape = shapefile.name)
#' plot(region)
#'
#' #Load a multi strata unprojected shapefile
#' shapefile.name <- system.file("extdata", "AreaRStrata.shp", package = "dssd")
#' # Need to load shapefile first as it is not projected
#' sf.shape <- sf::read_sf(shapefile.name)
#' # Check current coordinate reference system
#' sf::st_crs(sf.shape)
#' # Define a European Albers Equal Area projection
#' proj4string <- "+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=-9 +x_0=0 +
#'                 y_0=0 +ellps=intl +units=km"
#' # Project the study area on to a flat plane
#' projected.shape <- sf::st_transform(sf.shape, crs = proj4string)
#' # Create region with default strata names
#' region <- make.region(region.name = "study area",
#'                       shape = projected.shape)
#' # By plotting the region we can verify the order of the strata
#' plot(region)
#'
make.region <- function(region.name = "region",
                        strata.name = character(0),
                        units = character(0),
                        shape = NULL){
  #Process shape
  if("sf" %in% class(shape)){
    sf.shape = shape
  }else if("sfc" %in% class(shape) || "sfg" %in% class(shape)){
    if("sfg" %in% class(shape)){
      shape <- list(shape)
    }
    if(length(strata.name) < length(shape) && length(shape) > 1){
      strata.name <- LETTERS[1:length(shape)]
      warning("Automatically naming strata as insufficient strata names provided. Assigned strata names:", paste(strata.name, collapse = ", "), call. = F, immediate. = T)
    }else if(length(strata.name) == 0 && length(shape) == 1){
      strata.name <- region.name
    }else if(length(strata.name) > length(shape)){
      strata.name <- strata.name[1:length(shape)]
    }
    sf.shape = sf::st_sf(strata = strata.name,  geom = shape)
  }else if(any(class(shape) %in% c("Polygon", "Polygons", "SpatialPolygons", "SpatialPolygonsDataFrame"))){
    stop("The sp data type is not yet supported... coming soon!")
  }else if(length(class(shape)) == 1 && !is.null(shape)){
    if(class(shape) == "character"){
      sf.shape <- sf::read_sf(shape)
    }else if(class(shape) == "list"){
      stop("The list data type is not yet supported... coming soon!")
    }
  }else if(is.null(shape)){
    #Make a default shape (same as in DSsim currently)
    sfc.shape <- sf::st_sfc(sf::st_polygon(list(matrix(c(0,0,0,500,2000,500,2000,0,0,0), ncol = 2, byrow = TRUE))))
    sf.shape <- sf::st_sf(data.frame(region="study_ar", geom=sfc.shape))
    strata.name = region.name
  }else{
    stop("This data type is not currently supported.")
  }
  # Check the number of strata names are correct
  sf.column <- attr(sf.shape, "sf_column")
  strata.count <- length(sf.shape[[sf.column]])
  if(strata.count == 1 && length(strata.name) == 0){
    strata.name <- region.name
  }else if(strata.count != length(strata.name)){
    if(length(sf.shape) <= 26){
      strata.name <- LETTERS[1:length(sf.shape[[sf.column]])]
      warning("Automatically naming strata as no strata names provided. Assigned strata names: ", paste(strata.name, collapse = ", "), call. = F, immediate. = T)
    }else{
      stop("Too many strata (>26) for strata names to be assigned default names, please provide strata names.", call. = FALSE)
    }
  }
  # Check the format of the shape
  sf.shape <- check.shape(sf.shape)
  # Call to make the region object
  region <- new(Class="Region", region.name = region.name, strata.name = strata.name, units = units, sf.shape = sf.shape)
  return(region)
}

#' @title  Creates a Survey.Design object
#' @description Creates a description of a survey design. Designs may use different
#' types of either point or line transect designs across strata but cannot mix
#' point and line transect design types within a single design object.
#'
#' @details
#'
#' \strong{Plus versus Minus Sampling}
#' If you choose for your design to use a minus sampling strategy then transects will
#' only be generated within the survey region and will give lower coverage around the
#' edge of the survey region. Plus sampling generates transects within an area
#' greater than the study region. To do this \pkg{dssd} first puts a buffer around the
#' study region before generating the transects within the buffered region. The width
#' of the buffer is the truncation distance supplies by the user. Plus sampling
#' helps to ensure more even coverage around the edge of the study area. See
#' \emph{Buckland et. al, 2001} "Introduction to Distance Sampling" for information
#' on when to use plus versus minus sampling.
#'
#' \strong{Point Transect Designs}
#' For point transect designs the user may either specify "random" or
#' "systematic" for the design argument. If the user specifies "random", they
#' should also provide a value for effort detailing the number of point transects
#' they wish their survey to have. For stratified designs they may specify a vector
#' of numbers detailing the number of transects per strata or alternatively use the
#' effort.allocation argument to allocate a total effort amount proportionally. If
#' effort.allocation is left blank then effort will be allocated according to strata
#' area. If the user specified"systematic" they may either provide their desired number
#' of samplers or a value for spacing which defines the gap between each of the
#' points (again a vector of spacing values can be provided for each strata).
#' Optionally the user may select a design.angle. For both random and systematic
#' point transect designs the user may select either a minus or plus sampling edge
#' protocol.
#'
#' \strong{Line Transect Designs:}
#' For line transect designs the user may either specify "random" (randomly
#' placed full width lines), "systematic" (systematically placed full width lines),
#' "eszigzag" (equally spaced zigzag lines), "eszigzagcom" (two sets of complementary
#' equally spaced zigzag lines) or "segmentedgrid" (a grid of short line transect
#' segments). If the user specifies "random", they should provide the either the
#' number of samplers they wish the design to generate or the line length they wish
#' to achieve, either by strata or as a total. If the user specifies "systematic"
#' they should specify either the number of samplers, the desired line length or
#' the spacing between lines. The design angle for these parallel line designs
#' refers to the angle of the lines where 0 is a vertical line and moving round
#' in a clockwise direction. If the user specifies a zigzag design they should
#' specify the systematic spacing value, number of samplers or line length to be
#' used and should choose between generating the design in a minimum bounding
#' rectangle or a convex hull. The default is minimum bounding rectangle which gives
#' more even coverage but the convex hull is generally more efficient. A segmented
#' grid design may be generated using the either the number of samplers or total
#' line length, combined with a value for segment length. Alternatively the user
#' may specify a values for spacing and segment length. The segmented grid design
#' also uses the segment threshold argument. All the designs may be generated
#' using plus or minus sampling protocols. Similar to the point transect designs
#' different values may be specified for each strata for all of the above options.
#' The design angle for the zigzag designs refers to the angle of a line which
#' would run through the middle of each zigzag transect if the zigzags were to
#' be generated within a rectangle. The design angle for zigzags should usually
#' run along the longest dimension of the study region.
#'
#' NOTE: If multiple design effort arguments are supplied (i.e. spacing, samplers,
#' line.length) then only the first of spacing then line.length then number of
#' samplers will be used. The other values provided will be discarded as designs
#' must only be based on one of these parameters. Different design effort arguments
#' may supplied for different strata, however.
#'
#' See the Getting Started Vignette and the Multiple Strata in dssd Vignette for
#' example designs.
#'
#' @param region an object of class Region defining the survey region.
#' @param transect.type character variable specifying either "line" or "point"
#' @param design a character variable describing the type of design. Either "random",
#' "systematic", "eszigzag" (equal-spaced zigzag), "eszigzagcom" (equal spaced zigzag
#' with complementary lines) or "segmentedgrid". See details for more information.
#' @param samplers the number of samplers you wish the design to generate
#' (note that the number actually generated may differ slightly due to the
#' shape of the study region for some designs). This may be one value or a value
#' for each stratum.
#' @param line.length the total line length you desire or a vector of line lengths
#' the same length as the number of strata.
#' @param seg.length the length of the line transect segments for a segmented grid
#' design.
#' @param effort.allocation numeric values used to indicate the proportion of effort
#' to be allocated to each strata from number of samplers or line length. If length is
#' 0 (the default) and only a total line length or total number of samplers is supplied,
#' effort is allocated based on stratum area.
#' @param design.angle numeric value detailing the angle of the design. Can provide
#' multiple values relating to strata. The use of the angle varies with design, it
#' can be either the angle of the grid of points, the angle of lines or the design
#' axis for the zigzag design. See details. In addition, a value of -1 will cause a
#' random design angle to be generated.
#' @param spacing used by systematic designs, numeric value(s) to define spacing
#' between transects. Can be a vector of values with one value per stratum.
#' @param edge.protocol character value indicating whether a "plus" sampling or
#' "minus" sampling protocol is used. See details.
#' @param seg.threshold this is a percentage threshold value applicable to segmented
#' grid designs controlling which partial segments are discarded around the survey
#' region boundary. By default, the value of 50, means that only segments that are
#' more than half inside the survey region will be retained. To retain all segments,
#' no matter how small they are when clipped to the survey region boundary set this
#' value to 0.
#' @param bounding.shape only applicable to zigzag designs. A character value saying
#' whether the zigzag transects should be generated using a minimum bounding
#' "rectangle" or a "convex hull". The default is a minimum bounding rectangle.
#' @param truncation A single numeric value describing the longest distance at which
#' an object may be observed. Truncation distance is constant across strata.
#' @param coverage.grid An object of class Coverage.Grid for use when
#' running the coverage simulation.
#' @return object of a class which inherits from class Survey.Design either
#' Line.Transect.Design or Point.Transect.Design
#' @export
#' @author Laura Marshall
#' @examples
#' #Point transect example
#' shapefile.name <- system.file("extdata", "TrackExample.shp", package = "dssd")
#' region <- make.region(region.name = "study area",
#'                      shape = shapefile.name)
#'
#' \donttest{
#' # Generate coverage grid (spacing quite sparse for speed)
#' cover <- make.coverage(region,
#'                        n.grid.points = 250)
#'
#' # Define design
#' design <- make.design(region = region,
#'                       transect.type = "point",
#'                       design = "random",
#'                       samplers = 25,
#'                       design.angle = 45,
#'                       edge.protocol = "minus",
#'                       truncation = 3,
#'                       coverage.grid = cover)
#'
#' # Generate a single survey instance
#' survey <- generate.transects(design)
#' plot(region, survey, covered.area = TRUE)
#'
#' # Warning! this will take some time to run
#' design <- run.coverage(design, reps = 500)
#' # Plot the coverage
#' plot(design)
#' # Display the design statistics
#' design
#' #Extract coverage scores
#' coverage.scores <- get.coverage(design)
#' hist(coverage.scores)
#'
#' #Multi-strata line transect example
#' shapefile.name <- system.file("extdata", "AreaRProjStrata.shp", package = "dssd")
#' region <- make.region(region.name = "study area",
#'                      strata.name = c("North", "NW", "West Upper",
#'                                      "West Lower", "SW", "South"),
#'                      shape = shapefile.name)
#' plot(region)
#' # Make a coverage grid
#' cover <- make.coverage(region,
#'                        n.grid.points = 500)
#' # Define the design
#' design <- make.design(region = region,
#'                       transect.type = "line",
#'                       design = c("systematic", "systematic",
#'                                  "eszigzag", "systematic",
#'                                  "systematic", "eszigzagcom"),
#'                       line.length = 5000*1000, #5000km x 1000m (projection in m)
#'                       design.angle = c(160, 135, 170, 135, 50, 60),
#'                       edge.protocol = "minus",
#'                       truncation = 3000,
#'                       coverage.grid = cover)
#'
#' # Create a single set of transects to check
#' survey <- generate.transects(design)
#' plot(region, survey, covered.area = TRUE)
#'
#' # Warning! this will quite a long time to run as it is a complex example.
#' design <- run.coverage(design, reps = 500)
#' # Plot the coverage
#' plot(design)
#' # Display the design statistics
#' design
#' # Extract coverage scores for the first strata
#' coverage.scores <- get.coverage(design, strata.id = 1)
#' summary(coverage.scores)
#' }
#'
#' # Fast running example for CRAN testing purposes
#' # This spacing is too sparse to assess coverage in a real example and
#' # the number of repetitions is too low to assess design statistics
#' cover <- make.coverage(region,
#'                        n.grid.points = 50)
#' design <- make.design(region = region,
#'                       transect.type = "point",
#'                       design = "random",
#'                       samplers = 25,
#'                       design.angle = 45,
#'                       edge.protocol = "minus",
#'                       truncation = 3,
#'                       coverage.grid = cover)
#' survey <- generate.transects(design)
#' plot(region, survey, covered.area = TRUE)
#' design <- run.coverage(design, reps = 3)
#' plot(design)
#' design
#'
make.design <- function(region = make.region(), transect.type = "line", design = "systematic", samplers = numeric(0), line.length = numeric(0), seg.length = numeric(0), effort.allocation = numeric(0), design.angle =  0, spacing = numeric(0), edge.protocol = "minus", seg.threshold = numeric(0), bounding.shape = "rectangle", truncation = 1, coverage.grid = NULL){
  #Check if a coverage grid has been passed in - if not create one
  if(class(coverage.grid) != "Coverage.Grid"){
    if(!is.null(coverage.grid)){
      warning("The coverage.grid argument must be of class Coverage.Grid.")
    }
    #by default makes a grid with approx 1000 points
    coverage.grid <- new("Coverage.Grid", grid = list(), spacing = numeric(0))
  }
  #Check design arguments
  if(transect.type %in% c("Line", "line", "Line Transect", "line transect")){
    if(length(samplers) == 0 && length(line.length) == 0 && length(spacing) == 0){
      samplers = 20
    }
    #Create line transect object
    if(any(design == "segmentedgrid")){
      if(length(seg.threshold) == 0){
        seg.threshold <- 50
      }
      design <- new(Class="Segment.Transect.Design", region, truncation, design, line.length, seg.length, effort.allocation, spacing, samplers, design.angle, edge.protocol, seg.threshold, bounding.shape, coverage.grid)
    }else{
      design <- new(Class="Line.Transect.Design", region, truncation, design, line.length, effort.allocation, spacing, samplers, design.angle, edge.protocol, bounding.shape, coverage.grid)
    }
  }else if(transect.type %in% c("Point", "point", "Point Transect", "point transect")){
    if(all(design == "random")){
      if(length(samplers) == 0){
        samplers = 20
        spacing = numeric(0)
      }
    }else if(all(design == "systematic")){
      if(length(samplers) == 0 && length(spacing) == 0){
        samplers = 20
      }
    }else if(all(design %in% c("random", "systematic"))){
      if(length(samplers) == 0){
        samplers = 20
      }
    }else{
      stop("Point transect design not recognised, please choose from 'random' or 'systematic", call. = FALSE)
    }
    #Check values
    if(any(any(na.omit(samplers) < 0) || any(na.omit(spacing) < 0))){
      stop("Negative values were used to specify effort.", call. = FALSE)
    }
    #Create point transect object
    design <- new(Class="Point.Transect.Design", region, truncation, design, spacing, samplers, effort.allocation, design.angle, edge.protocol, coverage.grid)
  }
  #Check design object - make sure correct number of elements per slot etc
  if(class(design) == "Point.Transect.Design"){
    test <- check.point.design(design)
  }else{
    test <- check.line.design(design)
  }
  if(class(test) == "character"){
    stop(test, call. = FALSE)
  }else{
    design <- test
  }
  return(design)
}


#' @title Creates a Coverage.Grid object
#' @description This creates an instance of the Coverage.Grid class.
#' @param region the region name
#' @param spacing spacing to be used to create the coverage grid. If
#' spacing is specified then any value supplied for n.grid.points will
#' be ignored.
#' @param n.grid.points the desired number of grid points (note that
#' the exact number generated may differ slightly depending on the
#' shape of the study region).
#' @return object of class Coverage.Grid
#' @export
#' @author Laura Marshall
#' @examples
#' \donttest{
#' # This example will take a bit of time to generate
#' # A coverage grid in a rectangular region of 2000 x 500
#' region <- make.region()
#' cover <- make.coverage(region, spacing = 50)
#' plot(region, cover)
#' # Create coverage grid by approx number of grid points
#' cover <- make.coverage(region, n.grid.points = 1000)
#' plot(region, cover)
#' }
#'
#' # Fast running example for CRAN testing purposes
#' # This spacing is too sparse to assess coverage in a real example
#' region <- make.region()
#' cover <- make.coverage(region, spacing = 250)
#' plot(region, cover)
make.coverage <- function(region = make.region(),
                      spacing = numeric(0),
                      n.grid.points = 1000){
  #if neither, spacing, no.grid.points, or grid is provided make an empty coverage - used when this is called from within this function to generate a design to create the coverage grid.
  if(length(spacing) == 0 && length(n.grid.points) == 0){
    return(new("Coverage.Grid", list(), numeric(0)))
  }
  if(length(spacing) > 0 && length(n.grid.points) > 0){
    if(n.grid.points != 1000){
      warning("Both spacing and n.grid.points specified, n.grid.point will be disregarded.", call. = FALSE, immediate. = TRUE)
    }
    n.grid.points <- numeric(0)
  }
  #find union of region - coverage.grid is over the whole
  region.union <- sf::st_union(region@region)

  #op <- options(warn = -1)
  #on.exit(options(op))
  suppressMessages(region.union <- make.region(shape = region.union))
  #options(op)
  #Create a systematic point design with empty coverage grid
  cover.grid.design <- make.design(region = region.union,
                                   transect.type = "point",
                                   design = "systematic",
                                   samplers = n.grid.points,
                                   spacing = spacing)
  #Now generate a set of transects from the design
  grid <- generate.transects(cover.grid.design, for.coverage = TRUE)
  #Now extract the samplers and make the grid
  return(new("Coverage.Grid", grid@samplers, grid@spacing))
}

