library(dssd)
library(testthat)

context("Not Run Examples")

test_that("Examples not run by CRAN will run", {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Examples from make.design

  #Point transect example
  shapefile.name <- system.file("extdata", "TrackExample.shp", package = "dssd")
  region <- make.region(region.name = "study area",
                       shape = shapefile.name)
  # Generate coverage grid
  cover <- make.coverage(region,
                         n.grid.points = 500)
  # Define design
  design <- make.design(region = region,
                        transect.type = "point",
                        design = "random",
                        samplers = 25,
                        design.angle = 45,
                        edge.protocol = "minus",
                        truncation = 3,
                        coverage.grid = cover)

  # Generate a single survey instance
  survey <- generate.transects(design)
  plot(region, survey, covered.area = TRUE)

  # Warning! this will take some time to run
  design <- run.coverage(design, reps = 500)
  # Plot the coverage
  plot(design)
  # Display the design statistics
  design

  #Multi-strata line transect example
  shapefile.name <- system.file("extdata", "AreaRProjStrata.shp", package = "dssd")
  region <- make.region(region.name = "study area",
                       strata.name = c("North", "NW", "West Upper",
                                       "West Lower", "SW", "South"),
                       shape = shapefile.name)
  plot(region)
  # Make a coverage grid
  cover <- make.coverage(region,
                         n.grid.points = 100)
  plot(region, cover)
  # Define the design
  design <- make.design(region = region,
                        transect.type = "line",
                        design = c("systematic", "systematic",
                                   "eszigzag", "systematic",
                                   "systematic", "eszigzagcom"),
                        line.length = 5000*1000, #5000km x 1000m (projection in m)
                        design.angle = c(160, 135, 170, 135, 50, 60),
                        edge.protocol = "minus",
                        truncation = 3000,
                        coverage.grid = cover)

  # Create a single set of transects to check
  survey <- generate.transects(design)
  plot(region, survey, covered.area = TRUE)

  # Warning! this will quite a long time to run as it is a complex example.
  design <- run.coverage(design, reps = 50)
  # Plot the coverage
  plot(design)
  # Display the design statistics
  design

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Examples from make.coverage

  # This example will take a bit of time to generate
  # A coverage grid in a rectangular region of 2000 x 500
  region <- make.region()
  cover <- make.coverage(region, spacing = 50)
  plot(region, cover)
  # Create coverage grid by approx number of grid points
  cover <- make.coverage(region, n.grid.points = 1000)
  plot(region, cover)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

})
