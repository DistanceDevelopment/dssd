library(dssd)
library(testthat)
library(sf)

context("User Input Checks")

test_that("Can deal with various forms of user input", {
  #Load the unprojected shapefile
  shapefile.name <- system.file("extdata", "TentsmuirUnproj.shp", package = "dssd")
  sf.shape <- read_sf(shapefile.name)
  # Define a European Albers Equal Area projection
  proj4string <- "+proj=aea +lat_1=56 +lat_2=62 +lat_0=50 +lon_0=-3 +x_0=0
                +y_0=0 +ellps=intl +units=m"
  # Project the study area on to a flat plane
  projected.shape <- st_transform(sf.shape, crs = proj4string)

  # Create the survey region in dssd
  region.tm <- make.region(region.name = "Tentsmuir",
                           strata.name = c("Main Area", "Morton Lochs"),
                           shape = projected.shape)

  #Test single value for number of samplers
  design.tm <- make.design(region.tm,
                           transect.type = "line",
                           design = "segmentedgrid",
                           design.angle = c(-1,45),
                           samplers = 50,
                           seg.length = 200,
                           truncation = 25)
  survey.tm1 <- generate.transects(design.tm)

  expect_equal(round(survey.tm1@spacing,4), rep(453.6047,2))

  #Test single value for line length
  design.tm <- make.design(region.tm,
                           transect.type = "line",
                           design = "segmentedgrid",
                           design.angle = c(-1,45),
                           line.length = 10000,
                           seg.length = 200,
                           truncation = 25)
  survey.tm2 <- generate.transects(design.tm)
  expect_equal(round(survey.tm1@spacing,4), round(survey.tm2@spacing,4))

  #Test different effort (last 2 tests the effort corresponded!)
  design.tm <- make.design(region.tm,
                           transect.type = "line",
                           design = "segmentedgrid",
                           design.angle = c(-1,45),
                           line.length = 10000,
                           seg.length = 100,
                           truncation = 25)
  survey.tm <- generate.transects(design.tm)
  expect_equal(round(survey.tm@spacing,4), rep(338.2513,2))

  #Test allocating a specified number of samplers to each strata
  design.tm <- make.design(region.tm,
                           transect.type = "line",
                           design = "segmentedgrid",
                           design.angle = c(-1,45),
                           samplers = c(30, 10),
                           seg.length = 75,
                           truncation = 25)
  survey.tm <- generate.transects(design.tm)
  expect_equal(round(survey.tm@spacing,4), c(649.3001, 232.5606))

  #Test allocating a specified line length for each strata
  design.tm <- make.design(region.tm,
                           transect.type = "line",
                           design = "segmentedgrid",
                           design.angle = c(45,-1),
                           line.length = c(8500, 1500),
                           seg.length = 75,
                           truncation = 25)
  survey.tm <- generate.transects(design.tm)
  expect_equal(round(survey.tm@spacing,4), c(317.3158, 155.2939))

  #Test allocating a specified line length for each strata and different segment lengths for each strata.
  design.tm <- make.design(region.tm,
                           transect.type = "line",
                           design = "segmentedgrid",
                           design.angle = c(45,-1),
                           line.length = c(8000, 1000),
                           seg.length = c(200,50), #Not sure segment lengths are different
                           truncation = 25)
  survey.tm <- generate.transects(design.tm)
  expect_equal(round(survey.tm@spacing,4), c(502.2591, 165.7570))

  #Test allocating different segment lengths per strata with an overall number of samplers
  design.tm <- make.design(region.tm,
                           transect.type = "line",
                           design = "segmentedgrid",
                           design.angle = c(45,-1),
                           samplers = 75,
                           seg.length = c(200,50),
                           truncation = 25)
  survey.tm <- generate.transects(design.tm)
  expect_equal(round(survey.tm@spacing,4), c(372.9161, 165.7570))

  survey.tm
  plot(region.tm, survey.tm)

})
