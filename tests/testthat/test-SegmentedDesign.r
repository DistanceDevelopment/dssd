library(dssd)
library(testthat)
library(sf)

context("Segmented Grid Checks")

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
  expect_equal(design.tm@seg.length, rep(200,2))
  expect_equal(survey.tm1@seg.length, rep(200,2))

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
  expect_equal(round(survey.tm@spacing,4), c(355.6886, 420.2832))
  expect_equal(survey.tm@seg.threshold, rep(50,2))

  #Test when no transects
  set.seed(551)
  design.tm <- make.design(region.tm,
                           transect.type = "line",
                           design = "segmentedgrid",
                           design.angle = 90,
                           spacing = 750,
                           seg.length = 200,
                           seg.threshold = 100,
                           truncation = 25)
  suppressWarnings(survey.tm <- generate.transects(design.tm))
  expect_equal(design.tm@seg.threshold, rep(100,2))
  expect_equal(survey.tm@samp.count, 17)

  #Test when no transects
  set.seed(321)
  design.tm <- make.design(region.tm,
                           transect.type = "line",
                           design = "segmentedgrid",
                           design.angle = 90,
                           spacing = c(2500,100),
                           seg.length = c(2000,100),
                           seg.threshold = c(100,0),
                           truncation = 25)
  suppressWarnings(survey.tm <- generate.transects(design.tm))
  expect_equal(survey.tm@samp.count, c(NA,42))

  #test basic shape to check trackline lengths
  region <- make.region()
  design <- make.design(region,
                        transect.type = "line",
                        design = "segmentedgrid",
                        design.angle = 0,
                        spacing = 250,
                        seg.length = 100,
                        seg.threshold = 100,
                        truncation = 25)
  set.seed(223)
  survey <- generate.transects(design)
  expect_equal(survey@trackline, 5350)
  expect_equal(survey@cyclictrackline, 7100)

  shapefile.name <- system.file("extdata", "TrackExample.shp", package = "dssd")
  region <- make.region(region.name = "study area",
                        shape = shapefile.name)
  design <- make.design(region = region,
                        transect.type = "line",
                        design = "segmentedgrid",
                        spacing = 10,
                        seg.length = 50,
                        design.angle = 90,
                        seg.threshold = 5,
                        edge.protocol = "minus",
                        truncation = .15)
  set.seed(112)
  transects <- generate.transects(design)

  #Check cyclic trackline length
  region <- make.region()
  design <- make.design(region = region,
                        transect.type = "line",
                        design = "segmentedgrid",
                        spacing = 200,
                        seg.length = 250,
                        design.angle = 90,
                        seg.threshold = 50,
                        edge.protocol = "minus",
                        truncation = .15)
  set.seed(126)
  transects <- generate.transects(design)
  #plot(region, transects)
  #trackline length = 2000 + 2000 + 200 = 4200
  expect_equal(transects@trackline, 4200)
  expect_equal(transects@cyclictrackline, 4400)

})
