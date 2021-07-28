library(dssd)
library(testthat)
library(sf)

context("Tests which use seeds")

test_that("Seeded examples run", {

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
  if(Sys.info()['sysname'] != "Windows"){
    expect_equal(survey.tm@samp.count, 17)
  }

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
  if(Sys.info()['sysname'] != "Windows"){
    expect_equal(survey.tm@samp.count, c(NA,42))
  }

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
  if(Sys.info()['sysname'] != "Windows"){
    survey <- generate.transects(design)
    expect_equal(survey@trackline, 5350)
    expect_equal(survey@cyclictrackline, 7100)
  }
})
