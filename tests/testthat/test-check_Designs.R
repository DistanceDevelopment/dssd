library(dssd)
library(testthat)

context("Check different designs")

test_that("Check designs can generate survey transects", {

  #Setup region
  outer <- matrix(c(0,0,15,0,15,10,0,10,0,0),ncol=2, byrow=TRUE)
  hole1 <- matrix(c(2,2,2,3,3,3,3,2,2,2),ncol=2, byrow=TRUE)
  hole2 <- matrix(c(5,5,5,6,7,6,8,5.5,7,5,5,5),ncol=2, byrow=TRUE)
  pol1 <- sf::st_polygon(list(outer, hole1*1.5, hole2))
  pol2 <- sf::st_polygon(list(outer + 15, hole2*1.5 + 12))
  pol3 <- sf::st_polygon(list(outer + 30, hole2*2.5 + 20))
  sfc <- sf::st_sfc(pol1,pol2,pol3)
  strata.names <- c("SW", "central", "NE")
  mp1 <- sf::st_sf(strata = strata.names, geom = sfc)
  region <- make.region(region.name = "study.area",
                        strata.name = strata.names,
                        shape = mp1)

  # POINT TRANSECT DESIGNS

  # Check random points
  design <- make.design(region, transect.type = "point",
                        design = "random",
                        samplers = 30,
                        edge.protocol = c("minus"),
                        design.angle = 45,
                        truncation = 1)

  transects <- generate.transects(design)
  expect_equal(nrow(transects@samplers), 30)

  design <- make.design(region, transect.type = "point",
                        design = "random",
                        samplers = 40,
                        edge.protocol = c("minus"),
                        design.angle = 45,
                        effort.allocation = c(0.25, 0.25, 0.5),
                        truncation = 1)

  transects <- generate.transects(design)
  expect_equal(transects@samp.count, c(10,10,20))

  # Check random / systematic combination
  design <- make.design(region, transect.type = "point",
                        design = c("random", "random", "systematic"),
                        samplers = 50,
                        edge.protocol = c("minus"),
                        design.angle = 45,
                        truncation = 1)

  transects <- generate.transects(design)
  expect_equal(transects@samp.count[1:2], c(17,17))

  # Check random / systematic combination
  design <- make.design(region, transect.type = "point",
                        design = "systematic",
                        samplers = 100,
                        edge.protocol = c("minus"),
                        design.angle = 0,
                        truncation = 1)

  transects <- generate.transects(design)
  expect_equal(transects@spacing, rep(2.059126,3))

  design <- make.design(region, transect.type = "point",
                        design = "systematic",
                        samplers = 100,
                        edge.protocol = c("plus"),
                        design.angle = -1,
                        truncation = 1)

  transects <- generate.transects(design)
  expect_equal(transects@spacing, rep(2.059126,3))

  # LINE TRANSECT DESIGNS

  design <- make.design(region, transect.type = "line",
                        design = "random",
                        samplers = 100,
                        edge.protocol = c("minus"),
                        design.angle = 0,
                        truncation = 1)

  transects <- generate.transects(design)
  expect_equal(nrow(transects@samplers), 100)

  design <- make.design(region, transect.type = "line",
                        design = c("random", "systematic", "systematic"),
                        samplers = 60,
                        edge.protocol = c("minus"),
                        design.angle = 90,
                        truncation = 1)

  transects <- generate.transects(design)

  design <- make.design(region, transect.type = "line",
                        design = c("systematic", "systematic", "systematic"),
                        samplers = 15,
                        edge.protocol = c("minus"),
                        design.angle = 0,
                        truncation = 1)

  transects <- generate.transects(design)
  expect_equal(transects@spacing, rep(3,3))

  design <- make.design(region, transect.type = "line",
                        design = c("eszigzag"),
                        samplers = 30,
                        edge.protocol = c("minus"),
                        design.angle = 90,
                        truncation = 1)

  transects <- generate.transects(design)
  expect_equal(transects@spacing, rep(1.5,3))

  design <- make.design(region, transect.type = "line",
                        design = c("eszigzagcom"),
                        samplers = 30,
                        edge.protocol = c("minus"),
                        design.angle = 90,
                        truncation = 1)

  transects <- generate.transects(design)
  expect_equal(transects@spacing, rep(3,3))

  design <- make.design(region, transect.type = "line",
                        design = c("eszigzagcom"),
                        line.length = 300,
                        effort.allocation = c(0.25,0.25,0.5),
                        edge.protocol = c("minus"),
                        design.angle = 90,
                        truncation = 1)
  transects <- generate.transects(design)
  area.effort.ratio <- region@area/transects@spacing
  expect_equal(area.effort.ratio[1], area.effort.ratio[2])
  expect_equal(transects@effort.allocation, c(0.25,0.25,0.5))

  design <- make.design(region, transect.type = "line",
                        design = c("eszigzagcom"),
                        line.length = 300,
                        effort.allocation = c(0.25,0.25,0.5),
                        edge.protocol = c("plus"),
                        design.angle = 90,
                        truncation = 1)
  transects <- generate.transects(design)
  expect_equal(transects@edge.protocol, rep("plus",3))

})
