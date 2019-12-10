library(dssd)
library(testthat)

context("User Input Checks")

test_that("Can deal with various forms of user input", {

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


  #POINT TRANSECT DESIGNS
  #Test make.design for random points
  design <- make.design(region, transect.type = "point",
                        design = "random",
                        samplers = 30,
                        edge.protocol = c("minus"),
                        design.angle = 45,
                        truncation = 1)
  expect_equal(design@design, rep("random",3))
  expect_equal(design@edge.protocol, rep("minus",3))
  expect_equal(design@design.angle, rep(45,3))
  expect_equal(design@samplers, 30)

  design <- make.design(region, transect.type = "point",
                        design = "random",
                        truncation = 1)
  expect_equal(design@samplers, 20)

  #Test make.design for systematic points
  design <- make.design(region, transect.type = "point",
                        design = "systematic",
                        samplers = 30,
                        edge.protocol = c("plus"),
                        design.angle = 45,
                        truncation = 1)
  expect_equal(design@design, rep("systematic",3))
  expect_equal(design@edge.protocol, rep("plus",3))
  expect_equal(design@design.angle, rep(45,3))
  expect_equal(design@samplers, 30)

  design <- make.design(region, transect.type = "point",
                        design = "systematic",
                        spacing = 2,
                        edge.protocol = c("plus"),
                        design.angle = 45,
                        truncation = 1)
  expect_equal(design@design, rep("systematic",3))
  expect_equal(design@edge.protocol, rep("plus",3))
  expect_equal(design@design.angle, rep(45,3))
  expect_equal(design@spacing, rep(2,3))

  design <- make.design(region, transect.type = "point",
                        design = "systematic",
                        samplers = 30,
                        effort.allocation = c(0.25,0.25,0.5),
                        edge.protocol = c("minus"),
                        design.angle = 45,
                        truncation = 1)
  expect_equal(design@effort.allocation, c(0.25,0.25,0.5))

  expect_error(design <- make.design(region, transect.type = "point",
                                       design = "systematic",
                                       samplers = 30,
                                       effort.allocation = c(0.5,0.25,0.5),
                                       edge.protocol = c("minus"),
                                       design.angle = 45,
                                       truncation = 1),
               "Effort allocation should either be omitted or sum to 1")

  expect_error(design <- make.design(region, transect.type = "point",
                        design = "systematic",
                        samplers = c(30,10),
                        effort.allocation = c(0.25,0.25,0.5),
                        edge.protocol = c("minus"),
                        design.angle = 45,
                        truncation = 1),
               "Strata 3 has a systematic design but a non numeric argument has been supplied for the both the number of samplers and the spacing.")

  expect_error(design <- make.design(region, transect.type = "point",
                        design = "systematic",
                        samplers = 30,
                        effort.allocation = c(0.5,0.5),
                        edge.protocol = c("minus"),
                        design.angle = 45,
                        truncation = 1),
               "The length of the effort allocation argument should be equal to the number of strata.")

  expect_warning(design <- make.design(region, transect.type = "point",
                        design = "systematic",
                        samplers = 30,
                        edge.protocol = c("minus", "plus"),
                        design.angle = 45,
                        truncation = 1),
                 "Edge protocol argument has a different number of values than there are strata, only using the 1st value.")

  expect_warning(design <- make.design(region, transect.type = "point",
                                       design = c("systematic","random"),
                                       samplers = 30,
                                       design.angle = 45,
                                       truncation = 1),
                 "Design argument has a different number of values than there are strata, only using the 1st value.")


  expect_warning(design <- make.design(region, transect.type = "point",
                                       design = c("systematic"),
                                       samplers = 30,
                                       design.angle = c(45,0),
                                       truncation = 1),
                 "Design angle argument has a different number of values than there are strata, only using the 1st value.")

  #Check point transect works when design is systematic and only spacing provided
  design <- make.design(region, transect.type = "point",
                        design = c("systematic"),
                        #samplers = 30,
                        spacing = 2.5,
                        effort.allocation = c(0.25,0.25,0.5),
                        edge.protocol = c("minus"),
                        design.angle = -1,
                        truncation = 1)
  temp <- generate.transects(design)

  #Check point transect works when design is mix of random and systematic and design angle is -1
  design <- make.design(region, transect.type = "point",
                        design = c("systematic", "random", "systematic"),
                        samplers = c(NA,20,NA),
                        spacing = 2.5,
                        effort.allocation = c(0.25,0.25,0.5),
                        edge.protocol = c("minus"),
                        design.angle = c(-1,NA,-1),
                        truncation = 1)
  temp <- generate.transects(design)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #LINE TRANSECT DESIGNS
  #Test make.design for random line
  design <- make.design(region, transect.type = "line",
                        design = "random",
                        samplers = 30,
                        edge.protocol = c("minus"),
                        design.angle = 45,
                        truncation = 1)
  expect_equal(design@design, rep("random",3))
  expect_equal(design@edge.protocol, rep("minus",3))
  expect_equal(design@design.angle, rep(45,3))
  expect_equal(design@samplers, 30)

  design <- make.design(region, transect.type = "line",
                        design = "random",
                        truncation = 1)
  expect_equal(design@samplers, 20)

  design <- make.design(region, transect.type = "line",
                        design = "random",
                        line.length = 150,
                        truncation = 1)
  expect_equal(design@line.length, 150)

  #Test make.design for systematic line
  design <- make.design(region, transect.type = "line",
                        design = "systematic",
                        samplers = 30,
                        edge.protocol = c("plus"),
                        design.angle = 45,
                        truncation = 1)
  expect_equal(design@design, rep("systematic",3))
  expect_equal(design@edge.protocol, rep("plus",3))
  expect_equal(design@design.angle, rep(45,3))
  expect_equal(design@samplers, 30)

  design <- make.design(region, transect.type = "line",
                        design = "systematic",
                        spacing = 2,
                        edge.protocol = c("plus"),
                        design.angle = 45,
                        truncation = 1)
  expect_equal(design@design, rep("systematic",3))
  expect_equal(design@edge.protocol, rep("plus",3))
  expect_equal(design@design.angle, rep(45,3))
  expect_equal(design@spacing, rep(2,3))

  design <- make.design(region, transect.type = "line",
                        design = "systematic",
                        samplers = 30,
                        effort.allocation = c(0.25,0.25,0.5),
                        edge.protocol = c("minus"),
                        design.angle = 45,
                        truncation = 1)
  expect_equal(design@effort.allocation, c(0.25,0.25,0.5))

  expect_error(design <- make.design(region, transect.type = "line",
                        design = c("systematic", "random", "segmentedgrid"),
                        spacing = c(1,NA,1),
                        samplers = c(NA,30,NA),
                        edge.protocol = c("minus"),
                        design.angle = c(-1,-1,45),
                        truncation = 1), "Segment lengths must be provided for all segmented line transect designs.")


  design <- make.design(region, transect.type = "line",
                        design = c("systematic", "random", "segmentedgrid"),
                        spacing = c(2,NA,2),
                        samplers = c(NA,10,NA),
                        seg.length = c(NA,NA,1),
                        edge.protocol = c("minus"),
                        design.angle = -1,
                        truncation = 1)
  temp <- generate.transects(design)

  expect_error(design <- make.design(region, transect.type = "line",
                                     design = "systematic",
                                     samplers = 30,
                                     effort.allocation = c(0.5,0.25,0.5),
                                     edge.protocol = c("minus"),
                                     design.angle = 45,
                                     truncation = 1),
               "Effort allocation should either be omitted or sum to 1")

  expect_error(design <- make.design(region, transect.type = "line",
                                     design = "systematic",
                                     samplers = c(30,10),
                                     effort.allocation = c(0.25,0.25,0.5),
                                     edge.protocol = c("minus"),
                                     design.angle = 45,
                                     truncation = 1),
               "Strata 3 has a systematic design but a non numeric argument has been supplied for the number of samplers, the spacing and the line length.")

  expect_error(design <- make.design(region, transect.type = "line",
                                     design = "systematic",
                                     line.length = c(50,50),
                                     effort.allocation = c(0.25,0.25,0.5),
                                     edge.protocol = c("minus"),
                                     design.angle = 45,
                                     truncation = 1),
               "Strata 3 has a systematic design but a non numeric argument has been supplied for the number of samplers, the spacing and the line length.")

  expect_error(design <- make.design(region, transect.type = "line",
                                     design = "systematic",
                                     samplers = 30,
                                     effort.allocation = c(0.5,0.5),
                                     edge.protocol = c("minus"),
                                     design.angle = 45,
                                     truncation = 1),
               "The length of the effort allocation argument should be equal to the number of strata.")

  expect_warning(design <- make.design(region, transect.type = "line",
                                       design = "systematic",
                                       samplers = 30,
                                       edge.protocol = c("minus", "plus"),
                                       design.angle = 45,
                                       truncation = 1),
                 "Edge protocol argument has a different number of values than there are strata, only using the 1st value.")

  expect_warning(design <- make.design(region, transect.type = "line",
                                       design = c("systematic","random"),
                                       samplers = 30,
                                       design.angle = 45,
                                       truncation = 1),
                 "Design argument has a different number of values than there are strata, only using the 1st value.")

  expect_warning(design <- make.design(region, transect.type = "line",
                                       design = c("systematic"),
                                       samplers = 30,
                                       design.angle = c(45,0),
                                       truncation = 1),
                 "Design angle argument has a different number of values than there are strata, only using the 1st value.")


})
