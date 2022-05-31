library(dssd)
library(testthat)
library(sf)

context("Design parameter checks")

test_that("Only the implemented design parameters are stored", {
  # Some design arguments take priority over others, only the design
  # parameters used to generate the surveys should be stored.

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Single stratum examples

  region <- make.region()

  # SINGLE STRATA POINT TRANSECT DESIGN
  # --------------------------------

  # Spacing should take precedent over samplers
  expect_warning(point.design <- make.design(region,
                              transect.type = "point",
                              design = "systematic",
                              samplers = 20,
                              spacing = 220,
                              seg.threshold = 50,
                              design.angle = 45),
                 "Both spacing and samplers have been supplied for stratum 1, samplers argument will be ignored.")
  expect_true(inherits(point.design, "Point.Transect.Design"))
  expect_true(length(point.design@samplers) == 0)

  expect_warning(point.design <- make.design(region,
                                             transect.type = "point",
                                             design = "systematic",
                                             samplers = 20,
                                             effort.allocation = 1,
                                             seg.threshold = 50,
                                             design.angle = 45),
                 "Effort allocation argument redundant as there is only one stratum, it will be ignored.")
  expect_equal(point.design@effort.allocation, numeric(0))

  expect_warning(point.design <- make.design(region,
                                             transect.type = "line",
                                             design = "systematic",
                                             spacing = 220,
                                             effort.allocation = 1,
                                             seg.threshold = 50,
                                             design.angle = 45),
                 "Effort allocation argument redundant as there is only one stratum, it will be ignored.")

  # Expect warning
  expect_warning(point.design <- make.design(region,
                              transect.type = "point",
                              design = "random",
                              samplers = 20,
                              spacing = 220,
                              seg.threshold = 50,
                              design.angle = 45),
                 "Spacing is not a valid effort argument for the random design in stratum 1, it will be ignored.")

  # Generate some transects
  transects <- generate.transects(point.design)
  expect_equal(length(transects@spacing), 0)
  expect_equal(transects@samp.count, 20)

  # No spacing so use samplers
  point.design <- make.design(region,
                              transect.type = "point",
                              design = "systematic",
                              samplers = 20,
                              seg.threshold = 50,
                              design.angle = 45)

  expect_true(inherits(point.design, "Point.Transect.Design"))
  expect_true(point.design@samplers == 20)
  expect_true(length(point.design@spacing) == 0)

  # Generate some transects
  transects <- generate.transects(point.design)
  expect_equal(transects@spacing, 223.6068)

  expect_warning(point.design <- make.design(region,
                              transect.type = "point",
                              design = "systematic",
                              samplers = 20,
                              line.length = 100,
                              seg.threshold = 50,
                              design.angle = 45),
                 "Argument line.length not applicable to point transect designs.")

  # SINGLE STRATA LINE TRANSECT DESIGN
  # -------------------------------

  # Spacing should take precedent over samplers
  expect_warning(line.design <- make.design(region,
                             transect.type = "line",
                             design = "systematic",
                             samplers = 20,
                             spacing = 100,
                             line.length = 4500,
                             seg.threshold = 50,
                             design.angle = 45),
                 "Spacing, samplers and line.length have been supplied for stratum 1, samplers and line.length arguments will be ignored.")

  expect_true(inherits(line.design, "Line.Transect.Design"))
  expect_true(length(line.design@samplers) == 0)
  expect_true(length(line.design@line.length) == 0)

  # Generate some transects
  transects <- generate.transects(line.design)

  expect_equal(transects@spacing, 100)

  # No spacing so use line.length (not samplers)
  expect_warning(line.design <- make.design(region,
                             transect.type = "line",
                             design = "systematic",
                             samplers = 20,
                             line.length = 4500,
                             seg.threshold = 50,
                             design.angle = 45),
                 "Both sampers and line.length have been supplied for stratum 1, samplers argument will be ignored.")

  expect_true(inherits(line.design, "Line.Transect.Design"))
  expect_true(line.design@line.length == 4500)
  expect_true(length(line.design@samplers) == 0)
  expect_true(length(line.design@spacing) == 0)

  # Generate some transects
  transects <- generate.transects(line.design)

  # Spacing is not a valid choice for a random design
  expect_warning(line.design <- make.design(region,
                                            transect.type = "line",
                                            design = "random",
                                            samplers = 20,
                                            spacing = 100,
                                            seg.threshold = 50,
                                            design.angle = 45),
                 "Spacing is not a valid effort argument for the random design in stratum 1, it will be ignored.")

  # Line.length should be used and samplers ignored
  expect_warning(line.design <- make.design(region,
                                            transect.type = "line",
                                            design = "random",
                                            samplers = 20,
                                            line.length = 100,
                                            seg.threshold = 50,
                                            design.angle = 45),
                 "Both sampers and line.length have been supplied for stratum 1, samplers argument will be ignored.")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Multi-strata examples

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

  # TEST TRUNCATION VALUE INPUT
  expect_warning(line.design <- make.design(region,
                                            transect.type = "line",
                                            design = "segmentedgrid",
                                            samplers = 20,
                                            seg.length = 1,
                                            seg.threshold = 50,
                                            design.angle = 45,
                                            truncation = c(1,1.5,1)),
                 "You have supplied more than one truncation value. Currently the same truncation value must be applied across the entire study region. Using only the first value supplied.")
  expect_equal(line.design@truncation, 1)

  expect_error(line.design <- make.design(region,
                                          transect.type = "line",
                                          design = "segmentedgrid",
                                          samplers = 20,
                                          seg.length = 1,
                                          seg.threshold = 50,
                                          design.angle = 45,
                                          truncation = "5%"),
               "Truncation value must be numeric.")

  expect_error(line.design <- make.design(region,
                                          transect.type = "line",
                                          design = "segmentedgrid",
                                          samplers = 20,
                                          seg.length = 1,
                                          seg.threshold = 50,
                                          design.angle = 45,
                                          truncation = -10),
               "The truncation distance must be > 0.")

  expect_error(line.design <- make.design(region,
                                          transect.type = "line",
                                          design = "segmentedgrid",
                                          samplers = 20,
                                          seg.length = 1,
                                          seg.threshold = 50,
                                          design.angle = 45,
                                          truncation = 0),
               "The truncation distance must be > 0.")

  # CHECK EDGE PROTOCOL INPUT
  expect_error(design <- make.design(region, transect.type = "point",
                                     design = "systematic",
                                     samplers = 30,
                                     edge.protocol = c("minus", "plus"),
                                     design.angle = 45,
                                     truncation = 1),
               "Edge protocol argument has a different number of values than there are strata, please either supply a single global value or one value per stratum.")

  expect_error(design <- make.design(region, transect.type = "point",
                                       design = "systematic",
                                       samplers = 30,
                                       edge.protocol = c("Minus", "Plus", "Plus"),
                                       design.angle = 45,
                                       truncation = 1),
                 "Edge protocol values must either be 'plus' or 'minus'.")

  # CHECK DESIGN ANGLES

  expect_error(design <- make.design(region, transect.type = "point",
                                     design = "systematic",
                                     samplers = 30,
                                     edge.protocol = c("minus", "plus", "plus"),
                                     design.angle = c(45, 60),
                                     truncation = 1),
               "Design angle argument has a different number of values than there are strata, please supply a single global value or one value per stratum.")

  expect_error(design <- make.design(region, transect.type = "point",
                                     design = "systematic",
                                     samplers = 30,
                                     edge.protocol = c("minus", "plus", "plus"),
                                     design.angle = c(45, 60, NA),
                                     truncation = 1),
               "NA values supplied for design angle. Please supply values >= 0 and < 180 or the value -1 to indicate a random design angle selection.")

  expect_error(design <- make.design(region, transect.type = "point",
                                     design = "systematic",
                                     samplers = 30,
                                     edge.protocol = c("minus", "plus", "plus"),
                                     design.angle = c(45, 60, -45),
                                     truncation = 1),
               "Design angle values must be >=0 and < 180 or the value -1 to indicate a random design angle selection.")

  expect_error(design <- make.design(region, transect.type = "point",
                                     design = "systematic",
                                     samplers = 30,
                                     edge.protocol = c("minus", "plus", "plus"),
                                     design.angle = c(45, 60, 180),
                                     truncation = 1),
               "Design angle values must be >=0 and < 180 or the value -1 to indicate a random design angle selection.")

  expect_error(design <- make.design(region, transect.type = "point",
                                     design = "systematic",
                                     samplers = 30,
                                     edge.protocol = c("minus", "plus", "plus"),
                                     design.angle = "45 degrees",
                                     truncation = 1),
               "Design angle value\\(s\\) must be numeric.")

  # CHECK DESIGN

  expect_error(design <- make.design(region, transect.type = "point",
                                     design = c("systematic", "random"),
                                     samplers = 30,
                                     edge.protocol = c("minus", "plus", "plus"),
                                     design.angle = 45,
                                     truncation = 1),
               "Design description argument has a different number of values than there are strata, please supply a single global value or one value per stratum.")

  expect_error(design <- make.design(region, transect.type = "point",
                                     design = c("hello", "random", 555),
                                     samplers = 30,
                                     edge.protocol = c("minus", "plus", "plus"),
                                     design.angle = 45,
                                     truncation = 1),
               "Unrecognised designs: hello, 555")

  expect_error(design <- make.design(region, transect.type = "line",
                                     design = c("systematic", "random"),
                                     samplers = 30,
                                     edge.protocol = c("minus", "plus", "plus"),
                                     design.angle = 45,
                                     truncation = 1),
               "Design description argument has a different number of values than there are strata, please supply a single global value or one value per stratum.")


  expect_error(design <- make.design(region, transect.type = "line",
                                     design = c("hello", "random", 555),
                                     samplers = 30,
                                     edge.protocol = c("minus", "plus", "plus"),
                                     design.angle = 45,
                                     truncation = 1),
               "Unrecognised designs: hello, 555")

  design <- make.design(region, transect.type = "point",
                        design = c("systematic"),
                        samplers = 30,
                        edge.protocol = "minus",
                        design.angle = 45,
                        truncation = 1)
  expect_equal(design@design, rep("systematic", 3))
  expect_equal(design@edge.protocol, rep("minus", 3))

  design <- make.design(region, transect.type = "line",
                        design = c("eszigzagcom"),
                        samplers = 30,
                        edge.protocol = "minus",
                        design.angle = 45,
                        truncation = 1)
  expect_equal(design@design, rep("eszigzagcom", 3))
  expect_equal(design@edge.protocol, rep("minus", 3))

  samps <- generate.transects(design, region)
  expect_true(length(unique(samps@spacing)) == 1)

  # CHECK EFFORT DEFINITIONS

  expect_error(design <- make.design(region, transect.type = "line",
                                     design = c("eszigzagcom"),
                                     samplers = c(30, 20),
                                     edge.protocol = "minus",
                                     design.angle = 45,
                                     truncation = 1),
               "No sampler, spacing or line.length arguments have been specified for stratum 3.")

  expect_error(design <- make.design(region, transect.type = "line",
                                     design = c("eszigzagcom"),
                                     samplers = c(NA, 30, 20),
                                     edge.protocol = "minus",
                                     design.angle = 45,
                                     truncation = 1),
               "No sampler, spacing or line.length arguments have been specified for stratum 1.")

  expect_error(design <- make.design(region, transect.type = "line",
                                     design = c("eszigzagcom"),
                                     samplers = c(30, 20),
                                     line.length = 100,
                                     edge.protocol = "minus",
                                     design.angle = 45,
                                     truncation = 1),
               "You have supplied more than one effort descriptor \\(spacing, line.length, samplers\\). The number of values for each must be equal to the number of strata.")

  expect_error(design <- make.design(region, transect.type = "line",
                                     design = c("eszigzagcom"),
                                     spacing = c(30, 20),
                                     line.length = 100,
                                     edge.protocol = "minus",
                                     design.angle = 45,
                                     truncation = 1),
               "You have supplied more than one effort descriptor \\(spacing, line.length, samplers\\). The number of values for each must be equal to the number of strata.")

  expect_error(design <- make.design(region, transect.type = "point",
                                     design = c("random"),
                                     samplers = c(30, 20),
                                     spacing = c(10, 10, 15),
                                     edge.protocol = "minus",
                                     design.angle = 45,
                                     truncation = 1),
               "You have supplied more than one effort descriptor \\(spacing, samplers\\). The number of values for each must be equal to the number of strata.")

  expect_warning(design <- make.design(region, transect.type = "line",
                                       design = c("eszigzagcom"),
                                       spacing = c(30, 20, NA),
                                       line.length = c(100, NA, 5),
                                       edge.protocol = "minus",
                                       design.angle = 45,
                                       truncation = 1),
                 "Both spacing and line.length have been supplied for stratum  1, line.length argument will be ignored.")
  expect_equal(design@line.length, c(NA, NA, 5))

  expect_error(design <- make.design(region, transect.type = "line",
                                     design = c("random"),
                                     samplers = c(30, NA, NA),
                                     line.length = c(NA, NA, 1),
                                     edge.protocol = "minus",
                                     design.angle = 45,
                                     truncation = 1),
               "No sampler, spacing or line.length arguments have been specified for stratum 2.")

  expect_error(design <- make.design(region, transect.type = "point",
                                     design = c("systematic"),
                                     samplers = c(30, 20),
                                     spacing = c(NA, NA, 2),
                                     edge.protocol = "minus",
                                     design.angle = 45,
                                     truncation = 1),
               "You have supplied more than one effort descriptor \\(spacing, samplers\\). The number of values for each must be equal to the number of strata.")

  expect_warning(design <- make.design(region, transect.type = "point",
                                     design = c("systematic"),
                                     samplers = c(30, 20, 10),
                                     spacing = c(NA, NA, 2),
                                     edge.protocol = "minus",
                                     design.angle = 45,
                                     truncation = 1),
               "Both spacing and samplers have been supplied for stratum 3, samplers argument will be ignored.")
  expect_equal(design@samplers, c(30, 20, NA))

  # CHECK BOUNDING SHAPE

  expect_error(design <- make.design(region, transect.type = "line",
                                       design = c("eszigzagcom"),
                                       spacing = c(NA, 20, NA),
                                       line.length = c(100, NA, 5),
                                       edge.protocol = "minus",
                                       bounding.shape = c("rectangle", "convex.hull"),
                                       design.angle = 45,
                                       truncation = 1),
                 "Bounding shape argument has a different number of values than there are strata, please supply a single global value or one value per stratum.")

  expect_error(design <- make.design(region, transect.type = "line",
                                     design = c("eszigzagcom"),
                                     spacing = c(NA, 20, NA),
                                     line.length = c(100, NA, 5),
                                     edge.protocol = "minus",
                                     bounding.shape = c("rectangle", "convex.hull", "circle"),
                                     design.angle = 45,
                                     truncation = 1),
               "All bounding shape values must either be 'convex.hull' or 'rectangle'.")

  expect_error(design <- make.design(region, transect.type = "line",
                                     design = c("eszigzagcom", "random", "eszigzag"),
                                     spacing = c(NA, 20, NA),
                                     line.length = c(100, NA, 5),
                                     edge.protocol = "minus",
                                     bounding.shape = c("rectangle", "convex.hull", NA),
                                     design.angle = 45,
                                     truncation = 1),
               "NA values have been provided for bounding shape in strata where a zigzag design has been selected. Please supply valid values.")

  expect_warning(design <- make.design(region, transect.type = "line",
                                     design = c("eszigzagcom", "systematic", "eszigzag"),
                                     spacing = c(NA, 20, NA),
                                     line.length = c(100, NA, 5),
                                     edge.protocol = "minus",
                                     bounding.shape = c("rectangle", "convex.hull", "convex.hull"),
                                     design.angle = 45,
                                     truncation = 1),
               "Non NA values have been provided for bounding shape in strata where a zigzag design was NOT selected. These vaues will be ignored.")

  expect_equal(design@bounding.shape, c("rectangle", NA, "convex.hull"))

  # CHECK EFFORT ALLOCATION

  # Should be ignored when effort is explicit
  expect_warning(design <- make.design(region, transect.type = "point",
                                       design = "random",
                                       samplers = c(10,10,10),
                                       effort.allocation = c(0.1,0.1,0.8),
                                       edge.protocol = "minus",
                                       truncation = 1),
                 "Effort allocation argument redundant as you have supplied stratum specific effort values, it will be ignored.")
  expect_equal(design@effort.allocation, numeric(0))

  expect_warning(design <- make.design(region, transect.type = "line",
                                       design = "random",
                                       samplers = c(10,10,10),
                                       effort.allocation = c(0.1,0.1,0.8),
                                       edge.protocol = "minus",
                                       truncation = 1),
                 "Effort allocation argument redundant as you have supplied stratum specific effort values, it will be ignored.")
  expect_equal(design@effort.allocation, numeric(0))

  expect_warning(design <- make.design(region, transect.type = "line",
                                       design = "systematic",
                                       spacing = 10,
                                       effort.allocation = c(0.1,0.1,0.8),
                                       edge.protocol = "minus",
                                       truncation = 1),
                 "Effort allocation not applicable when effort is determined by spacing, it will be ignored.")
  expect_equal(design@effort.allocation, numeric(0))

  expect_warning(design <- make.design(region, transect.type = "point",
                                       design = "systematic",
                                       spacing = 10,
                                       effort.allocation = c(0.1,0.1,0.8),
                                       edge.protocol = "minus",
                                       truncation = 1),
                 "Effort allocation not applicable when effort is determined by spacing, it will be ignored.")
  expect_equal(design@effort.allocation, numeric(0))

  design1 <- make.design(region, transect.type = "point",
                        design = "random",
                        samplers = 30,
                        # This allocation is manually based on areas
                        # Strata areas:  145, 144, 134
                        effort.allocation = c(0.3428,0.3404,0.3168),
                        edge.protocol = "minus",
                        truncation = 1)

  design2 <- make.design(region, transect.type = "point",
                         design = "random",
                         samplers = 30,
                         edge.protocol = "minus",
                         truncation = 1)

  t1 <- generate.transects(design1)
  t2 <- generate.transects(design2)

  expect_equal(t1@samp.count, t2@samp.count)

  design <- make.design(region, transect.type = "point",
                         design = "systematic",
                         samplers = 30,
                         edge.protocol = "minus",
                         truncation = 1)

  transects <- generate.transects(design)
  # Spacings should be the same across strata
  expect_equal(transects@spacing[1], transects@spacing[2])
  expect_equal(transects@spacing[2], transects@spacing[3])

  design <- make.design(region, transect.type = "point",
                        design = "systematic",
                        samplers = 30,
                        effort.allocation = c(0.3428,0.3404,0.3168),
                        edge.protocol = "minus",
                        truncation = 1)

  transects <- generate.transects(design)
  expect_true(transects@spacing[1] != transects@spacing[2])
  expect_true(transects@spacing[2] != transects@spacing[3])

})
