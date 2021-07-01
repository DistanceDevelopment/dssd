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

  # SYSTAMATIC POINT TRANSECT DESIGN
  # --------------------------------

  # Spacing should take precedent over samplers
  expect_warning(point.design <- make.design(region,
                              transect.type = "point",
                              design = "systematic",
                              samplers = 20,
                              spacing = 220,
                              seg.threshold = 50,
                              design.angle = 45),
                 "Both spacing and samplers have been supplied for systematic design, samplers argument will be ignored. Please only supply one of these arguments.")

  expect_true(class(point.design) == "Point.Transect.Design")
  expect_true(length(point.design@samplers) == 0)

  # Expect warning
  expect_warning(point.design <- make.design(region,
                              transect.type = "point",
                              design = "random",
                              samplers = 20,
                              spacing = 220,
                              seg.threshold = 50,
                              design.angle = 45),
                 "Spacing argument not applicable for random design, it will be ignored.")

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

  expect_true(class(point.design) == "Point.Transect.Design")
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

  # SYSTAMATIC LINE TRANSECT DESIGN
  # -------------------------------

  # Spacing should take precedent over samplers
  line.design <- make.design(region,
                             transect.type = "line",
                             design = "systematic",
                             samplers = 20,
                             spacing = 100,
                             line.length = 4500,
                             seg.threshold = 50,
                             design.angle = 45)

  expect_true(class(line.design) == "Line.Transect.Design")
  expect_true(length(line.design@samplers) == 0)
  expect_true(length(line.design@line.length) == 0)

  # Generate some transects
  transects <- generate.transects(line.design)

  expect_equal(transects@spacing, 100)

  # No spacing so use line.length (not samplers)
  line.design <- make.design(region,
                             transect.type = "line",
                             design = "systematic",
                             samplers = 20,
                             line.length = 4500,
                             seg.threshold = 50,
                             design.angle = 45)

  expect_true(class(line.design) == "Line.Transect.Design")
  expect_true(line.design@line.length == 4500)
  expect_true(length(line.design@samplers) == 0)
  expect_true(length(line.design@spacing) == 0)

  # Generate some transects
  transects <- generate.transects(line.design)
  expect_equal(transects@spacing, 223.6068)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Multi-strata examples



})
