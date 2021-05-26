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
  point.design <- make.design(region,
                              transect.type = "point",
                              design = "systematic",
                              samplers = 20,
                              spacing = 220,
                              seg.threshold = 50,
                              design.angle = 45)

  expect_true(class(point.design) == "Point.Transect.Design")
  expect_true(length(point.design@samplers) == 0)

  # Generate some transects
  transects <- generate.transects(point.design)

  expect_equal(transects@spacing, 220)

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
