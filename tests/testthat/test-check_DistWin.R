library(dssd)
library(testthat)

context("Distance for Windows Checks")

test_that("Can deal with data from distance for windows", {

  #Setup region
  outer <- matrix(c(0,0,15,0,15,10,0,10,0,0),ncol=2, byrow=TRUE)
  hole1 <- matrix(c(2,2,2,3,3,3,3,2,2,2),ncol=2, byrow=TRUE)
  hole2 <- matrix(c(5,5,5,6,7,6,8,5.5,7,5,5,5),ncol=2, byrow=TRUE)
  pol1 <- sf::st_polygon(list(outer, hole1*1.5, hole2))
  pol2 <- sf::st_polygon(list(outer + 15, hole2*1.5 + 12))
  pol3 <- sf::st_polygon(list(outer + 30, hole2*2.5 + 20))
  sfc <- sf::st_sfc(pol1,pol2,pol3)
  strata.names.sf <- c("SW", "central", "NE")
  LinkID <- c(3,1,2)
  mp1 <- sf::st_sf(LinkID = LinkID,  geom = sfc)
  
  # Label strata based on LinkID
  region <- make.region(region.name = "study.area", 
                        strata.name = c("central", "NE", "SW"), 
                        shape = mp1)
  # Check the LinkID order
  expect_equal(region@region$LinkID, c(3,1,2))
  
  # Check the LinkID order - should still be the same
  region@region <- check.shape(sf.shape = region@region, dist.for.win = FALSE)
  expect_equal(region@region$LinkID, c(3,1,2))
  
  # Check the LinkID order - should now be corrected with a warning
  expect_warning(region@region <- check.shape(sf.shape = region@region, dist.for.win = TRUE), 
                 "The LinkID values were not in sequential order in the shapefile attribute table, dssd is reordering the strata to match that which Distance for Windows uses. This is a necessary step if you are running simulations from Distance for Windows. If you are running simulations directly in R and would like to switch this option off please set dist.for.win to FALSE in make.region.")
  expect_equal(region@region$LinkID, c(1,2,3))


})
