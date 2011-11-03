require(testthat)
require(csound)

context("load")

test_that(".lastInstance global variable exists and is NA on load", {
  expect_that(.lastInstance, equals(NA))
})

test_that("csoundlibrary option and version is correct", {
  if(!is.null(getOption("csoundlibrary"))) {
    csndlib <- getOption("csoundlibrary") 
    expect_true(is.externalptr(csndlib))
    expect_true(attr(csndlib, "auto.unload"))
    ## Make sure version is reasonable
    expect_true(csoundGetVersion() > 5)
    expect_true(csoundGetVersion() < 6)
  }
})
    
