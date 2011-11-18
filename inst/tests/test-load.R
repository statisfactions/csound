require(testthat)
require(csound)

context("load")

test_that("csoundlibrary option and version is correct", {
    csndlib <- getOption("csoundlibrary") 
    expect_true(is.externalptr(csndlib))
    expect_true(attr(csndlib, "auto.unload"))
    ## Make sure version is reasonable
    expect_true(.csoundGetVersion() > 5)
    expect_true(.csoundGetVersion() < 6)
})
    
