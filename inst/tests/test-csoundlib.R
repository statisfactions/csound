require(csound)
require(testthat)

context("csound library getting and setting")

test_that("blank 'csoundlibrary' option returns error from getCsoundLibrary()", {
  oldlib <- getOption("csoundlibrary")
  options(csoundlibrary=NULL)
  expect_error(getCsoundLibrary())
  options(csoundlibrary=oldlib)
})

test_that("setting library by path works", {
  if(!is.null(options("csoundlibrary"))) {
     path <- attr(getCsoundLibrary(), "path")
     setCsoundLibrary(path)
   }
})
