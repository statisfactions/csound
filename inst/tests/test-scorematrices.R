require(csound)
require(testthat)

context("scorematrices")

test_that("scoreMatrices() correctly handles extreme input values", {
  expect_error(scoreMatrices(-1, 0))
  expect_error(scoreMatrices(0, -1))
  emptyscore <- scoreMatrices(0, 0)
  attr(emptyscore, "names") <- NULL
  expect_equal(emptyscore, list())
})

test_that("Sample output plays correctly", {
  sndcheck <- scoreMatrices(5, 5)
  sndcheck$FM[, "start"] <- 0:4
  sndcheck$FM[, "dur"] <- 0.5
  sndcheck$FM[, "amp"] <- 0.5
  sndcheck$FM[, "pan"] <- (0:4)/4
  sndcheck$FM[, c("attkp", "decayp")] <- 0.01
  sndcheck$FM[, "cps"] <- (1:5)*110
  sndcheck$FM[, "mod"] <- (1:5)/2
  sndcheck$FM[, "indx"] <- 4:0
  sndcheck$subtractive[, "start"] <- 0:4 + 0.5
  sndcheck$subtractive[, "dur"] <- 0.1
  sndcheck$subtractive[, "amp"] <- 0.05
  sndcheck$subtractive[, "pan"] <- (4:0)/4
  sndcheck$subtractive[, c("attkp", "decayp")] <- 0.01
  sndcheck$subtractive[, "cntr"] <- (5:1)*500
  sndcheck$subtractive[, "bw"] <- (5:1)*500
  createPerformance(sndcheck)
})
