require(csound)
require(testthat)

context("Score writes to files")

test_that("sample score matches writeCsoundScore result", {
  brutefile <- tempfile()
  score <- c("f 1 0 2048 10 1",
           "i 1 0 1",
           "i 1 1 1",
           "i 2 0 1 3",
           "i 2 1 1 4",
           "e")
  write(score, brutefile)
  cscore <- writeCsoundScore(f = list(c(1, 0, 2048, 10, 1)),
                             i = list(matrix(c(1, 1, 0, 1, 1, 1), ncol = 3),
                               matrix(c(2, 2, 0, 1, 1, 1, 3, 4), ncol = 4)))
  expect_true(all.equal(readLines(brutefile), readLines(cscore)))
})
