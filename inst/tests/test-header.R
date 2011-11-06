require(csound)
require(testthat)

context("headerinfo")

## Set up
csinst <- .csoundCreate()
.csoundPreCompile(csinst)
tmporc <- paste(tempfile(), "orc", sep = ".")
tmpout <- tempfile()
samphead <- "sr = 16000
	ksmps = 128
	nchnls = 2
	0dbfs = 1
	instr 1
	endin"
write(samphead, tmporc)
.csoundCompile(csinst, c(tmporc, paste("-o", tmpout, sep ="")))

test_that("Header info is correctly read from csound", {
  res <- getHeaderInfo(csinst)
  expect_is(res, "list")
  expect_equal(length(res), 5)
  expect_equal(names(res), c("sr", "ksmps", "kr", "x0dbfs", "nchnls"))
  expect_equal(res$sr, 16000)
  expect_equal(res$ksmps, 128)
  expect_equal(res$x0dbfs, 1)
  expect_equal(res$nchnls, 2)
})

## Finish up
.csoundCleanup(csinst)
.csoundDestroy(csinst)
unlink(tmpout)
