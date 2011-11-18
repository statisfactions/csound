##' @import rdyncall

.onLoad <- function(libname, pkgname) {
  if(is.null(getOption("csoundlibrary"))) {
    candidates <- c("csound64",
                    "csnd64",
                    "csound",
                    "csnd",
                    "lib_csnd64.dylib",
                    "csound64.dll",
                    "csound.dll")
    candidates <- c(candidates,
                    paste(candidates, 5, formatC(40:1, width = 2, flag = "0"), sep = "."),
                    paste(candidates, 5, 9:1, sep = "."))
    candidates <- c(candidates, paste(candidates, 9:1, sep = "."))
    csndlib <- dynfind(candidates)
  }
  if(is.null(csndlib)) {
    packageStartupMessage("Note: No valid csound library found automatically. You must have a valid Csound library to use this package; if you know the path to your Csound shared library, you can specify it using setCsoundLibrary.")
  } else options(csoundlibrary=csndlib)
}
