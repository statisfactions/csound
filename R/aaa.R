##' @importFrom rdyncall dynfind

.onLoad <- function(libname, pkgname) {
  if(is.null(getOption("csoundlibrary")))
    csndlib <- dynfind(c("csound64",
                         "csound64.dll.5.2",
                         "csound.dll.5.2",
                         "lib_csnd.dylib",
                         "lib_csnd64.dylib"))
  if(is.null(csndlib)) {
    warning("No valid csound library found automatically. You must have a valid Csound library to use this package; if you know the path to your Csound shared library, you can specify it using setCsoundLibrary.")
  } else options(csoundlibrary=csndlib)
}


