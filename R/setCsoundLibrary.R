##' Get and set the Csound shared library for accessing Csound's
##' functionality.
##'
##' Functions to get and set the option \code{"csoundlibrary"}, which
##' is used by all the functions in the package to actually access the
##' functionality of Csound.
##'
##' The \pkg{csound} package attempts to find the library file
##' automatically on startup and warns if it fails. In this case, you
##' need to find the path to Csound's shared library (often a
##' \code{.so}, \code{.dll} or \code{.dylib}, depending on the
##' system). \code{setCsoundLibrary} will only actually set the option
##' if it successfully locates and links to the library; if it fails
##' to link it, it stops with an error.
##'
##' Hopefully these options won't need to be accessed by most users,
##' but they are provided as a backup.
##'
##' @return \code{getCsoundLibrary()} returns a pointer to the Csound
##' library, with the path to the library passed as an attribute.
##'
##' \code{.csoundGetVersion()} returns the version of Csound that is
##' linked to; this is useful as a quick check to make sure everything
##' has linked correctly.
##'
##' @seealso \code{\link{getHeaderInfo}}
##'
##' @rdname setCsoundLibrary
##' @export
getCsoundLibrary <- function() {
  csndlib <- getOption("csoundlibrary")
  if(is.null(csndlib))
    stop("Csound library is not set. See ?setCsoundLibrary.")

  return(csndlib)
         
}

##' @rdname setCsoundLibrary
##' @export
##' @param path A character string containing the path to the Csound
##' shared library.
setCsoundLibrary <- function(path) {
  csndlib <- .dynload(path)
  if(is.null(csndlib)) {
    stop("No valid shared object found at '", path, "'.")
  } else {
    cat("Shared object successfully found. Testing if it is Csound by\n",
        "checking version number...\n\n")
    versymbol <- .dynsym(csndlib, "csoundGetVersion")
    if(is.null(versymbol))
      stop("'", path, "' is detected as a shared library, but it\n",
           "is not the correct one for Csound--the attempt to access function\n",
           "csoundGetVersion() from the library failed.")
    ver <- .csoundGetVersion()

    options(csoundlibrary=csndlib)
    cat(paste("Successfully set option for Csound", ver, "\n"))
    
  }
}

##' @rdname setCsoundLibrary
##' 
##' @export
.csoundGetVersion <- function() {
  symptr <- .dynsym(getCsoundLibrary(), "csoundGetVersion")
  ## Version number appears in 1000s, so divide
 return(.dyncall(symptr, ")i")/1000)
}
