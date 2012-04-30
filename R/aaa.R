##' @import rdyncall

.onLoad <- function(libname, pkgname) {
  if(is.null(getOption("csoundlibrary"))) {
    candidates <- c("csound64",
                    "csnd64",
                    "csound",
                    "csnd",
                    "lib_csnd64.dylib",
                    "lib_csnd.dylib",
                    "csound64.dll",
                    "csound.dll")
    candidatesfull <- c(candidates,
                    unlist(sapply(candidates, function(x)
                                  paste(x, 5, formatC(40:1, width = 2, flag = "0"), sep = "."))),
                    unlist(sapply(candidates, function(x)
                                  paste(x, 5, 9:1, sep = "."))))
    candidatesfiltered <- setdiff(candidatesfull, c("csnd", "csound", "csound.dll"))
    csndlib <- dynfind(candidatesfull)
    if(!is.null(csndlib) && (basename(attr(csndlib, "path")) %in% c("csnd", "csnd.dll"))) {
      path <- dirname(attr(csndlib, "path"))
      pathlist <- dir(path)
      matches <- pathlist %in% candidatesfiltered
      if(any(matches))
        csndlib <- .dynload(dir(path, full.names = T)[which(matches)[1]])
    }
    if(is.null(csndlib) && file.exists("/Library/Frameworks/CsoundLib64.framework/Versions/Current/lib_csound.dylib"))
      csndlib <- "/Library/Frameworks/CsoundLib64.framework/Versions/Current/lib_csound.dylib" ## in case we can make it work on OSX...
  }
  if(is.null(csndlib)) {
    packageStartupMessage("Note: No valid csound library found automatically. You must have a valid Csound library to use this package; if you know the path to your Csound shared library, you can specify it using setCsoundLibrary.")
  } else options(csoundlibrary=csndlib)
}

