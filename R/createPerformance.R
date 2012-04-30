##' Perform score statements with specified Csound orchestra
##'
##' These functions provide a safe, streamlined, and R-friendly
##' interface to Csound's API, allowing users to pass lists of
##' matrices and vectors to use as parameter data for controlling
##' Csound instruments.
##'
##' \code{createPerformance()} initializes Csound and compiles the
##' orchestra and allows the rendering of matrices and vectors as
##' parameter data; it can either close its instance of Csound or
##' return it, depending on the value of the argument
##' \code{finishPerformance}.
##'
##' If \code{finishPerformance = FALSE}, one can use the object
##' returned by \code{createPerformance()} to control Csound and
##' subsequently add more control data which Csound then performs
##' immediately with \code{performScoreRealTime()}. You can then
##' finish up the performance and close Csound with
##' \code{finishPerformance()}.
##'
##' If you encounter an error and cannot run
##' \code{finishPerformance()} run \code{cleanupCrash} before doing
##' anything else. Otherwise you may cause a segfault and crash R.
##' 
##' @export
##' @rdname createPerformance
## Parameter list exported to sonopts.R on Friday, December 16, 2011
##' @param i A list of \code{matrix} objects. Each \code{matrix} is
##' the instructions for a single instrument. Each row of the
##' \code{matrix} is an \code{i} statement, which instructs Csound to
##' make an instrument active at a specific time and for a certain
##' duration, and with certain parameters (p-fields). These p-fields are
##' interpreted in the order of the columns of the \code{matrix}.
##' @param f A list of numeric vectors; these create the function
##' tables Csound uses for oscillators and various other uses.
##' @param orcfile The path of the orchestra file to be used for the
##' performance. If this equals \code{"built-in.orc"}, the default,
##' the orchestra included with this package will be used (see
##' \code{\link{scoreMatrices}} for more details of using the built-in
##' instruments.)
##' @param scorefile The path of the score file, if any, to be used
##' for the performance. The whole purpose of this function is to feed
##' the score statements to Csound and bypass the need for score
##' files, but this option is provided in any case.
##' @param out String representing where to send output sound; the
##' default, \code{"dac"}, indicates to send it your computer's sound
##' output. If you want to render a file, enter the path to the (WAV)
##' file you want.
##' @param realTime Indicates whether the performance is to be
##' rendered in real time. If you are rendering to a file, you
##' probably want this as \code{FALSE}, since it can render a whole
##' lot faster than real-time to file.
##' @param finishPerformance Should the performance be closed after completing
##' the score? If \code{TRUE}, the default, cleans up and
##' closes Csound. If \code{FALSE}, returns a pointer to a
##' Csound instance that can be used to continue the performance or
##' eventually close it.
##' @param suppressDisplays Csound by default pops up with annoying
##' graphical widgets. This alloys you to suppress them (the default).
##' @param moreflags A character vector of extra command-line flags to
##' pass to Csound upon compilation of the orchestra. See
##' \href{http://www.csounds.com/manual/html/CommandFlagsCategory.html}{The
##' Csound Manual's page on the Csound command-line options}.
##' @param csInstance An instance of Csound that can be used to
##' continue or close the current performance.
##' 
##' @seealso \code{\link{scoreMatrices}()} for easy creation of the
##' \code{i} argument to \code{createPerformance}
##' 
##' @examples 
##' sndcheck <- scoreMatrices(5, 5)
##' sndcheck$FM[, "start"] <- 0:4
##' sndcheck$FM[, "dur"] <- 0.5
##' sndcheck$FM[, "amp"] <- 0.5
##' sndcheck$FM[, "pan"] <- (0:4)/4
##' sndcheck$FM[, c("attkp", "decayp")] <- 0.01
##' sndcheck$FM[, "cps"] <- (1:5)*110
##' sndcheck$FM[, "mod"] <- (1:5)/2
##' sndcheck$FM[, "indx"] <- 4:0
##' sndcheck$subtractive[, "start"] <- 0:4 + 0.5
##' sndcheck$subtractive[, "dur"] <- 0.1
##' sndcheck$subtractive[, "amp"] <- 0.05
##' sndcheck$subtractive[, "pan"] <- (4:0)/4
##' sndcheck$subtractive[, c("attkp", "decayp")] <- 0.01
##' sndcheck$subtractive[, "cntr"] <- (5:1)*500
##' sndcheck$subtractive[, "bw"] <- (5:1)*500
##' \dontrun{createPerformance(sndcheck)}
createPerformance <- function(i = NULL, f = NULL,
                              orcfile = "built-in.orc",
                              scorefile=NULL,
                              out = "dac",
                              realTime = FALSE,
                              finishPerformance = TRUE,
                              suppressDisplays = TRUE,
                              moreflags = NULL) {
  ## Preliminaries
  flags <- c(paste("-o", out, sep = ""), "-d"[suppressDisplays], moreflags)
  csinst <- .csoundCreate()
  assign(".lastInstance", csinst, pos=".GlobalEnv")
  .csoundPreCompile(csinst)

  ## Get path of built-in orchestra, if applicable
  if(orcfile == "built-in.orc") {
    orcfile <- system.file("orc/built-in.orc", package = "csound")
    if(orcfile == "") stop("Unable to find built-in orchestra file.")
  }
  ## Create score file for non-real-time performances
  if(!realTime & is.null(scorefile)) {
    scorefile <- writeCsoundScore(i, f)
    finishPerformance <- TRUE
  }
  
  ## Compile score & get ready for performance
  .csoundCompile(csinst, c(orcfile, scorefile, flags))

  if(realTime) {
      performScoreRealTime(csinst, i, f)
    } else .csoundPerform(csinst)


  if(finishPerformance) {
    invisible(finishPerformance(csinst))
  } else return(csinst)
}

##' @rdname createPerformance
##' @export
performScoreRealTime <- function(csInstance, i = NULL, f = NULL) {
  ## For each matrix in i,
  ## send the score events for each row and calculates the length 
  maxends <- sapply(i, function(x) {
    lapply(1:nrow(x), function(y) {
      .csoundScoreEvent(csInstance, "i", x[y,])
      return()
    })
    maxend <- max(rowSums(x[,2:3])) #p2 is start, p3 dur, so their sum
                                    #is the endtime of each event
    return(maxend)
  })

  ## Calculate the total num of control samples to be performed
  totalTime <- max(maxends)
  totalKsmps <- ceiling(totalTime*getHeaderInfo(csInstance)$kr)
  
  ## Similarly, send any f events
  lapply(f, function(x) {
    .csoundScoreEvent(csInstance, "f", x)
  })

  ## Perform!
  replicate(totalKsmps, .csoundPerformKsmps(csInstance))

  invisible(NULL)
}

##' @rdname createPerformance
##' @export
finishPerformance <- function(csInstance) {
  .csoundCleanup(csInstance)
  .csoundDestroy(csInstance)
  rm(.lastInstance, pos = ".GlobalEnv")
  return(NULL)
}

##' @export
##' @rdname createPerformance
cleanupCrash <- function() {
  if(exists(".lastInstance", ".GlobalEnv")) {
     finishPerformance(.lastInstance)
     rm(.lastInstance, pos = ".GlobalEnv")
   }
}
