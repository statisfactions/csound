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
##' If left open, one can use the object returned by
##' \code{createPerformance()} to control Csound and subsequently add
##' more control data which Csound then performs immediately with
##' \code{performScore()}. You can then finish up the performance and
##' close Csound with \code{finishPerformance()}.
##'
##' If you encounter an error and cannot run
##' \code{finishPerformance()} run \code{cleanupCrash} before doing
##' anything else. Otherwise you may cause a segfault and crash R.
##' 
##' @export
##' @rdname createPerformance
##' @param orcfile The path of the orchestra file to be used for the
##' performance
##' @param scorefile The path of the score file, if any, to be used
##' for the performance. The whole purpose of this function is to feed
##' the score statements to Csound and bypass the need for score
##' files, but this option is provided in any case.
##' @param i A list of matrices containing the \preformatted{i}
##' statements, one for each instrument included. Each
##' \preformatted{i} statement makes an instrument active at a
##' specific time and for a certain duration. Its p-fields are
##' interpreted in the order of the columns of the \code{data.frame}.
##' @param f A list of numeric vectors; these create the function
##' tables Csound uses for oscillators and various other uses.
##' @param flags A character vector of extra command-line flags to
##' pass to Csound upon compilation of the orchestra. See
##' \href{http://www.csounds.com/manual/html/CommandFlagsCategory.html}{The
##' Csound Manual's page on the Csound command-line options}.
##' @param finishPerformance Should the performance be closed after completing
##' the score? If \preformatted{TRUE}, the default, cleans up and
##' closes Csound. If \preformatted{FALSE}, returns a pointer to a
##' Csound instance that can be used to continue the performance or
##' eventually close it.
##' @param csInstance An instance of Csound that can be used to
##' continue or close the current performance.
createPerformance <- function(orcfile,
                              scorefile=NULL,
                              i = NULL,
                              f = NULL,
                              flags = c("-odac", "-g"),
                              finishPerformance = TRUE) {
  ## Preliminaries
  csinst <- csoundCreate()
  assign(".lastInstance", csinst, pos=".GlobalEnv")
  csoundPreCompile(csinst)

  ## Compile score & get ready for performance
  csoundCompile(csinst, c(orcfile, scorefile, flags))

  performScore(csinst, i, f)

  if(finishPerformance) {
    invisible(finishPerformance(csinst))
  } else return(csinst)
}

##' @rdname createPerformance
##' @export
performScore <- function(csInstance, i = NULL, f = NULL) {
  ## For each matrix in i,
  ## send the score events for each row and calculates the length 
  maxends <- sapply(i, function(x) {
    lapply(1:nrow(x), function(y) {
      csoundScoreEvent(csInstance, "i", x[y,])
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
    csoundScoreEvent(csInstance, "f", x)
  })

  ## Perform!
  replicate(totalKsmps, csoundPerformKsmps(csInstance))

  invisible(NULL)
}

##' @rdname createPerformance
##' @export
finishPerformance <- function(csInstance) {
  csoundCleanup(csInstance)
  csoundDestroy(csInstance)
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
  
