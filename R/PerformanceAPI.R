##' Low-level Csound API functions
##'
##' These functions provide a low-level interface to Csound
##' functionality, and are often called for their side effects on a
##' running instance of Csound. \code{\link{createPerformance}()} is
##' the recommended interface to this functionality, however.
##'
##' Order is important (see note!). These should be called in roughly
##' the following order.
##'
##' 1. \code{.csoundCreate()} is where it all starts, returning a
##' pointer to a running instance of Csound that the others can then manipulate.
##'
##' 2. \code{.csoundPreCompile()} performs a few steps to prepare
##' Csound for compiling the instruments
##'
##' 3. \code{.csoundCompile()} compiles the orchestra (and the score if
##' provided) and processes any command-line arguments.
##'
##' 4. Performance (\code{.csoundPerformKsmps()}) can be intermingled
##' with sending score events to the instance
##' (\code{.csoundScoreEvent()}.
##'
##' Alternatively, you can use \code{.csoundPerform()} which does not
##' return control until the score is complete (so is not suitible for
##' truly real-time performance).
##'
##' 5. After performance completes, use \code{.csoundCleanup()} to do
##' exactly that.
##'
##' 6. You need to explicitly destroy the Csound instance with
##' \code{.csoundDestroy()}.
##' 
##' @note Using these functions in the wrong order can cause a crash!
##' \code{\link{createPerformance}()} provides a safer interface and
##' is recommended for most users.
##' 
##' @rdname PerformanceAPI
##'
##' @return \code{.csoundCreate} returns a Csound instance that is an
##' argument to most of the other API functions.
##'
##' \code{.csoundPerform} returns an integer, which is positive number
##' if it reaches the end of the score, or zero if it is stopped by
##' another thread.
##' 
##' @param csInstance an instance of Csound created by
##' \code{.csoundCreate}
##'
##' @keywords internal
##' @export
.csoundCreate <- function() {
  symptr <- .dynsym(getCsoundLibrary(), "csoundCreate")
  return(.dyncall(symptr,")*<CSOUND>"))
}


##' @rdname PerformanceAPI
##' @param args A character vector of command-line arguments to pass
##' to csound. See
##' \href{http://www.csounds.com/manual/html/CommandFlagsCategory.html}{The
##' Csound Manual's page on the Csound command-line options}.
##' @export
.csoundCompile <- function(csInstance, args) {
  args <- c("csound", args) # 'csound' the first argument that
                            # .csoundCompile expects and is ignored.
  ptrargs <- strarrayptr(args)
  symptr <- .dynsym(getCsoundLibrary(), "csoundCompile")
  result <- .dyncall(symptr,"*<CSOUND>iZ)i",
                     csInstance, length(args), ptrargs)
    if(result != 0) {
    stop(getCsoundError(result))
  } else invisible(NULL)
}

##' @rdname PerformanceAPI
##' 
##' @export
.csoundCleanup <- function(csInstance) {
  symptr <- .dynsym(getCsoundLibrary(), "csoundCleanup")
  result <- .dyncall(symptr,"*<CSOUND>)i", csInstance)

  if(result != 0) {
    stop(getCsoundError(result))
  } else invisible(NULL)
}

##' @rdname PerformanceAPI
##' @export
##' 
.csoundDestroy <- function(csInstance) {
  symptr <- .dynsym(getCsoundLibrary(), "csoundDestroy")
  .dyncall(symptr,"*<CSOUND>)v", csInstance)
  invisible(NULL)
}



##' @export
##' @rdname PerformanceAPI
.csoundPerform <- function(csInstance) {
  symptr <- .dynsym(getCsoundLibrary(), "csoundPerform")
  result <- .dyncall(symptr, "*<CSOUND>)i", csInstance)
  if(result<0) stop("Error during performance")
  return(result)
}

##' @export
##' @rdname PerformanceAPI
.csoundPerformKsmps <- function(csInstance) {
  symptr <- .dynsym(getCsoundLibrary(), "csoundPerformKsmps")
  finished <- .dyncall(symptr, "*<CSOUND>)i", csInstance)
  return(finished) # logical indicating whether score is complete or
                   # not.
}

##' @export
##' @rdname PerformanceAPI
.csoundPreCompile <- function(csInstance) {
  symptr <- .dynsym(getCsoundLibrary(), "csoundPreCompile")
  .dyncall(symptr, "*<CSOUND>)v", csInstance)
  invisible(NULL)
}

##' @export  
##' @rdname PerformanceAPI
##' 
##' @param type The type of score event; only
##' \href{http://www.csounds.com/manual/html/a.html}{a},
##' \href{http://www.csounds.com/manual/html/i.html}{i},
##' \href{http://www.csounds.com/manual/html/q.html}{q},
##' \href{http://www.csounds.com/manual/html/f.html}{f}, and
##' \href{http://www.csounds.com/manual/html/e.html}{e} are supported
##' in the API:
##' 
##' \sQuote{a}: Advance score time by a specified amount.
##' \sQuote{i}: Makes an instrument active at a specific time and for a certain duration 
##' \sQuote{q}: Used to quiet an instrument 
##' \sQuote{f}: Causes a GEN subroutine to place values in a stored function table
##' \sQuote{e}: Marks the end of the last section of the score. (In
##' practice I've had trouble making \sQuote{e} statements work as
##' I've expected they would.)
##'
##' @param pfields The pfields (Parameter Fields) for the given score
##' event. See the Csound manual links for parameter \code{type} for
##' more information on the parameters that each of these objects take
.csoundScoreEvent <- function(csInstance,
                             type=c('a', 'i', 'q', 'f',  'e'),
                             pfields) {
  type <- match.arg(type)
  
  ## Deal with the fact that csound5 can be compiled with both 32-bit
  ## and 64-bit samples
  .csoundGetSizeOfMYFLT <- function() {
    ## Internal utility function to get number of bytes in sample
    ## (csound5 can be compiled for both 4 bytes or 8 bytes)
    symptr <- .dynsym(getCsoundLibrary(), "csoundGetSizeOfMYFLT")
    .dyncall(symptr, ")i")
  }
  if(.csoundGetSizeOfMYFLT() == 4) {
    pfields <- as.floatraw(pfields)
    funcsig <- "*<CSOUND>c*fj)i"
  } else funcsig <- "*<CSOUND>c*dj)i"
  
  ## Replace with ASCII character code
  ## (There may be a more logical way to do this)
  typeCharCode <- switch(type,
                         a = 97L,
                         i = 105L,
                         q = 113L,
                         f = 102L,
                         e = 101L)

  symptr <- .dynsym(getCsoundLibrary(), "csoundScoreEvent")
  result <- .dyncall(symptr, funcsig, csInstance,
                     typeCharCode, pfields, length(pfields))
                        
  if(result != 0) {
    stop(getCsoundError(result))
  } else invisible(NULL)
  
}

##' @export
##' @rdname PerformanceAPI
.csoundPerform <- function(csInstance) {
  symptr <- .dynsym(getCsoundLibrary(), "csoundPerform")
  result <- .dyncall(symptr, "*<CSOUND>)i", csInstance)
  if(result < 0) {
    stop(getCsoundError(result))
  } else invisible(NULL)
}
