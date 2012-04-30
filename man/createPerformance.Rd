\name{createPerformance}
\alias{cleanupCrash}
\alias{createPerformance}
\alias{finishPerformance}
\alias{performScoreRealTime}
\title{Perform score statements with specified Csound orchestra}
\usage{
  createPerformance(i = NULL, f = NULL,
    orcfile = "built-in.orc", scorefile = NULL,
    out = "dac", realTime = FALSE,
    finishPerformance = TRUE, suppressDisplays = TRUE,
    moreflags = NULL)

  performScoreRealTime(csInstance, i = NULL, f = NULL)

  finishPerformance(csInstance)

  cleanupCrash()
}
\arguments{
  \item{i}{A list of \code{matrix} objects. Each
  \code{matrix} is the instructions for a single
  instrument. Each row of the \code{matrix} is an \code{i}
  statement, which instructs Csound to make an instrument
  active at a specific time and for a certain duration, and
  with certain parameters (p-fields). These p-fields are
  interpreted in the order of the columns of the
  \code{matrix}.}

  \item{f}{A list of numeric vectors; these create the
  function tables Csound uses for oscillators and various
  other uses.}

  \item{orcfile}{The path of the orchestra file to be used
  for the performance. If this equals
  \code{"built-in.orc"}, the default, the orchestra
  included with this package will be used (see
  \code{\link{scoreMatrices}} for more details of using the
  built-in instruments.)}

  \item{scorefile}{The path of the score file, if any, to
  be used for the performance. The whole purpose of this
  function is to feed the score statements to Csound and
  bypass the need for score files, but this option is
  provided in any case.}

  \item{out}{String representing where to send output
  sound; the default, \code{"dac"}, indicates to send it
  your computer's sound output. If you want to render a
  file, enter the path to the (WAV) file you want.}

  \item{realTime}{Indicates whether the performance is to
  be rendered in real time. If you are rendering to a file,
  you probably want this as \code{FALSE}, since it can
  render a whole lot faster than real-time to file.}

  \item{finishPerformance}{Should the performance be closed
  after completing the score? If \code{TRUE}, the default,
  cleans up and closes Csound. If \code{FALSE}, returns a
  pointer to a Csound instance that can be used to continue
  the performance or eventually close it.}

  \item{suppressDisplays}{Csound by default pops up with
  annoying graphical widgets. This alloys you to suppress
  them (the default).}

  \item{moreflags}{A character vector of extra command-line
  flags to pass to Csound upon compilation of the
  orchestra. See
  \href{http://www.csounds.com/manual/html/CommandFlagsCategory.html}{The
  Csound Manual's page on the Csound command-line
  options}.}

  \item{csInstance}{An instance of Csound that can be used
  to continue or close the current performance.}
}
\description{
  These functions provide a safe, streamlined, and
  R-friendly interface to Csound's API, allowing users to
  pass lists of matrices and vectors to use as parameter
  data for controlling Csound instruments.
}
\details{
  \code{createPerformance()} initializes Csound and
  compiles the orchestra and allows the rendering of
  matrices and vectors as parameter data; it can either
  close its instance of Csound or return it, depending on
  the value of the argument \code{finishPerformance}.

  If \code{finishPerformance = FALSE}, one can use the
  object returned by \code{createPerformance()} to control
  Csound and subsequently add more control data which
  Csound then performs immediately with
  \code{performScoreRealTime()}. You can then finish up the
  performance and close Csound with
  \code{finishPerformance()}.

  If you encounter an error and cannot run
  \code{finishPerformance()} run \code{cleanupCrash} before
  doing anything else. Otherwise you may cause a segfault
  and crash R.
}
\examples{
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
\dontrun{createPerformance(sndcheck)}
}
\seealso{
  \code{\link{scoreMatrices}()} for easy creation of the
  \code{i} argument to \code{createPerformance}
}

