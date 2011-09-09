\name{createPerformance}
\alias{cleanupCrash}
\alias{createPerformance}
\alias{finishPerformance}
\alias{performScore}
\title{Perform score statements with specified Csound orchestra}
\usage{
  createPerformance(orcfile, scorefile = NULL, i = NULL, f
  = NULL, flags = c("-odac", "-g"), finishPerformance =
  TRUE)

  performScore(csInstance, i = NULL, f = NULL)

  finishPerformance(csInstance)

  cleanupCrash()
}
\arguments{
  \item{orcfile}{The path of the orchestra file to be used
  for the performance}

  \item{scorefile}{The path of the score file, if any, to
  be used for the performance. The whole purpose of this
  function is to feed the score statements to Csound and
  bypass the need for score files, but this option is
  provided in any case.}

  \item{i}{A list of matrices containing the
  \preformatted{i} statements, one for each instrument
  included. Each \preformatted{i} statement makes an
  instrument active at a specific time and for a certain
  duration. Its p-fields are interpreted in the order of
  the columns of the \code{data.frame}.}

  \item{f}{A list of numeric vectors; these create the
  function tables Csound uses for oscillators and various
  other uses.}

  \item{flags}{A character vector of extra command-line
  flags to pass to Csound upon compilation of the
  orchestra. See
  \href{http://www.csounds.com/manual/html/CommandFlagsCategory.html}{The
  Csound Manual's page on the Csound command-line
  options}.}

  \item{finishPerformance}{Should the performance be closed
  after completing the score? If \preformatted{TRUE}, the
  default, cleans up and closes Csound. If
  \preformatted{FALSE}, returns a pointer to a Csound
  instance that can be used to continue the performance or
  eventually close it.}

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

  If left open, one can use the object returned by
  \code{createPerformance()} to control Csound and
  subsequently add more control data which Csound then
  performs immediately with \code{performScore()}. You can
  then finish up the performance and close Csound with
  \code{finishPerformance()}.

  If you encounter an error and cannot run
  \code{finishPerformance()} run \code{cleanupCrash} before
  doing anything else. Otherwise you may cause a segfault
  and crash R.
}

