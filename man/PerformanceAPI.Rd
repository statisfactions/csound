\name{.csoundCreate}
\alias{.csoundCleanup}
\alias{.csoundCompile}
\alias{.csoundCreate}
\alias{.csoundDestroy}
\alias{.csoundPerform}
\alias{.csoundPerformKsmps}
\alias{.csoundPreCompile}
\alias{.csoundScoreEvent}
\title{Low-level Csound API functions}
\usage{
  .csoundCreate()

  .csoundCompile(csInstance, args)

  .csoundCleanup(csInstance)

  .csoundDestroy(csInstance)

  .csoundPerform(csInstance)

  .csoundPerformKsmps(csInstance)

  .csoundPreCompile(csInstance)

  .csoundScoreEvent(csInstance,
    type = c("a", "i", "q", "f", "e"), pfields)

  .csoundPerform(csInstance)
}
\arguments{
  \item{csInstance}{an instance of Csound created by
  \code{.csoundCreate}}

  \item{args}{A character vector of command-line arguments
  to pass to csound. See
  \href{http://www.csounds.com/manual/html/CommandFlagsCategory.html}{The
  Csound Manual's page on the Csound command-line
  options}.}

  \item{type}{The type of score event; only
  \href{http://www.csounds.com/manual/html/a.html}{a},
  \href{http://www.csounds.com/manual/html/i.html}{i},
  \href{http://www.csounds.com/manual/html/q.html}{q},
  \href{http://www.csounds.com/manual/html/f.html}{f}, and
  \href{http://www.csounds.com/manual/html/e.html}{e} are
  supported in the API:

  \sQuote{a}: Advance score time by a specified amount.
  \sQuote{i}: Makes an instrument active at a specific time
  and for a certain duration \sQuote{q}: Used to quiet an
  instrument \sQuote{f}: Causes a GEN subroutine to place
  values in a stored function table \sQuote{e}: Marks the
  end of the last section of the score. (In practice I've
  had trouble making \sQuote{e} statements work as I've
  expected they would.)}

  \item{pfields}{The pfields (Parameter Fields) for the
  given score event. See the Csound manual links for
  parameter \code{type} for more information on the
  parameters that each of these objects take}
}
\value{
  \code{.csoundCreate} returns a Csound instance that is an
  argument to most of the other API functions.

  \code{.csoundPerform} returns an integer, which is
  positive number if it reaches the end of the score, or
  zero if it is stopped by another thread.
}
\description{
  These functions provide a low-level interface to Csound
  functionality, and are often called for their side
  effects on a running instance of Csound.
  \code{\link{createPerformance}()} is the recommended
  interface to this functionality, however.
}
\details{
  Order is important (see note!). These should be called in
  roughly the following order.

  1. \code{.csoundCreate()} is where it all starts,
  returning a pointer to a running instance of Csound that
  the others can then manipulate.

  2. \code{.csoundPreCompile()} performs a few steps to
  prepare Csound for compiling the instruments

  3. \code{.csoundCompile()} compiles the orchestra (and
  the score if provided) and processes any command-line
  arguments.

  4. Performance (\code{.csoundPerformKsmps()}) can be
  intermingled with sending score events to the instance
  (\code{.csoundScoreEvent()}.

  Alternatively, you can use \code{.csoundPerform()} which
  does not return control until the score is complete (so
  is not suitible for truly real-time performance).

  5. After performance completes, use
  \code{.csoundCleanup()} to do exactly that.

  6. You need to explicitly destroy the Csound instance
  with \code{.csoundDestroy()}.
}
\note{
  Using these functions in the wrong order can cause a
  crash! \code{\link{createPerformance}()} provides a safer
  interface and is recommended for most users.
}
\keyword{internal}

