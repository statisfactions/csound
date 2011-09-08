\name{getCsoundError}
\alias{getCsoundError}
\title{Get Csound error message text}
\usage{
  getCsoundError(errornum)
}
\arguments{
  \item{errornum}{A negative integer representing a Csound
  error message}
}
\value{
  A character string with the text of the error; these were
  copied directly fro their definition in the header file
  for the Csound library.
}
\description{
  Several Csound API functions return an integer to
  indicate different kinds of errors. This function takes
  the integer and returns the character string with the
  text for the error.
}
\keyword{internal}

