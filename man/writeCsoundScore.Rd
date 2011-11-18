\name{writeCsoundScore}
\alias{writeCsoundScore}
\title{Write a Csound score file, given lists of i and f statements}
\usage{
  writeCsoundScore(i, f, outfile = NULL)
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

  \item{outfile}{The name of the file to write to. If
  code{NULL}, the default, the score is written to a
  temporary file.}
}
\value{
  The file name that the score was written to.
}
\description{
  Sometimes you want to write data to a Csound score file,
  either for rendering later or for when you want to render
  to a sound file. This is principally intended to be
  called by \code{\link{createPerformance}}.
}

