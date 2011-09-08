\name{getCsoundLibrary}
\alias{csoundGetVersion}
\alias{getCsoundLibrary}
\alias{setCsoundLibrary}
\title{Get and set the Csound shared library for accessing Csound's
functionality.}
\usage{
  getCsoundLibrary()

  setCsoundLibrary(path)

  csoundGetVersion()
}
\arguments{
  \item{path}{A character string containing the path to the
  Csound shared library.}
}
\value{
  \code{getCsoundLibrary()} returns a pointer to the Csound
  library, with the path to the library passed as an
  attribute.

  \code{csoundGetVersion()} returns the version of Csound
  that is linked to.
}
\description{
  Functions to get and set the option "csoundlibrary",
  which is used by all the functions in the package to
  actually access the functionality of Csound.
}
\details{
  The \pkg{csound} package attempts to find the library
  file automatically on startup and warns if it fails. In
  this case, you need to find the path to Csound's shared
  library (often a \code{.so} or \code{.dll}, depending on
  the system. \code{setCsoundLibrary} will only actually
  set the option if it successfully locates and links to
  the library; if it fails to link it, it stops with an
  error.

  Hopefully these options won't need to be accessed by most
  users, but they are provided as a backup.
}

