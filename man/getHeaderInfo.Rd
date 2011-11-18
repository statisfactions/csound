\name{getHeaderInfo}
\alias{getHeaderInfo}
\title{Get the header specifications of a Csound instance}
\usage{
  getHeaderInfo(csInstance)
}
\arguments{
  \item{csInstance}{An instance of Csound, created by
  \code{\link{.csoundCreate}}.}
}
\value{
  \code{getHeaderInfo()} returns a list with the following
  elements:

  \code{$sample rate}, abbreviated as \code{sr} in the
  orchestra header. is the number of data points
  (\sQuote{samples}) per second used to represent a sound.
  The default is 44100 Hz, which is CD-quality.

  \code{$ksmps} is the number of samples in a control
  period, which must be an integer. Csound allows one to
  control an audio signal at a rate below that of the audio
  rate (i.e. the \dfn{control rate} or \code{$kr}), useful
  for things such as creating an envelope for sounds; using
  a lower-than-audio rate uses less processing power for
  what is often the same audio effect. Higher values of
  \code{ksmps} indicate a slower control rate; \code{ksmps
  = 1} means audio rate. A typical \code{ksmps} is 10.

  \code{$x0dbfs} is the maximum amplitude value--specified
  amplitude values are scaled between 0 and this number.
  \code{0dbfs} is short for Zero Decibels at Full-Scale
  Amplitude. A common value is 1; if not specified in the
  file, the default is 32768.

  \code{$nchnls} is the number of output channels
  specified; 1 is mono output, 2 stereo, etc.

  If the orchestra has been compiled, these should match
  the header in the orchestra; if no orchestra has been
  compiled, \code{getHeaderInfo()} simply returns the
  defaults.
}
\description{
  The header of the csound orchestra declares various
  important variables including the sample rate, the number
  of audio samples per control period, the number of output
  channels, and the maximum amplitude value. This function
  queries a Csound instance and returns the information in
  a list.
}

