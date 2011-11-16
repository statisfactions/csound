##' Create blank score matrices for the built-in instruments
##'
##' This convenience function creates score matrices (for use in
##' \code{\link{createPerformance}} with the instruments included with
##' the \pkg{csound} package.
##'
##' There are currently two instruments included with the \pkg{csound}
##' package: a basic FM synthesis instrument and a subtractive
##' synthesis instrument.
##'
##' @param nFM The number of rows (each representing a note) desired
##' for the FM synthesis instrument.
##' @param nsubtractive The number of rows (each representing a note) desired
##' for the subtractive synthesis instrument.
##'
##' @return A list of two matrices: \code{$FM}, the score events for
##' the FM instrument, and \code{$subtractive}, the score events for
##' the subtractive synthesis instrument. These can be used as the
##' \code{i} argument to \code{\link{createPerformance}()}.
##'
##' @section Parameters for built-in instruments: The columns of the
##' returned matrices are parameters that Csound uses to render each
##' note. The two instruments share several parameters:
##'
##' \describe{
##' \item{instr}{The instrument number. \code{scoreMatrices()} fills
##' this in for you and users should not need to alter this.}
##' \item{start}{The starting time of the note (in seconds).}
##' \item{dur}{The duration of the note (in seconds).}
##' \item{amp}{The volume of the note, as a proportion between 0 and
##' 1, where 1 is the maximum volume. Note that a multiple notes that happen
##' at the same time could add up to more than one, causing distortion an
##' clipping.}
##' \item{pan}{The stereo placement of the note; 0 means entirely on the left
##' speaker, and 1 means entirely on the right.}
##' \item{attkp}{The proportion of the note's length devoted to the initial (linear)
##' attack.}
##' \item{decayp}{The proportion of the note's length devoted to the (linear) decay.}
##' }
##'
##' The FM instrument has these additional parameters:
##'
##' \describe{
##' \item{cps}{The frequency of the carrier tone (in Hertz).}
##' \item{mod}{The modulating frequency, given as a \emph{multiple}
##' of the carrier tone.}
##' \item{indx}{The index of modulation.}
##' }
##'
##' The subtractive synthesis instrument has these additional parameters:
##' \describe{
##' \item{cntr}{The central frequency of the band-pass filter (in Hertz).}
##' \item{bw}{The bandwidth of the band-pass filter (in Hertz).}
##' }
##'
##' @note When using the built-in instruments with
##' \code{\link{createPerformance}()},  these matrices can be used as
##' the \code{i} argument and no \code{f} argument is needed.
##' 
##' @export
scoreMatrices <- function(nFM = 0, nsubtractive = 0) {
  if(nFM < 0 | nsubtractive < 0)
    stop("n cannot be negative")
  
  out <- list(FM = switch(nFM > 0, matrix(nrow = nFM, ncol = 10)),
              subtractive = switch(nsubtractive > 0,
                matrix(nrow = nsubtractive, ncol = 9)))
  if(!is.null(out$FM)) {
    colnames(out$FM) <- c("instr", "start", "dur", "amp",
                           "pan", "attkp", "decayp", "cps",
                           "mod", "indx")
    out$FM[, "instr"] <- 1
  }
  if(!is.null(out$subtractive)) {
    colnames(out$subtractive) <- c("instr", "start", "dur",
                                   "amp", "pan", "attkp",
                                   "decayp", "cntr", "bw")
    out$subtractive[, "instr"] <- 2
  }

  out
}
                          
