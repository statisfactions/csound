##' Write a Csound score file, given lists of i and f statements
##'
##' Sometimes you want to write data to a Csound score file, either
##' for rendering later or for when you want to render to a sound
##' file. This is principally intended to be called by
##' \code{\link{createPerformance}}.
##'
##' @export
##' @param i A list of \code{matrix} objects. Each \code{matrix} is
##' the instructions for a single instrument. Each row of the
##' \code{matrix} is an \code{i} statement, which instructs Csound to
##' make an instrument active at a specific time and for a certain
##' duration, and with certain parameters (p-fields). These p-fields are
##' interpreted in the order of the columns of the \code{matrix}.
##' @param f A list of numeric vectors; these create the function
##' tables Csound uses for oscillators and various other uses.
##' @param outfile The name of the file to write to. If code{NULL},
##' the default, the score is written to a temporary file.
##'
##' @return The file name that the score was written to.
writeCsoundScore <- function(i, f, outfile = NULL) {
  if(is.null(outfile)){
    outfile <- paste(tempfile(),"sco",sep=".")
  }
  fscore <- sapply(f, function(x) paste("f", paste(x, collapse = " ")))
  write(fscore, outfile)
  iscore <- sapply(i, function(x) write.table(x, file = outfile,
                                              append = T,
                                              row.names = rep("i", nrow(x)),
                                              col.names = F, quote = F))
  write("e", outfile, append = T)
  outfile
}

