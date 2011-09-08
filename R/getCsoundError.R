##' Get Csound error message text
##'
##' Several Csound API functions return an integer to indicate
##' different kinds of errors. This function takes the integer and
##' returns the character string with the text for the error.
##' 
##' @param errornum A negative integer representing a Csound error message
##' @return A character string with the text of the error; these were
##' copied directly fro their definition in the header file for the Csound library.
##' @export
##' @keywords internal
getCsoundError <- function(errornum) {
  switch((-errornum), 
         "Unspecified failure.",
         "Failed during initialization.",
         "Failed during performance.",
         "Failed to allocate requested memory.",
         "Termination requested by SIGINT or SIGTERM.")
}
