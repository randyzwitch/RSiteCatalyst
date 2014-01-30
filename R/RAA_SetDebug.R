#' RAA_SetDebug
#'
#' Sets a debug flag that saves output and prints out more information
#'
#' @param debug.mode set to TRUE to enable debug mode (defaults to FALSE)
#'
#' @export

RAA_SetDebug <- function(debug.mode = TRUE){
  RAA.Debug <<- debug.mode
  if(RAA.Debug) {
    print(paste("DEBUG MODE: ",debug.mode))
  }
}