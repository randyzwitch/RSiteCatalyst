#' SCSetDebug
#'
#' Sets a debug flag that saves output and prints out more information
#'
#' @param debug.mode set to TRUE to enable debug mode (defaults to FALSE)
#'
#' @export

SCSetDebug <- function(debug.mode = TRUE){
  SC.Debug <<- debug.mode
  if(SC.Debug) {
    print(paste("DEBUG MODE: ",debug.mode))
  }
}