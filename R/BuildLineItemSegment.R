#' BuildLineItemSegment
#'
#' Helper function to build a line item segment for use with other reports
#' Multiple segments can be combined in a list
#'
#' @param elements list of elements on which to vase the segment
#' @param selected named list of element value vectors for each element 
#'
#' @return segment definition for use with Queue* helper functions
#'
#' @export

BuildLineItemSegment <- function(element,selected=list()) {
  segment.definition <- list(id=jsonlite:::as.scalar(element),selected=selected)
  return(segment.definition) 
}