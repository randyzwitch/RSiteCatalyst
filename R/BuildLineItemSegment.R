#' BuildLineItemSegment
#'
#' Helper function to build a line item segment for use with other reports
#' Multiple segments can be combined in a list
#'
#' @param elements list of elements on which to vase the segment
#' @param selected named list of element value vectors for each element 
#'
#' @importFrom jsonlite toJSON unbox
#'
#' @return segment definition for use with Queue* helper functions
#'
#' @export
#' @keywords internal

BuildLineItemSegment <- function(element,selected=list()) {
  segment.definition <- list(id=unbox(element),selected=selected)
  return(segment.definition) 
}