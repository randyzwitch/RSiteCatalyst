#' BuildClassificationValueSegment
#'
#' Helper function to build a classification value segment for use with other reports
#' Multiple segments can be combined in a list.
#' Note that search can only be applied to a breakdown classification and not an element value.
#'
#' @param elements list of elements on which to vase the segment
#' @param search.keywords list of search keyword vectorsfor each element 
#' (this can use ^ to pin to start, and $ to pin to end, or both to specify exact match)
#' @param classification optional classification breakdown name for the element (defaults to the element name)
#' @param search.type how to combine the keywords list. This defaults to 'OR' if it is not specified.
#'
#' @return segment definition for use with Queue* helper functions
#'
#' @export

BuildClassificationValueSegment <- function(element,search.keywords,classification='',search.type='OR') {
  
  if(classification=='') {
    classification <- element
  }
  
  search <- list(type=jsonlite:::as.scalar(search.type),keywords=search.keywords)
  
  segment.definition <- list(id=jsonlite:::as.scalar(element),search=search)
  return(segment.definition) 
}