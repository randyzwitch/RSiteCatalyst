#' @name BuildClassificationValueSegment
#'
#' @details Helper function to build a classification value segment for use with other reports.
#' 
#' Multiple segments can be combined in a list.
#' Note that search can only be applied to a breakdown classification and not an element value.
#'
#' @title Build a Classification Value Segment
#' 
#' @param element List of elements on which to vase the segment
#' @param search.keywords List of search keyword vectorsfor each element 
#' (this can use ^ to pin to start, and $ to pin to end, or both to specify exact match)
#' @param classification (optional) Classification breakdown name for the element (defaults to the element name)
#' @param search.type How to combine the keywords list. This defaults to 'OR' if it is not specified.
#'
#' @importFrom jsonlite toJSON unbox
#'
#' @return Segment definition for use with Queue* functions
#'
#' @examples
#' \dontrun{
#' vistor_segment <- BuildClassificationValueSegment(element,
#'                                                   search.keywords,
#'                                                   classification,
#'                                                   search.type)
#' }
#' @export

BuildClassificationValueSegment <- function(element,search.keywords,classification='',search.type='OR') {
  
  if(classification=='') {
    classification <- element
  }
  
  search <- list(type=unbox(search.type),keywords=search.keywords)
  
  segment.definition <- list(id=unbox(element),search=search)
  return(segment.definition) 
}