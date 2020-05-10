#' Parse a segment definition from Segments_Get 
#' 
#' For a call to Segments_Get where 'definition' is requested, attempt to parse (flatten) the return
#'
#' @importFrom jsonlite unbox toJSON
#' 
#' @inherit flatten_nested_defn params return details
#' 
#' @param verbose Should the function communicate the number of iterations
#' required to parse each definition? Defaults to \code{FALSE}
#'
#' @export
#'
#' @examples
#' # TBD
parse_segment_defn <- function(x, verbose = FALSE) {
  
  if(! is.data.frame(x)) {
    stop("Input of class data.frame required")
  }
  
  if(! "definition" %in% names(x)) {
    return(x)
  }
  
  if(! all(c("id", "name") %in% names(x))) {
    stop("Required names of 'id' and 'name' not all present")
  }
  
  splitted <- split_segment_ret(x)
  if(!verbose) {
    flat <- suppressMessages(
      Map(flatten_nested_defn, splitted)
    )
  } else {
    flat <- Map(flatten_nested_defn, splitted)
  }
}