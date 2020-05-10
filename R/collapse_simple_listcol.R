#' collapse_simple_listcol
#' 
#' Internal function - unnest a list-column into a (possibly bracket-quoted) comma-delimited atomic vector
#' 
#' @family internal
#'
#' @param x An unnested list of length 1 
#'
#' @return A collapsed atomic vector, with each element of \code{x[[1]]}
#' separated by \code{, } (comma\code{space}). If a comma is encountered
#' in any element of \code{x[[1]]}, ALL elements will additionally be 
#' encapsulated by square brackets as a more readable proxy for 
#' quotes. 
#'  
#' @details 
#' This does not check for any upsteam (e.g. data.frame) names, and should be used
#' directly on a column accessed by name (or double-subscripted via index/name); mainly
#' a helper function that does the heavy lifting for collapse_tags and collapse_compatibility
#' 
#' See \code{\link{collapse_simple_target}}, where this function does the heavy lifting.
#'
#' @examples
#' # TBD
collapse_simple_listcol <- function(x) {
  
  if(class(x) != "list") {
    stop("Expected an input of class 'list', but input is a ", class(x))
  }
  if(length(x) > 1L) {
    stop("Expected an input of length 1, but length is ", length(x))
  }
  
  x_tag <- x[[1]]
  
  # handle empty return
  if(identical(list(), x_tag) || identical(character(0), x_tag)) {
    return(NA_character_)
  }
  if(!is.character(x_tag)) {
    stop("character expected for parsing, but encountered ", 
         class(x[[1]]), " at x[[1]] instead")
  }
  
  comma_present <- any(vapply(x_tag, function(f) grepl(",", f), logical(1)))
  if(comma_present) {
    collapse_prefix <- "["
    collapse_suffix <- "]"
  } else {
    collapse_prefix <- ""
    collapse_suffix <- ""
  }
  
  out <- vapply(x_tag, function(f) paste0(collapse_prefix, f, collapse_suffix), character(1))
  out <- paste(out, collapse = ", " )
  # if any NA literal were pasted, coerce back to NA_character
  out[out == "NA"] <- NA_character_
  return(out)

}
