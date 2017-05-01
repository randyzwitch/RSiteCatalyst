#' Collapse a target list-column to a delimited atomic vector
#' 
#' A custom mechanism to unnest simple list-columns by collapsing into a delimited atomic vector
#' 
#' @param x A data.frame
#' @param targ A character vector of length 1 to denote the column in \emph{x} to operate on
#' 
#' @family internal
#' 
#' @details This wraps the internal function \code{collapse_simple_listcol} to handle more (and) typical
#' inputs, with some additional constraints. \emph{targ} is set to a default of \code{NULL}
#' intentionally, for flexibility and to enforce explicit provision of a value. However, it 
#' is expected that the valid values should typically be:
#' 
#' \itemize{
#' \item \preformatted{tags}
#' \item \preformatted{compatibility}
#' }
#' 
#' @note This function is called by \code{\link{call.Get_base}} if that function's \emph{collapse_simple}
#' argument is \code{TRUE}.
#' 
#' @return A unnested character vector; see \code{\link{collapse_simple_listcol}} for details.
#' 
#' @examples 
#' \dontrun{
#'simple_df <- data.frame(
#'   x = LETTERS[1:10],
#'   y = cbind(c(
#'     list(c(LETTERS[1:2])),
#'     list(c("tag1", "tag2", "tag3")),
#'     list(c("", NA_character_, " ")),
#'     letters[1:7])
#'   ),
#'   stringsAsFactors = FALSE
#' )
#' collapse_simple_target(simple_df, targ = "y")
#' simple_df$z <- collapse_simple_target(simple_df, targ = "y")
#' # Note that NA and blank will are counted, if present:
#' data.frame(
#'   n_parts = vapply(simple_df$z, function(f)
#'     nchar(gsub("[^,]", "", f))+1L, integer(1))
#' )
#' }
collapse_simple_target <- function(x, targ = NULL) {
  
  if(missing(targ) || is.null(targ)) {
    stop("A target column name must be specified")
  }
  if(length(targ) > 1L || !is.character(targ)) {
    stop("targ must be a character vector of length 1")
  }
  
  stopifnot(is.data.frame(x))
  
  # anticipating how this would be used, this check should never
  # be triggered unless this function is called on its own.
  if(!targ %in% names(x)) { 
    stop("'", substitute(targ), "' not found in x\n")
  }
  
  targ_col <- x[[targ]]
  
  if(length(targ_col) != nrow(x)) {
    stop("Mismatch in row count and target column length")
  }
  
  # handle NULLs
  targ_col <- .nullTOna(targ_col)

  if(length(targ_col) > 1L) {
    out <- vector("character", length(targ_col))
    for(i in seq_along(targ_col)) {
      out[i] <- collapse_simple_listcol(targ_col[i])
    }
    return(out)
  } else {
    return(collapse_simple_listcol(targ_col))
  }
  
}
NULL
.nullTOna <- function(x) {
  # can only work if a list
  if(!is.list(x)) {
    stop(substitute(x), " must be a list")
  }
  
  xclass.default <- "character"
  
  # try to find the class for non-null elements
  xfilt <- Filter(function(f) !is.null(f), x)
  if(length(xfilt) == length(x)) {
    return(x)
  }
  if(length(xfilt) == 0L) {
    xclass <- xclass.default
  }
  
  xclasses <- vapply(xfilt, class, character(1))
  if(length(unique(xclasses)) == 1L) {
    xclass <- unique(xclasses)
  } else {
    warning("More than one class detected; outputs will all be coerced to 'character'")
    xclass <- xclass.default
  }
  
  NA_type <- list(character = NA_character_, 
                  logical = NA, 
                  integer = NA_integer_, 
                  numeric = NA_real_, 
                  factor = NA_character_
  )
  
  NA_type <- NA_type[[xclass]]
  
  out <- Map(function(f) {
    f[is.null(f)] <- c(NA_type)
    f
  }, x
  )
  
  return(out)
}