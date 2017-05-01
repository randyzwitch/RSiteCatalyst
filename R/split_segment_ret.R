#' split_segment_ret
#' 
#' Internal function - splits data.frame into rows, with checks
#' 
#' @param seg_ret A data.frame from a successful call to Segments.Get via \code{\link{Segments_Get}}
#' 
#' @return A list of length equal to \code{nrow(x)}
#' 
#' @family internal
split_segment_ret <- function(seg_ret) {
  # check class
  if(!is.data.frame(seg_ret)) {
    stop("seg_ret is of class ",
         class(seg_ret), " but expected a data.frame"
    )
  }
  # get id
  id <- seg_ret[["id"]]
  # check row and id match
  if(nrow(seg_ret) != length(id)) {
    stop("Mismatch in number of rows and count of ids\n", 
         "  There are ", nrow(seg_ret), " rows",
         " but ", length(id), " ids"
    )
  }
  
  out <- lapply(seq_along(id), function(f)
    seg_ret[f, ])
  names(out) <- id
  
  return(out)
}
