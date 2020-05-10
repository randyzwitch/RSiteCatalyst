#' parse shares
#' 
#' For a return from call.Get_base() with 'shares', parse into more user-friendly output
#'
#' @param x a data.frame 
#'
#' @details 
#' A valid return, defined as a \code{data.frame} with the expected columns (see \strong{Value}), means that
#' the other possibly returned fields from \code{\link{call.Get_base}}, excluding \code{definition},
#' should be exported as a \code{list} of \code{data.frame}s. This is important for consistent return
#' data structure expectations, as it means we can either return a single data.frame, for the case where
#' the return from \emph{Segments.Get} does not include \code{definition} or \code{shares} -- or we must return
#' a \code{list}. To summarize, the following three sets of fields:
#' 
#' \itemize{
#' \item \preformatted{definition}
#' \item \preformatted{shares}
#' \item Everything else: \itemize{
#'                       \item \preformatted{tags}
#'                       \item \preformatted{owner}
#'                       \item \preformatted{description}
#'                       \item \preformatted{modified}
#'                       \item \preformatted{compatibility}
#'                       \item \preformatted{favorite}
#'                       \item \preformatted{reportSuiteID}
#'                      }
#' }
#' 
#' ...should (each) be returned as a separate data.frame (or data.table), and therefore, a \code{list} structure
#' makes the most sense for consistency, even though the latter is a bit clunky for the simplest case.
#'
#' @return 
#' Depending on the type and length of input, one of the following:
#' 
#' \itemize{
#' \item If \emph{x} is a single-row \code{data.frame} and \emph{shares} is an empty \code{list} or zero-row
#' \code{data.frame}:
#'     \itemize{
#'     \item \code{NA_character_}
#'     }
#'       If \emph{x} is a multi-row \code{data.frame}, this implies that there is at least one valid
#'       (data-containing) record for 'shares', and therefore:
#'    \itemize{
#'    \item a \code{data.frame} with as many rows as valid records and three columns, denoting
#'    \code{id,name,type} where \code{id} is the segment ID. Columns aside from \code{id} are
#'    prefixed with \code{shares.}. 
#'    }
#' }
#' 
#' @note 
#' This function does NOT check for \code{shares} names, i.e. it does not require that non zero-row
#' \code{shares} have names of \code{type} and \code{name}. This means it also does NOT check that
#' the values within \code{type} must be one of \code{user,group}. 
#' 
#' @export
#'
#' @examples
#' df <- data.frame(id = c("segment_id_1", "segment_id_2"), 
#' name = c("segment_name_1", "segment_name_2"),
#' shares = cbind(list(
#'   data.frame(list(
#'     type = c("user", "user", "group"),
#'     name = c("my_user1_name", "my_user2_name", "my_group_name")
#'   ),
#'   stringsAsFactors = FALSE),
#'   data.frame(list(type = "user",
#'                   name = "my_user1_name"),
#'              stringsAsFactors = FALSE)
#' )),
#' stringsAsFactors = FALSE
#' )
#' 
#' parse_shares(df)
#' \dontrun{
#' # Must have shares, id, and name
#' parse_shares(df[, c("name", "id")])
#' parse_shares(df[, c("id", "shares")])
#' parse_shares(df[, c("name", "shares")])
#' }
parse_shares <- function(x) {
  if(!is.data.frame(x)) {
    stop("x must be a data.frame")
  }
  if(!"shares" %in% names(x)) {
    stop("'shares' not found in x")
  }
  if(!all(c("id", "name") %in% names(x))) {
    stop("One or more expected names of 'id' and/or 'name' missing in x")
  }
  
  # get id and shares
  id <- x[["id"]]
  shares <- x[["shares"]]
  
  if(length(id) != length(shares)) {
    stop("Mismatch in length of 'id' and 'shares'")
  }
  
  if(!is.list(shares)) {
    stop("Expected a list for 'structures', but class is ", class(shares))
  }
  # handle all empty list or zero-row data.frame
  is_allEmpty <- all(
    vapply(shares, function(f) length(f) == 0L, logical(1))
  )
  if(is_allEmpty) {
    return(NA_character_)
  }
  
  # now do actual processing
  all_df <- all(vapply(shares, is.data.frame, logical(1)))
  if(!all_df) {
    stop("Unexpected data structure(s) detected in 'shares'")
  }
  
  # assign names and kill NA or 0-row
  names(shares) <- id
  shares <- Filter(function(x) nrow(x) > 0L || is.na(x), shares)
  
  if(length(shares) == 0L || is.null(shares)) {
    return(NA_character_)
  }
  
  for(i in seq_along(shares)) {
    shares[[i]]$id <- names(shares[i])
  }
  
  out <- do.call(rbind.data.frame, shares)
  rownames(out) <- NULL
  # ensure 'id' is the first column, append 'shares.' to non-id names
  non_id_nms <- setdiff(names(out), "id")
  out <- out[, c("id", non_id_nms)]
  return(
    setNames(out, c("id", paste0("shares.", non_id_nms)))
  )
  
}