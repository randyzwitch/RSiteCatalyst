#' Get Adobe Analytics Segments or Calculated Metrics
#' 
#' Internal function - base args and doc for exported Segments and CM GET functions
#' 
#' @importFrom jsonlite unbox toJSON
#'
#' @family internal
#' 
#' @param accessLevel (optional) A character vector of length 1. Must be one of \code{all, shared, owned}. 
#' If not specified, defaults to \code{owned}; \code{all} can only be used by an admin.
#' @param fields (optional) A character vector denoting the quantity, depth, and general detail of information desired. 
#' Must be one of 
#' \code{tags, shares, description, owner, modified, compatibility, favorite, reportSuiteID, definition}. 
#' The API always includes \code{id} and \code{name}, by default.
#' @param selected (optional) A character vector of ID(s) you wish to query for. If both \code{selected} and 
#' \code{accessLevel} are provided, \code{selected} take precedence.
#' @param sort (optional) A character vector of length 1. Must be one of \code{id, name, description, reportSuiteID,
#' owner, modified, favorite}. If not specified, defaults to \code{id}.
#' @param filters (optional) A named \code{list}. Valid names include
#' \code{approved, favorite, name, owner, reportSuiteID, tags}. For \code{tags}, character vectors of length > 1 are supported, 
#' and will be collapsed into comma-separated vectors of length 1 per API requirements. For the other fields, the API supports 
#' only vectors of length 1. 
#' @param collapse_simple Should we parse simple list-columns, i.e. \code{tags} and \code{compatibility} in the return
#' value? Defaults to \code{TRUE} and only applies if these columns are requested via \emph{fields}
#' @param func.name Passed to \code{ApiRequest} to denote which method to call. Must be one of 
#' \code{Segments.Get,CalculatedMetrics.Get}.
#'
#' @return
#' A \code{data.frame}; the number of rows corresponds to the number of unique \code{id}s.
#' With default settings, a successful return will contain two fields, \code{id} and \code{name}. 
#' 
#' If requested (via \emph{fields}), the following columns are returned as list-columns of varying complexity:
#' 
#' \itemize{
#' \item{tags*}
#' \item{compatibility*}
#' \item{shares}
#' \item{definition}
#' }
#' 
#' \code{tags} and \code{compatibility} are called out because they are, by default, automatically collapsed into 
#' atomic vectors (i.e. unnested columns) if requested within \emph{fields}. Collapsing is performed by 
#' \code{\link{collapse_simple_target}}. 
#' 
#' Please see \code{\link{parse_shares}} for handling shares.
#' 
#' @section Access Privileges: 
#' This function calls an Adobe Analytics method that requires administrative/elevated privileges for
#' some or all functionality. 
#' 
#' @details 
#' It is possible to constrain results at the reportsuite ID (and more) level through the new \emph{filters}
#' argument. Note that \emph{filters} has some nuances; there are six fields, which are grouped by argument length,
#' then type, below:
#' 
#' \itemize{
#' \itemize{length 1, \code{character}; partial case-insensitive matching
#'     \item{name}
#'     \item{owner}
#'     \item{reportSuiteID}
#'     }
#' \itemize{length 1, \code{logical} (or coercible to logical, without
#'           generating \code{NA}); \code{TRUE}/\code{FALSE} selection
#'     \item{approved}
#'     \item{favorite}
#'     }
#' \itemize{> length 1, \code{character}; exact matching
#'     \item{tags}
#'     }
#' }
#' 
#' The \code{filters} argument as a whole is optional, but if used, the input must be a named list, where
#' the name(s) denote the field(s) to filter, by the provided value. Of the six available fields, only 
#' \code{tags} accepts inputs of length \code{>1}. Passing vectors of length \code{>1} to any of the other
#' fields will raise an error. 
#'
#' @examples
#' \dontrun{
#' # Get your segments or CMs, with id and name
#' my_own_simple.segments <- call.Get_base(func.name = "Segments.Get")
#' my_own_simple.calcMetrics <- call.Get_base(func.name = "CalculatedMetrics.Get")
#' 
#' }
call.Get_base <- function(accessLevel = NULL, fields = NULL, 
                              selected = NULL, sort = NULL, 
                              filters = NULL, 
                              collapse_simple = TRUE, 
                              func.name = c("Segments.Get", "CalculatedMetrics.Get")) {
  
  # accessLevel, must be vector of length 1
  validAccessLevel <- c("all", "shared", "owned")
  if(!is.null(accessLevel)) {
    # check input length, must be length 1
    if(length(accessLevel) > 1L) {
      stop("'accessLevel' must be a vector of length 1")
    }
    accessLevel <- .l_helper_inputCheck(nm = "accessLevel", 
                                        input = accessLevel, 
                                        ref = validAccessLevel, 
                                        unbox = TRUE
    )
  } else {
    accessLevel <- unbox("owned") # default
  }
  
  # fields, will always include id and name
  validFields <- c("tags", "shares",
                   "description", "owner",
                   "modified", "compatibility",
                   "favorite", "reportSuiteID",
                   "definition"
  )
  if(!is.null(fields)) {
    fields <- .l_helper_inputCheck(nm = "fields", 
                                   input = fields, 
                                   ref = validFields, 
                                   unbox = FALSE)
  }
  
  # selected, will override accessLevel if present
  if(!is.null(selected)) {
    selected <- c(selected, recursive=TRUE)
  } 
  
  # sort, must be vector of length 1
  validSort <- c("id", "name", "description", "reportSuiteID",
                 "owner", "modified", "favorite"
  )
  if(!is.null(sort)) {
    # check input length, must be length 1
    if(length(sort) > 1L) {
      stop("'sort' must be a vector of length 1")
    }
    sort <- .l_helper_inputCheck(nm = "sort", 
                                 input = sort, 
                                 ref = validSort, 
                                 unbox = TRUE)
  } else {
    sort <- unbox("id")
  }
  
  # filters; has own helper, as must provide a named list to arg
  if(!is.null(filters)) {
    filters <- .l_helper_process_filters(filters)
    filters <- lapply(filters, function(f) unbox(f))
  }
  
  body <- list(accessLevel = accessLevel, 
               fields = fields, 
               selected = selected, 
               sort = sort, 
               filters = filters
  )
  
  body <- Filter(function(x) !is.null(x), body)
  
  query <- toJSON(body)
  out <- ApiRequest(body = query, func.name = func.name)
  
  # parse simple list-col(s) if call is successful and at least one such column is present
  if(collapse_simple) {
    if(is.data.frame(out)) {
      simple_cols <- intersect(c("compatibility", "tags"), names(out))
      if(length(simple_cols) > 0L) {
        for(i in simple_cols) {
          out[[i]] <- collapse_simple_target(out, i)
        }
      }
    }
  }
  return(out)
}


NULL

# helper to validate and preprocess filters arg
.l_helper_process_filters <- function(arglst) {
  if(!is.list(arglst)) {
    stop("Class of 'filters' must be a list, but is currently ", 
         class(arglst), call. = FALSE)
  }
  argNms <- names(arglst)
  if(any(argNms %in% c("")) | all(is.null(argNms))) {
    stop("One or more names missing in 'filters'; all elements of 'filters' must be named", 
         call. = FALSE)
  }
  validNms <- c("approved", "favorite",
                "owner", "name", 
                "reportSuiteID", "tags"
  )
  if(! all(argNms %in% validNms)) {
    .l_helper_inputCheck(nm = "filters", 
                         input = argNms, 
                         ref = validNms, 
                         chk_names_instead = TRUE)
  }
  
  # process args for certain names by data type req
  logi_nms <- c("approved", "favorite")
  chr_nms  <- setdiff(validNms, c(logi_nms, "tags"))
  tag_nm   <- "tags"
  
  logi_elems <- Filter(function(x) !is.null(x), arglst[logi_nms])
  chr_elems  <- Filter(function(x) !is.null(x), arglst[chr_nms])
  tag_elem   <- Filter(function(x) !is.null(x), arglst[tag_nm])
  
  #check and process logi_elems
  if(length(logi_elems) > 0L) {
    logi_lenTrap <- any(vapply(logi_elems, length, integer(1)) > 1L)
    if(logi_lenTrap) {
      stop("filters named arguments of ", paste(logi_nms, collapse = ", "), 
           " must all be vectors of length 1"
      )
    }
    logi_invalid <- anyNA(vapply(logi_elems, as.logical, logical(1)))
    if(logi_invalid) {
      stop("Invalid inputs resulting in 'NA' upon coercion to 'logical' 
           detected in one or more of ", paste(logi_nms, collapse = ", ")
      )
    }
    
    logi_out <- lapply(logi_elems, function(f) as.logical(f))
  } else {
    logi_out <- NULL
  }
  #check and process chr_elems
  if(length(chr_elems) > 0L) {
    chr_lenTrap <- any(vapply(chr_elems, length, integer(1)) > 1L)
    if(chr_lenTrap) {
      stop("filters named arguments of ", paste(chr_nms, collapse = ", "), 
           " must all be vectors of length 1"
      )
    }
    chr_out <- chr_elems
  } else {
    chr_out <- NULL
  }
  #check and process tag_elem
  if(length(tag_elem) > 0L) {
    tag_out <- lapply(tag_elem, function(f) 
      paste(f, collapse = ",", sep = ",")
    )
  } else {
    tag_out <- NULL
  }
  
  c(logi_out, chr_out, tag_out)
}

NULL

# helper to validate, optionally preprocess, and return values or error msg
.l_helper_inputCheck <- function(nm, input, ref, 
                                 collapse_lst = TRUE, dedupe = TRUE, unbox = FALSE, 
                                 msgOnly = FALSE, 
                                 chk_names_instead = FALSE) {
  # allow input to be vector of mode atomic and 'list'
  if(collapse_lst & is.list(input)) {
    input <- c(input, recursive = TRUE)
  }
  # pretty sure default of TRUE is the right call here
  if(dedupe) {
    input <- unique(input)
  }
  # Handle messaging for errors, or valid returns, assuming msgOnly is FALSE (default)
  # chk_names_instead to ensure sensible message in case of filters, where checking is at
  #  element names level
  delta <- setdiff(input, ref)
  if(chk_names_instead) {
    msg_base = "name(s)"
  } else {
    msg_base = "values(s)"
  }
  # if errors, stop with message, else depends on unbox and msgOnly vals
  if(length(delta) > 0L) {
    msg <- paste(
      "Invalid input ", msg_base, " detected in '", nm, "':",
      "\n\t", paste(delta, collapse = ", "),
      "\nValid ", msg_base, " are: ", paste(sort(ref), collapse = ", "), 
      sep = ""
    )
    stop(message = msg, call. = FALSE)
  } else {
    if(msgOnly) {
      return(invisible(NULL))
    }
    if(unbox) {
      return(unbox(input))
    } else {
      return(input)
    }
  }
}