#' Flatten a nested definition return
#' 
#' Flatten the definition return data structure for nested container(s) and rule(s)
#'
#' @param x The return from a call to Segments.Get via \code{\link{Segments_Get}} 
#' with a named element of \emph{definition} 
#' @param d Internal counter to track recursion iterations
#' @param out Accumulator for results
#' 
#' @details 
#' This is still experimental, and is not guaranteed to work for all cases; see below. Furthermore, 
#' note that \emph{parse} should not be taken as literal-- this function more accurately attempts
#' to flatten the often-nested list-of-\code{data.frame}(s) that comprises the return when \emph{definition} 
#' is requested in the \emph{fields} argument of \code{Segments_Get}.
#' 
#' There remain (at least) two edge cases that are not handled; the base method (on Adobe's end) 
#' currently cannot parse certain definitions, namely those with a \emph{then} operator. 
#' In fact, when a single such segment is a part of a set of returns, via \emph{Segments.Get}, 
#' the entire \code{definition} type is \code{list}, rather than \code{data.frame}. Furthremore,
#' the return value for the unparsable \emph{definition(s)} is a message along the lines of e.g:
#' 
#' \itemize{
#' \item \code{failed converting segment definition: failed converting container rule: datetime-within}
#' }
#' 
#' This function will pass such cases through, untouched.
#' 
#' The second case involves complex nesting patterns, where a nested container contains nested rules
#' contains nested containers, and so forth. In the unlikely event it was necessary to create a segment
#' in such a manner to begin with, you are on your own when it comes to parsing, and this function
#' will error when it encounters these scenarios. 
#'
#' @return 
#' A list, currently taking one of two possible patterns, both of which apply to single
#' homogeneous cases:
#' 
#' Nested rules, i.e. a definiton where \emph{rules} is nested within \emph{containers}, 
#' will return a list of length 2, with named elements of \code{res,rules}. 
#' 
#' Nested containers, i.e. a definitions where \emph{container} is nested within \emph{rules}, 
#' will return a list of length \emph{n}, where \emph{n} is the number of nested containers. 
#' 
#' Stacked segments are not meaningfully parsed, but they are flattened into a more readable 
#' structure (for now).
#' 
#' @note 
#' This function attempts to flatten a definition return, and should be used only
#' on single-row data frames.
#'
#' @examples
#' # TBD
flatten_nested_defn <- function(x, d = 0L, out = list()) {
  if("definition" %in% names(x)) {
    x <- x[["definition"]]
  }
  
  if(!(.is_nested_valid(x)) || is.null(x)) {
    msg_iters <- paste0("Converged in ", d, " iterations")
    message(msg_iters)
    
    out <- append(out, x)
    return(out)
  } else {
    tmp <- .flatten_defn(x)
    flatten_nested_defn(x = tmp[["rem"]], d = d+1L, out = append(out, tmp["res"]))
  }
}


# main fun
.flatten_defn <- function(x) {
  x <- .filt_rule_null(x)
  # get top level names, aside from rule
  nms_L1 <- setdiff(names(x[[1]]), "rules")
  out_L1 <- x[[1]][c(nms_L1)]

  # use helper to extract rest of res
  out_L2 <- .detect_and_parse(x)
  
  res <- c(out_L1, out_L2[["res"]])
  rem <- out_L2[["rem"]]
  
  list(res = res, rem = rem)
}


# helper to parse either container or rule nested
# logi are handled here for e.g. exclude
.detect_and_parse <- function(x) {
  
  check <- names(x[[1]][["rules"]][[1]])
  
  if(length(check) == 1L && check == "container") {
    # then nested rules
    extracted <- x[[1]][["rules"]][[1]][["container"]]
    nest_patt <- "rules"
  } else {
    # nested container
    extracted <- x[[1]][["rules"]][[1]]
    nest_patt <- "container"
  }
  
  nms <- setdiff(names(extracted), nest_patt)
  out <- extracted[, c(nms)]
  # impute defaults for NA
  out <- .fill_default_nm(out, nm = "exclude", FALSE)

  # the new name should be the opposite pattern
  nm_prefix <- setdiff(c("rules", "container"), nest_patt)
  names(out) <- paste(nm_prefix, nms, sep = ".")

  # handle rem
  still_nested <- nest_patt %in% names(extracted)
  if(!still_nested) {
    rem <- NULL
  } else {
    rem <- extracted[nest_patt]
  }
  
  list(res = out, rem = rem)
}

# helper null handler
.filt_rule_null <- function(x) {
  if(!.is_nested_valid(x)) {
    stop("a valid structure was not detected")
  }
  
  # create check structure to fill defaults
  # to avoid false positive for anyNA
  x_check <- x[[1]]
  x_check <- .fill_default_nm(x_check, nm = "exclude", FALSE)
  x_check <- .fill_default_nm(x_check, nm = "operator", "and")
  x_check <- .fill_default_nm(x_check, nm = "name", "")
  
  # look for NA
  if(!anyNA(x_check)) {
    
    return(x)
  } else {
    diffnms <- setdiff(names(x[[1]]), "rules")
    na_pos <- vapply(x, function(f) which(is.na(f)), integer(length(diffnms)))
    
    out <- data.frame(container = NA)
    out[[1]] <- x[-na_pos, ]
    return(out)
  }
}


# helper to inpute defaults selectively
.fill_default_nm <- function(x, nm, default) {
  
  if(is.null(nm) || is.null(default)) {
    stop("a value for 'nm' and 'default' must be provided")
  }
  if(length(nm) > 1L || length(default) > 1L) {
    stop("value for 'nm' and 'default' must both be of length 1")
  }
  
  if(!nm %in% names(x)) {
    return(x)
  }
  
  x_new <- x[[nm]]
  x_new[is.na(x_new)] <- default
  x[[nm]] <- x_new
  
  return(x)
}

# helper checker for structure validity
.is_nested_valid <- function(x) {
  has_cont      <- names(x) == "container"
  is_df_cont    <- is.data.frame(x)
  
  if(!has_cont || !is_df_cont) {
    return(FALSE)
  }
  
  has_rule <- "rules" %in% names(x[[1]])
  is_rule_list <- class(x[[1]][["rules"]]) == "list"
  
  if(!has_rule || !is_rule_list) {
    return(FALSE)
  }
  
  return(TRUE)
}
