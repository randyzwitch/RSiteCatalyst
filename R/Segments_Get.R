#' Get Segments via the 1.4 API Segments.Get method
#' 
#' Get segments information with different search criteria and return options
#' 
#' @importFrom jsonlite unbox toJSON
#' 
#' @export
#' 
#' @inherit call.Get_base params return details
#' @inheritSection call.Get_base Access Privileges
#' 
#' @section Definition Parsing:
#' This is still experimental: \code{\link{parse_segment_defn}}
#' 
#' @section API Method Info:
#' This function calls the Adobe Analytics 1.4
#' \href{https://marketing.adobe.com/developer/documentation/segments-1-4/r-get-1}{Segments.Get}
#' method, which supercedes the deprecated \emph{ReportSuite.GetSegments} method. 
#' The \emph{Segments_Get} method, and therefore this function, 
#' allows essentially all available information about one or more segments to be returned.
#' 
#' As such, it is now possible to download one or more complete segment definitions, which
#' may be useful for batch auditing, back-up, and much more. Note, though, that \emph{Segments.Get}
#' operates at the segment ownership level, as opposed to the reportsuite ID level, 
#' which means this is not a strict replacement for the (deprecated) \emph{ReportSuite.GetSegments} method.
#' 
#' If this function is called with \code{accessLevel = "all"} by a non-admin, the following error message is
#' returned:
#' 
#' \code{ERROR: Bad Request  -  invalid accessLevel, only an admin user can request "all" segments}
#' 
#' 
#' @examples 
#' \dontrun{
#' # Get all segments you own
#' Segments_Get()
#' 
#' # Get all segments that are shared with you
#' Segments_Get(accessLevel = "shared")
#' 
#' # Get all segments, period
#' # Note this requires admin privileges
#' Segments_Get(accessLevel = "all")
#' 
#' # To constrain at report suite level
#' Segments_Get(filters = list(reportSuiteID = "your_rsid"))
#' 
#' # Parsing is needed for certain fields, in particular 'definition'
#' # This returns some nested fields, but tags and compatibility are collapsed automatically...
#' needs_parsing_1 <- Segments_Get(fields = c("tags", "shares", "compatibility"))
#' # ...unless you request otherwise
#' needs_parsing_1_alt <- Segments_Get(fields = c("tags", "shares", "compatibility"), 
#'                                          collapse_simple = FALSE
#' )
#' 
#' # `definition` is the most complex
#' needs_parsing_2 <- Segments_Get(fields = c("definition"))
#' 
#' # Here's what it looks like if we ask for all fields
#' needs_parsing_3 <- Segments_Get(fields = c("compatibility", "definition", 
#'                                                 "favorite", "modified", 
#'                                                 "owner", "reportSuiteID", 
#'                                                 "shares", "tags")
#' )
#' 
#' }
Segments_Get <- function(accessLevel = NULL, fields = NULL, 
                          selected = NULL, sort = NULL, 
                          filters = NULL, 
                          collapse_simple = TRUE) {
  
  call.Get_base(accessLevel = accessLevel, fields = fields, 
                selected = selected, sort = sort, 
                filters = filters, 
                collapse_simple = collapse_simple,
                func.name = "Segments.Get")
  
}