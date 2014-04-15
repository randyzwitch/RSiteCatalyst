#' GetSegments
#'
#' Gets a list of segments for the specified report suites. 
#' Useful to find segment IDs for use in reporting helper functions or JSON report definitions.
#' 
#' @title Get Segments Defined within a Report Suite
#' 
#' @param reportsuite.ids report suite id (or list of report suite ids)
#'
#' @importFrom jsonlite toJSON
#' @importFrom plyr rbind.fill
#'
#' @return List of valid segments
#'
#' @export
#'
#' @examples
#' \dontrun{
#' segments <- GetSegments(c("your_prod_report_suite","your_dev_reportsuite"))
#' }

GetSegments <- function(reportsuite.ids) {
  
  request.body <- c()
  request.body$rsid_list <- reportsuite.ids
  
  valid.segments <- ApiRequest(body=toJSON(request.body),func.name="ReportSuite.GetSegments")

  segments.formatted <- data.frame()
  for (i in 1:length(valid.segments$rsid) ) {
    if(nrow(valid.segments$segments[[i]])>0) {
      valid.segments$segments[[i]]$report_suite <- valid.segments$rsid[[i]]
      if(nrow(segments.formatted)==0) {
        segments.formatted <- valid.segments$segments[[i]]
      } else {
        segments.formatted <- rbind.fill(segments.formatted,valid.segments$segments[[i]])
      }
    }
  }

  return(segments.formatted)

}