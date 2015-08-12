#' ParseSummary
#'
#' Internal Function - Parses a summary report returned from the API
#'
#' @param report.data jsonlite formatted data frame of report data returned from the API
#'
#' @return Data frame
#'
#' @importFrom plyr rename
#'
#' @family internal
#' @keywords internal

ParseSummary <- function(report.data) {
  
  # jsonlite puts this into a useful format
  # so just leave it as is, but rename the counts column to the metric
  #Completely quick-and-dirty parser
  reportdata_ <- report.data[1]$report
  
  result <- cbind(reportdata_$period, reportdata_$data, t(as.data.frame(reportdata_$data[[3]])))
  result$counts <- NULL
  names(result) <- c("period", reportdata_$elements$id, "url", reportdata_$metrics$id)
  
  return(result)
}
