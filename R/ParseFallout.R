#' ParseFallout
#'
#' Internal Function - Parses a fallout report returned from the API
#'
#' @param report.data jsonlite formatted data frame of report data returned from the API
#'
#' @return Data frame
#'
#' @importFrom plyr rename
#'
#' @family internal
#' @keywords internal

ParseFallout <- function(report.data) {

  # jsonlite puts this into a useful format
  # so just leave it as is, but rename the counts column to the metric
  data <- report.data$report$data
  data <- rename(data, c("counts"=report.data$report$metrics$id))
  return(data)

}
