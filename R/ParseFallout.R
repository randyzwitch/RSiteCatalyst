#' ParseFallout
#'
#' Internal Function - Parses a fallout report returned from the API
#'
#' @param report.data jsonlite formatted data frame of report data returned from the API
#'
#' @return Formatted data frame
#'
#' @family internal
#'

ParseFallout <- function(report.data) {

  # jsonlite puts this into a useful format
  # so just leave it as is, but rename the counts column to the metric
  data <- report.data$report$data
  colnames(data)[colnames(data) == "counts"] <- as.numeric(report.data$report$metrics[1,1])
  return(data)

}
