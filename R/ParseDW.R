#' ParseDW
#'
#' Internal Function - Parses a ranked report returned from the API
#'
#' @param report.data jsonlite formatted data frame of report data returned from the API
#' @param format "json" or "csv"
#'
#' @importFrom plyr ldply
#'
#' @return Formatted data frame
#'
#' @family internal
#' @keywords internal

ParseDW <- function(report.data, format = "json") {
  
  if(format == "json") {
    # jsonlite already makes this into a nice data frame for us
    data <- report.data$report$data
    
    elements <- report.data$report$elements
    metrics <- report.data$report$metrics
    
    if(nrow(elements)==1) {
      formatted.df <- data.frame()
      for(i in 1:length(data)){
        row <- data[i,]
        breakdown <- ldply(row$breakdown, quickdf)
        row$breakdown <- NULL
        counts <- ldply(breakdown$counts)
        breakdown$counts <- NULL
        names(breakdown) <- elements$id
        names(counts) <- metrics$id
        row_p <- cbind(report.data$report$reportSuite, row, breakdown, counts, row.names = NULL)
        formatted.df <- rbind.fill(formatted.df, row_p)
      }
    } else {
      # We need to work our way down the nested data structure
      formatted.df <- BuildInnerBreakdownsRecursively(data,elements,metrics,1,c())
    }
  } else {
    formatted.df <- report.data
  }

  return(formatted.df)
  
}