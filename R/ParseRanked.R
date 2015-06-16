#' ParseRanked
#'
#' Internal Function - Parses a ranked report returned from the API
#'
#' @param report.data jsonlite formatted data frame of report data returned from the API
#'
#' @importFrom plyr ldply
#'
#' @return Formatted data frame
#'
#' @family internal
#' @keywords internal

ParseRanked <- function(report.data) {

  # jsonlite already makes this into a nice data frame for us
  data <- report.data$report$data

  elements <- report.data$report$elements
  metrics <- report.data$report$metrics$id

  if(nrow(elements)==1) {
    # We don't need to traverse down the data structure
    counts.df <- ldply(data$counts)
    names(counts.df) <- metrics #assign names to counts.df

    # convert all count columns to numeric
    for(i in 1:ncol(counts.df)) {
      counts.df[,i] <- as.numeric(counts.df[,i])
    }

    drops <- c("counts")
    rows.df <- data[,!(names(data) %in% drops)]

    formatted.df <- cbind(rows.df, counts.df)
  } else {
    # We need to work our way down the nested data structure
    formatted.df <- BuildInnerBreakdownsRecursively(data,elements,metrics,1,c())
  }
  
  #Get segment 
  seg <- report.data$report$segments
  
  #If segment null, don't add it in
  if(is.null(seg)){
    return(formatted.df)
  } else {
    names(seg) <- c("segment.id", "segment.name")
    return(cbind(formatted.df, seg, row.names = NULL))
  }

}
