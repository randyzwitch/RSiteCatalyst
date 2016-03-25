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

  #If segment null, make a dummy data frame
    if(is.null(seg)){
      seg <- data.frame(list("", ""))
      names(seg) <- c("segment.id", "segment.name")
    }

  #If segment has values, concatenate all values with "AND".  R puts the
  #concatenated values in every single row, so I dedupe the dataframe
    else{
    names(seg) <- c("segment.id", "segment.name")
    seg$segment.name<-(paste(as.list(seg$segment.name),collapse=" AND "))
    seg$segment.id<-(paste(as.list(seg$segment.id),collapse=" AND "))

    seg<-subset(seg,!duplicated(seg$segment.name))}

    #Put segment after dates
    if(nrow(formatted.df) > 0){
      formatted.df <- cbind(formatted.df, seg, row.names = NULL)
    }

    return(formatted.df)

}
