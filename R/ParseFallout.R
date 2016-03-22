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

  return(cbind(data, seg, row.names=NULL))

}
