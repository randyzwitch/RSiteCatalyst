#' ParseOvertime
#'
#' Internal Function - Parses an overtime report returned from the API
#'
#' @param report.data jsonlite formatted data frame of report data returned from the API
#'
#' @importFrom plyr ldply
#'
#' @return Formatted data frame
#'
#' @family internal
#' @keywords internal

ParseOvertime <- function(report.data) {

  # jsonlite already makes this into a nice data frame for us
  data <- report.data$report$data

  # Create a column of R datetimes
  if('hour' %in% colnames(data)){
    datetime <- strptime(paste(data$year,data$month,data$day,data$hour,sep="-"), "%Y-%m-%d-%H")
  } else {
    datetime <- strptime(paste(data$year,data$month,data$day,sep="-"), "%Y-%m-%d")
  }

  counts.df <- ldply(data$counts)

  metrics <- report.data$report$metrics$id
  names(counts.df) <- metrics #assign names to counts.df

  drops <- c("counts")
  rows.df <- data[,!(names(data) %in% drops)]

  # check if we have anomaly detection
  if("forecasts" %in% colnames(data)) {
    forecasts.df <- ldply(data$forecasts)
    names(forecasts.df) <- paste("forecast.",metrics,sep="")
    counts.df <- cbind(counts.df,forecasts.df)
    drops <- c("forecasts")
    rows.df <- rows.df[,!(names(rows.df) %in% drops)]
  }

  if("upperBounds" %in% colnames(data)) {
    upperBounds.df <- ldply(data$upperBounds)
    names(upperBounds.df) <- paste("upperBound.",metrics,sep="")
    counts.df <- cbind(counts.df,upperBounds.df)
    drops <- c("upperBounds")
    rows.df <- rows.df[,!(names(rows.df) %in% drops)]
  }

  if("lowerBounds" %in% colnames(data)) {
    lowerBounds.df <- ldply(data$lowerBounds)
    names(lowerBounds.df) <- paste("lowerBound.",metrics,sep="")
    counts.df <- cbind(counts.df,lowerBounds.df)
    drops <- c("lowerBounds")
    rows.df <- rows.df[,!(names(rows.df) %in% drops)]
  }

  # convert all count columns to numeric
  for(i in 1:ncol(counts.df)) {
    counts.df[,i] <- as.numeric(counts.df[,i])
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
  formatted.df <- cbind(datetime,rows.df, seg, counts.df, row.names = NULL)

  return(formatted.df)

}
