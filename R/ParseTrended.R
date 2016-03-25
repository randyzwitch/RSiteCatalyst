#' ParseTrended
#'
#' Internal Function - Parses a trended report returned from the API
#'
#' @param report.data jsonlite formatted data frame of report data returned from the API
#'
#' @importFrom plyr rename
#'
#' @return Formatted data frame
#'
#' @family internal
#' @keywords internal

ParseTrended <- function(report.data) {

  # jsonlite already makes this into a nice data frame for us
  data <- report.data$report$data

  elements <- report.data$report$elements
  metrics <- report.data$report$metrics$id

  formatted.df <- data.frame()

  # We need to work our way down the nested data structure
  # We've essentially got a ranked report for each date
  for(i in 1:nrow(data)) {

    if(nrow(elements)>1){
      # if we have multiple elements, then build inner breakdowns
      breakdown <- data[i,"breakdown"][[1]]
      if(length(breakdown)>0) {
        temp <- BuildInnerBreakdownsRecursively(breakdown,elements,metrics,1,c())
      }
    } else {
      # if we have just one element, then we just process this, as we may have anomaly detection
      temp <- data[i,"breakdown"][[1]]

      if(!is.null(temp)&&length(temp)>0) {
        counts.df <- ldply(temp$counts)
        names(counts.df) <- metrics #assign names to counts.df

        # check if we have anomaly detection
        if("forecasts" %in% colnames(temp)) {
          forecasts.df <- ldply(temp$forecasts)
          names(forecasts.df) <- paste("forecast.",metrics,sep="")
          counts.df <- cbind(counts.df,forecasts.df)
        }

        if("upperBounds" %in% colnames(temp)) {
          upperBounds.df <- ldply(temp$upperBounds)
          names(upperBounds.df) <- paste("upperBound.",metrics,sep="")
          counts.df <- cbind(counts.df,upperBounds.df)
        }

        if("lowerBounds" %in% colnames(temp)) {
          lowerBounds.df <- ldply(temp$lowerBounds)
          names(lowerBounds.df) <- paste("lowerBound.",metrics,sep="")
          counts.df <- cbind(counts.df,lowerBounds.df)
        }

        # convert all count columns to numeric
        for(j in 1:ncol(counts.df)) {
          counts.df[,j] <- as.numeric(counts.df[,j])
        }

        drops <- c("counts","forecasts","upperBounds","lowerBounds")
        temp <- temp[,!(names(temp) %in% drops)]
        temp <- cbind(temp,counts.df)
      }
    }

    # build out the date columns and bind them to the left of the data frame
    if(exists("temp")&&!is.null(temp)&&length(temp)>0) {
      date.df <- data.frame(matrix(NA, nrow = nrow(temp), ncol = 5))
      names(date.df) <- c("date_desc","datetime","year","month","day")
      date.df$date_desc <- data[i,]$name
      date.df$year <- data[i,]$year
      date.df$month <- data[i,]$month
      date.df$day <- data[i,]$day
      if('hour' %in% colnames(data[i,])){
        date.df$datetime <- strptime(paste(data[i,]$year,data[i,]$month,data[i,]$day,data[i,]$hour,sep="-"), "%Y-%m-%d-%H")
        date.df$hour <- data[i,]$hour
      } else {
        date.df$datetime <- strptime(paste(data[i,]$year,data[i,]$month,data[i,]$day,sep="-"), "%Y-%m-%d")
      }

      temp <- cbind(date.df,temp)

      # clean up redundant date field
      drops <- c("date_desc","year","month","day")
      temp <- temp[,!(names(temp) %in% drops)]

      if(nrow(formatted.df)>0) {
          formatted.df <- rbind(formatted.df,temp)
      } else {
        formatted.df <- temp
      }
    }
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

  if(nrow(formatted.df) > 0){
    formatted.df <- cbind(formatted.df, seg, row.names = NULL)
  }

return(formatted.df)
}
