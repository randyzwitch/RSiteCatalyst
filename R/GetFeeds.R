#' @details This function requires having a character vector with one or more valid Report Suites specified. All other function arguments are optional.
#'
#' @description Returns a list of data feeds for the specified report suites,
#' including delivery status. Note that the difference between start.time and end.time can be no
#' more than 48 hours.
#'
#' @title Get Data Feed Detail for a Report Suite(s)
#'
#' @param reportsuite.ids Report suite id (or list of report suite ids)
#' @param start.time Beginning of time period you want to check
#' @param end.time End of time period you want to check
#' @param status Character vector/list of statuses to filter by (processing/delivered/upload_error/no_data)
#'
#' @importFrom jsonlite toJSON
#' @importFrom plyr rbind.fill
#'
#' @return Data frame
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' #Get info for all feeds for a report suite in past day
#' feeds <- GetFeeds("zwitchdev")
#'
#' #Get info for all feeds for a 48-hour period
#' feeds2 <- GetFeeds("zwitchdev", "2014-12-02 05:00:00", "2014-12-03 05:00:00")
#'
#' }

GetFeeds <- function(reportsuite.ids, start.time="", end.time="", status=c()) {

  request.body <- c()
  request.body$rsid_list <- reportsuite.ids

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")

  if(start.time != ""){
    request.body$start_time <- unbox(start.time)
  }

  if(end.time != ""){
    request.body$end_time <- unbox(end.time)
  }

  if(length(status) > 0){
    request.body$status <- status
  }

  response <- ApiRequest(body=toJSON(request.body),func.name="DataFeed.GetFeeds")
  
  r <- response$data_feeds
  
  feedstatus_activity <- ldply(r$activity, quickdf)
  r$activity <- NULL
  r_ <- cbind(r, feedstatus_activity)

  #Could be parsed more, but currently returns data frame mostly parsed
  return(r_)

}