#' GetUsageLog
#'
#' Gets the Adobe Analytics usage log for all users within the specified date range.
#'
#' @title Get Admin Actions, Logins, and Reports Accessed
#'
#' @param date.from Log start date (YYYY-MM-DD)
#' @param date.to Log end date (YYYY-MM-DD)
#' @param localtime Whether to change datetimes from UTC to local time
#'
#' @importFrom jsonlite toJSON unbox
#' @importFrom plyr rename
#'
#' @return data frame with: datetime, login, event_num, event_type, ip_address, report_suite, event_details
#'
#' @family internal
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' usagelog <- GetUsageLog("2014-01-01","2014-01-31")
#' }

GetUsageLog <- function(date.from=as.character(Sys.Date()-1),date.to=as.character(Sys.Date()),localtime=FALSE) {

  request.body <- c()
  request.body$date_from <- unbox(date.from)
  request.body$date_to <- unbox(date.to)

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")

  usagelog <- ApiRequest(body=toJSON(request.body),func.name="Logs.GetUsageLog")

  if(localtime==TRUE) {
    usagelog$timestamp <- as.POSIXlt(as.numeric(usagelog$timestamp), origin="1970-01-01")
  } else {
    usagelog$timestamp <- as.POSIXlt(as.numeric(usagelog$timestamp), origin="1970-01-01", tz="UTC")
  }
  usagelog <- rename(usagelog, c("timestamp"="datetime"))

  return(usagelog)

}