#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get scheduled traffic spikes for the specified report suites.
#'
#' @title Get Scheduled Traffic Spike Setting for a Report Suite(s)
#'
#' @param reportsuite.ids Report suite id (or list of report suite ids)
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
#' spike <- GetScheduledSpike("your_report_suite")
#'
#' spike2 <- GetScheduledSpike(report_suites$rsid)
#' }

GetScheduledSpike <- function(reportsuite.ids) {

  request.body <- c()
  request.body$rsid_list <- reportsuite.ids

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")

  response <- ApiRequest(body=toJSON(request.body),func.name="ReportSuite.GetScheduledSpike")

  #Don't even know if this is possible, holdover from GetSegments code
  if(length(response$scheduled_spike[[1]]) == 0) {
      return(print("No scheduled spikes For This Report Suite"))
    }

  #Parse first level of classification
  accumulator <- data.frame()
  scheduled_spike_list <- response$scheduled_spike
  response$scheduled_spike <- NULL

  for(i in 1:nrow(response)){
    #Split get element classifications out of report
    scheduled_spike_df <- as.data.frame(scheduled_spike_list[[i]])
    if(nrow(scheduled_spike_df) == 0){
      temp <- response[i,]
    } else {
      temp <- cbind(response[i,], scheduled_spike_df, row.names = NULL)
    }
    accumulator <- rbind.fill(accumulator, temp)
  }

  return(accumulator)

}