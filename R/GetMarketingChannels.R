#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get defined marketing channels for each of the specified report suites.
#'
#' @title Get Defined Marketing Channels for a Report Suite(s)
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
#' mchan <- GetMarketingChannels("your_report_suite")
#'
#' mchan2 <- GetMarketingChannels(report_suites$rsid)
#' }

GetMarketingChannels <- function(reportsuite.ids) {

  request.body <- c()
  request.body$rsid_list <- reportsuite.ids

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")

  response <- ApiRequest(body=toJSON(request.body),func.name="ReportSuite.GetMarketingChannels")

  #Don't even know if this is possible, holdover from GetSegments code
  if(length(response$marketing_channels[[1]]) == 0) {
    return(print("Marketing Channels Not Enabled For This Report Suite"))
  }

  #Parse first level of classification
  accumulator <- data.frame()
  marketing_channels_list <- response$marketing_channels
  response$marketing_channels <- NULL

  for(i in 1:nrow(response)){
    #Split get element classifications out of report
    marketing_channels_df <- as.data.frame(marketing_channels_list[[i]])
    if(nrow(marketing_channels_df) == 0){
      temp <- as.data.frame(response[i,])
    } else {
      temp <- cbind(response[i,], marketing_channels_df, row.names = NULL)
    }
    accumulator <- rbind.fill(accumulator, temp)
  }

  accumulator <- rename(accumulator, c("response[i, ]" = "rsid"))

  return(accumulator)

}