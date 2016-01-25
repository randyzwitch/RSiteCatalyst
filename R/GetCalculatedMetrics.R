#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get calculated metrics for the specified report suites.
#'
#' @title Get Calculated Metrics for a Report Suite(s)
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
#' calc<- GetCalculatedMetrics("your_report_suite")
#'
#' calc2 <- GetCalculatedMetrics(report_suites$rsid)
#' }

GetCalculatedMetrics<- function(reportsuite.ids) {

  request.body <- c()
  request.body$rsid_list <- reportsuite.ids

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")

  response <- ApiRequest(body=toJSON(request.body),func.name="ReportSuite.GetCalculatedMetrics")

  #Don't even know if this is possible, holdover from GetSegments code
  if(length(response$calculated_metrics[[1]]) == 0) {
      return(print("No Calculated Metrics Defined For This Report Suite"))
    }

  #Parse first level of classification
  accumulator <- data.frame()
  calculatedmetrics_list <- response$calculated_metrics
  response$calculated_metrics <- NULL

  for(i in 1:nrow(response)){
    #Split get element classifications out of report
    metrics_df <- as.data.frame(calculatedmetrics_list[[i]])
    if(nrow(metrics_df) == 0){
      temp <- response[i,]
    } else {
      temp <- cbind(response[i,], metrics_df, row.names = NULL)
    }
    accumulator <- rbind.fill(accumulator, temp)
  }

  accumulator <- rename(accumulator, c("name" = "formula_name"))

  return(accumulator)

}