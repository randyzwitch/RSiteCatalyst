#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get paid search detection parameters for the specified report suites.
#'
#' @title Get Paid Search Detection Parameters for a Report Suite(s)
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
#' paidsearch <- GetPaidSearchDetection("your_report_suite")
#'
#' paidsearch2 <- GetPaidSearchDetection(report_suites$rsid)
#' }

GetPaidSearchDetection <- function(reportsuite.ids) {

  request.body <- c()
  request.body$rsid_list <- reportsuite.ids

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")

  response <- ApiRequest(body=toJSON(request.body),func.name="ReportSuite.GetPaidSearchDetection")

  #Don't even know if this is possible, holdover from GetSegments code
  if(length(response$paid_search_detection[[1]]) == 0) {
      return(print("Paid Search Detection Not Enabled For This Report Suite"))
    }

  #Parse first level of classification
  accumulator <- data.frame()
  paid_search_detection_list <- response$paid_search_detection
  response$paid_search_detection <- NULL

  for(i in 1:nrow(response)){
    #Split get element classifications out of report
    paid_search_detection_df <- as.data.frame(paid_search_detection_list[[i]])
    if(nrow(paid_search_detection_df) == 0){
      temp <- response[i,]
    } else {
      temp <- cbind(response[i,], paid_search_detection_df, row.names = NULL)
    }
    accumulator <- rbind.fill(accumulator, temp)
  }

  return(accumulator)

}