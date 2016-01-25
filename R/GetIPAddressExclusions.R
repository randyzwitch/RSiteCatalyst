#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get the IP address exclusions for the requested report suites.
#'
#' @title Get the IP Address Exclusions for a Report Suite(s)
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
#' ipexc <- GetIPAddressExclusions("your_report_suite")
#'
#' ipexc2 <- GetIPAddressExclusions(report_suites$rsid)
#' }

GetIPAddressExclusions <- function(reportsuite.ids) {

  request.body <- c()
  request.body$rsid_list <- reportsuite.ids

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")

  response <- ApiRequest(body=toJSON(request.body),func.name="ReportSuite.GetIPAddressExclusions")

  #Don't even know if this is possible, holdover from GetSegments code
  if(length(response$rsid[[1]]) == 0) {
      return(print("IP Exclusions Not Enabled For This Report Suite"))
    }

  #Parse first level of classification
  accumulator <- data.frame()
  ip_list <- response$ip_address_exclusions
  response$ip_address_exclusions <- NULL

  for(i in 1:nrow(response)){
    #Split get element classifications out of report
    ip_df <- as.data.frame(ip_list[[i]])
    if(nrow(ip_df) == 0){
      temp <- response[i,]
    } else {
      temp <- cbind(response[i,], ip_df, row.names = NULL)
    }
    accumulator <- rbind.fill(accumulator, temp)
  }

  return(accumulator)

}