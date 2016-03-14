#' @details Returns either a console message that no reports are queued 
#' or the reportID number that was cancelled
#'
#' @description Cancels a report in the Report Queue
#'
#' @title Cancel a Report in the Report Queue
#' @param report.id Id of the report that you want to cancel
#'
#' @return Console message
#'
#' @importFrom httr content add_headers POST
#' @importFrom jsonlite toJSON fromJSON
#'
#' @export
#'
#' @examples
#' \dontrun{
#' CancelReport('12345678')
#' }

CancelReport <- function(report.id) {

  request.body <- sprintf('{"reportID": %s}',report.id)

  # This does not use ApiRequest() because it does not return JSON
  endpoint <- AdobeAnalytics$SC.Credentials$endpoint
  if(AdobeAnalytics$SC.Credentials$auth.method=='OAUTH2') {
    url <- paste0(endpoint,'?method=Report.Cancel&access_token=',AdobeAnalytics$SC.Credentials$access_token)
    response <- POST(url, body=request.body)
  } else if(AdobeAnalytics$SC.Credentials$auth.method=='legacy') {
    url <- paste0(endpoint, '?method=Report.Cancel')
    response <- POST(url, config=add_headers('',.headers=BuildHeader()), body=request.body)
  }

  result <- content(response,'text', encoding = "UTF-8")

  if(result=='true') {
    print(paste0("Report #",report.id," was successfully cancelled."))
  } else {
    result <- fromJSON(result)
    #print(paste0('ERROR: ',response.content$error," - ",response.content$error_description))
    #RZ, 4/18/14: Commented the above out, R CMD Check was throwing warning
    #Appears that response.content isn't correct reference, changed to result
    print(paste0('ERROR: ',result$error," - ",result$error_description))
  }

}