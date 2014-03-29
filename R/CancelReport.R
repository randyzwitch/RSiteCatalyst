#' CancelReport
#'
#' Cancels the specified report
#'
#' @param report.id id of the report that you want to cancel
#'
#' @return nothing
#'
#' @importFrom httr content add_headers POST
#' @importFrom jsonlite toJSON fromJSON
#'
#' @export
#'
#' @examples
#' \dontrun{
#' queue <- GetQueue()
#' }

CancelReport <- function(report.id) {

  request.body <- sprintf('{"reportID": %s}',report.id)

  # This does not use ApiRequest() because it does not return JSON
  endpoint <- SC.Credentials$endpoint
  if(SC.Credentials$auth.method=='OAUTH2') {
    url <- paste0(endpoint,'?method=Report.Cancel&access_token=',SC.Credentials$access_token)
    response <- POST(url, body=request.body)
  } else if(SC.Credentials$auth.method=='legacy') {
    url <- paste0(endpoint, '?method=Report.Cancel')
    response <- POST(url, config=add_headers('',.headers=BuildHeader()), body=request.body)
  }

  result <- content(response,'text')

  if(result=='true') {
    print(paste0("Report #",report.id," was successfully cancelled."))
  } else {
    result <- fromJSON(result)
    print(paste0('ERROR: ',response.content$error," - ",response.content$error_description))
  }

}