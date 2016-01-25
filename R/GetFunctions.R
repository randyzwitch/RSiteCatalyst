#' @description Requests the id and definitions of functions in Adobe Analytics.
#'
#' @details Returns descriptions/formulas available within Adobe Analytics such as median, pi, regression, etc.
#'
#' @title Get Functions Defined in Adobe Analytics
#'
#' @return Data Frame
#'
#' @export
#'
#' @examples
#' \dontrun{
#' aa_functions <- GetFunctions()
#' }

GetFunctions <- function() {

  request.body <- c()
  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")

  request <- ApiRequest(body=toJSON(request.body),func.name="CalculatedMetrics.GetFunctions")

  return(request)


}