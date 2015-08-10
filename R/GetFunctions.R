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
  request <- ApiRequest(body=toJSON(request.body),func.name="CalculatedMetrics.GetFunctions")
  
  return(request)
  
  
}