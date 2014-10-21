#' @description Get Products/Versions associated with a specific company
#'
#' @details Returns a Data Frame of Adobe Analytics products 
#' 
#' @title Get Products/Versions associated with a specific company
#' 
#' @return DataFrame
#'
#' @export
#'
#' @examples
#' \dontrun{
#' versions <- GetVersionAccess()
#' }


GetVersionAccess <- function() {

  gva <- ApiRequest(func.name="Company.GetVersionAccess")
  
  gva <- as.data.frame(gva)
  names(gva) = "AdobeProducts"
  return(gva)

}