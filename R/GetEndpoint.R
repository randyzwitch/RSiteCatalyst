#' GetEndpoint
#'
#' Internal function that gets proper API endpoint for a given 
#' Adobe Analytics Company 
#'
#'
#' @param company Adobe Analytics Report Suite Company
#'
#' @return String containing API endpoint
#'
#' @family internal
#' @keywords internal


#GetEndpoint - Find out what data center for a specific company 

GetEndpoint <- function(company) {
  
  # This does not use ApiRequest() because it does not return JSON
  endpoint <- content(POST('https://api.omniture.com/admin/1.4/rest/?method=Company.GetEndpoint', 
         body=sprintf('{"company": "%s"}',company)),'text', encoding = "UTF-8")
  endpoint <- gsub('\\','',gsub('"','',endpoint,fixed=TRUE),fixed=TRUE)
  return(endpoint)
   
} #End function bracket