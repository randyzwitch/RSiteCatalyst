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
  
  return(content(POST("https://api.omniture.com/admin/1.4/rest/?method=Company.GetEndpoint", add_headers(BuildHeader()), 
                      sprintf('{"company": "%s"}',company))))
   
} #End function bracket