#Store credentials as character vector for later usage

utils::globalVariables("SCCredentials")

#Create function defining a global variable to hold user_name and shared_secret


#' SiteCatalyst User Name & Shared Secret
#' 
#' Saves SiteCatalyst User Name and Shared Secret for use in authentication.
#' 
#' This function is run once at the beginning of a session to save credentials.
#' The credentials list is referenced each time an API call is sent, in order
#' to create the proper authentication header.
#' 
#' Note that while the Shared Secret is input in plain text in the R script and
#' stored within an R session in a list, all user credentials are hashed using
#' the SHA1 algorithm and converted to base64 as required by Adobe before
#' making the API call.
#' 
#' In order to access Reporting API, user must be part of the "Web Service
#' Access" user group (This is set in the Admin panel).
#' 
#' When credentials are set properly, a message of "Authentication succeeded"
#' will be printed to the console.
#' 
#' @param user_name Username:Company
#' @param shared_secret Shared Secret
#' @return List named "SCCredentials"
#' @seealso \code{\link{GetTokenCount}}
#' @keywords authentication
#' @examples
#' 
#' \dontrun{
#' 
#'     
#'     SCAuth("username:company", "28fa10193b6006badb981f6d0c370688")
#'     
#'     }
#' 
#' @export SCAuth
SCAuth <- function(user_name, shared_secret){
  #Silence visible binding error
  
  SCCredentials <- ""
  
  error_flag = 0
  if(str_count(user_name, ":") != 1){
    warning("Check User Name. Must have 'username:company' pattern")
    error_flag = error_flag + 1
  }
  if(nchar(shared_secret) != 32){
    warning("Shared Secret does not have valid number of characters (32)")
    error_flag = error_flag + 1
  }
  
  if(error_flag >0){
    stop("Authentication failed due to errors")
  } else {
  
    
  company <- str_split_fixed(user_name, ":", 2)
    
  #Create SCCredentials object in Global Environment
  SCCredentials <<- c(user_name, shared_secret)
  
  #Assign endpoint to 3rd position in credentials
  SCCredentials[3] <<- GetEndpoint(company[2])

  print("Authentication succeeded")
  }
} #End function bracket  
