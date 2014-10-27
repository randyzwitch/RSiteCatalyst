#' @details This function requires a character string as the input
#'
#' @description Get login information for a single login 
#' 
#' @title Get Login Information for a Single Login
#' 
#' @param login Login to get information about
#'
#' @importFrom jsonlite toJSON
#' @importFrom plyr rbind.fill
#'
#' @return List
#'
#' @export
#'
#' @examples
#' \dontrun{
#' login_info<- GetLogin('login-name')
#' 
#' }

GetLogin<- function(login) {
  
  request.body <- c()
  request.body$login <- unbox(login)

  
  response <- ApiRequest(body=toJSON(request.body),func.name="Permissions.GetLogin")

  temp_df <- as.data.frame(lapply(response[1:11], function (x) ifelse(is.null(x), "", x)))
  temp_df$group_names <- paste(response[[12]], collapse = ',')
  
  return(temp_df)

}