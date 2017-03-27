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

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")


  response <- ApiRequest(body=toJSON(request.body),func.name="Permissions.GetLogin")

  temp_df <- as.data.frame(lapply(response[2:12], function (x) ifelse(is.null(x), "", x)))
  temp_df$group_names <- paste(response[[13]], collapse = ',')
  temp_df$selected_ims_group_list <- paste(response[[1]], collapse = ',')
  

  return(temp_df)

}