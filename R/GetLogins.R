#' @details This function's arguments are both optional
#'
#' @description Get logins for a company
#'
#' @title Get Logins for a Company
#'
#' @param login.search.field Search login, first_name, last_name or title
#' @param login.search.value Value to search for (case-insensitive)
#'
#' @importFrom jsonlite toJSON
#' @importFrom plyr rbind.fill
#'
#' @return Data frame
#'
#' @export
#'
#' @examples
#' \dontrun{
#' logins<- GetLogins()
#'
#' logins2 <- GetLogins('last_name', 'zwitch')
#' }

GetLogins<- function(login.search.field='', login.search.value='') {

  request.body <- c()

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")

  if(login.search.field != ''){
    request.body$login_search_field <- unbox(login.search.field)
  }

  if(login.search.value != ''){
    request.body$login_search_value <- unbox(login.search.value)
  }


  response <- ApiRequest(body=toJSON(request.body),func.name="Permissions.GetLogins")

  #Returns values, no need for parsing!
  return(response)

}