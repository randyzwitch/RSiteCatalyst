#Code for error checking JSON response

jsonResponseError <- function(error_code) {
  
  if(error_code == 404) {
  
  return(print("404:URL Not Found. Check Credentials or Try a Different Data Center"))
  
} else if(error_code == 401) {
  
  return(print("401:Unauthorized, Probably a Credentials Issue or Misspelled Report Suite"))
  
} else if(error_code == 400) {
  
  return(print("400:Bad Request, Probably a Credentials Issue or Misspelled Report Suite"))
  
} else {
  
  return(print("Unknown Error"))
  
}
  
}#End function bracket