#Build generic POST request
#Not sure if this is overkill

postRequest <- function(method,body=NULL){
  
  return(POST(buildURL(method), add_headers(buildHeader()), body))
  
}