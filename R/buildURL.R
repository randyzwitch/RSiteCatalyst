#Build URL string

buildURL <- function(method){
  
  #Get endpoint from SCCredentials position 3
  
  endpoint <- SCCredentials[3]
  
  return(paste(endpoint, "?method=",method, sep=""))
}