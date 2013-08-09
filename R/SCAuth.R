#Store credentials as character vector for later usage

utils::globalVariables("SCCredentials")

#Create function defining a global variable to hold user_name and shared_secret
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