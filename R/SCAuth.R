#Store credentials as character vector for later usage

utils::globalVariables("SCCredentials")

#Create function defining a global variable to hold user_name and shared_secret
SCAuth <- function(user_name, shared_secret, datacenter){
  error_flag = 0
  if(str_count(user_name, ":") != 1){
    print("Error: Check User Name. Must have 'username:company' pattern")
    error_flag = error_flag + 1
  }
  if(nchar(shared_secret) != 32){
    print("Error: Shared Secret does not have valid number of characters (32)")
    error_flag = error_flag + 1
  }
  if(!(datacenter %in% (1:4))){
    print("Error: Data Center Values range from 1-4")
    error_flag = error_flag + 1
  }
  
  if(error_flag >0){
    return(print("Object 'omtrCredentials' not created due to errors"))
  } else {
  
  assign("SCCredentials", c(user_name, shared_secret, datacenter), .GlobalEnv)
  }
} #End function bracket  