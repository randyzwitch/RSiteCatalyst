#Build URL string

#API Server is based on datacenter: 
#San Jose is api.omniture.com - 1
#Dallas api2.omniture.com - 2
#London api3 - 3
#Singapore - 4
#This can be determined by asking ClientCare or just a guess, no harm in trying either

buildURL <- function(method){
  #logic to make datacenter choice a parameter
  #if length 3 and value = 1, then default of api.omniture.com
  if(as.numeric(SCCredentials[3] == 1)) {
    server <- "https://api.omniture.com/admin/1.3/rest/?method="
  } else if(as.numeric(SCCredentials[3] == 2)){
    server <- "https://api2.omniture.com/admin/1.3/rest/?method="
  } else if(as.numeric(SCCredentials[3] == 3)){
    server <- "https://api3.omniture.com/admin/1.3/rest/?method="
  } else {
    server <- "https://api4.omniture.com/admin/1.3/rest/?method="
  }
  
  return(paste(server, method, sep=""))
}