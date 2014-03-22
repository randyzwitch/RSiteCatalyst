#GetEndpoint - Find out what data center for a specific company 

GetEndpoint <- function(company) {

response <- POST("https://api.omniture.com/admin/1.3/rest/?method=Company.GetEndpoint", 
       body = sprintf('{"company": "%s"}',company))

#Clean up response content
return(rjson::fromJSON(content(response, "text")))  

} #End function bracket
