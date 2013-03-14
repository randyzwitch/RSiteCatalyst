#GetEndpoint - Find out what data center for a specific company 

GetEndpoint <- function(company) {

return(content(POST("https://api.omniture.com/admin/1.3/rest/?method=Company.GetEndpoint", add_headers(buildHeader()), 
sprintf('{"company": "%s"}',company))))


} #End function bracket