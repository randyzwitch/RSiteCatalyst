#' R Client for Adobe Analytics v1.4 API
#'
#'
#' R client library for the Adobe Analytics 1.4 API, currently in beta, but due out in Feb 2014.
#' 
#' @name RSiteCatalyst
#' @docType package
#' @title R Client for Adobe Analytics v1.4 API
#' @author Willem Paling, Randy Zwitch, Jowanza Joseph
#' 

utils::globalVariables("SC.Credentials")
utils::globalVariables("SC.Debug")
utils::globalVariables("reportsuite.id") #RZ: Silence CRAN check
SC.Debug <<- FALSE