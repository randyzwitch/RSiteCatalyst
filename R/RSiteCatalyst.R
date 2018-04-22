#' Package:  RSiteCatalyst \cr
#' Type:     Package \cr
#' Version:  1.4.15 \cr
#' Date:     2018-04-21 \cr
#' License:  MIT + file LICENSE \cr
#'
#'
#' 
#' @name RSiteCatalyst
#' @docType package
#' @title R Client for Adobe Analytics API V1.4 
#' @author Willem Paling, Randy Zwitch, Jowanza Joseph
#' @description 
#' This package contains an "analyst's toolbox" of functions for accessing the Adobe Analytics Reporting API v1.4. 
#' These functions allow the user to authenticate, get metadata about report suites (eVars, props, events, segments, etc.), and create reports using Adobe Analytics data.
#' 
#' This package is not intended for Adobe Analytics administration.
#' 
#' 
#' @references
#' Official Adobe Analytics API documentation: 
#' 
#' https://marketing.adobe.com/developer/en_US/documentation 
#' 
#' For support & bugs:
#' 
#' https://github.com/randyzwitch/RSiteCatalyst
#' 

utils::globalVariables("SC.Credentials")
utils::globalVariables("reportsuite.id") #RZ: Silence CRAN check
utils::globalVariables("SC.Debug")
