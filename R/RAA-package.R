#' R Client for Adobe Analytics v1.4 API
#'
#' Pronounced RAAAH! (imagine you are a lion).
#'
#' R client library for the Adobe Analytics 1.4 API, currently in beta, but due out in Feb 2014.
#' Get in touch with me if you're interested in using it. It is functional, but has not been extensively tested.
#'
#' This library borrows from Randy Zwitch's excellent RSiteCatalyst package which provides access to the 
#' Adobe Analytics v1.3 API. RSiteCatalyst is on CRAN, so if you're looking for something tried and tested, 
#' that may be a better option. Install using install.packages("RSiteCatalyst").
#'
#' @name RAA
#' @docType package
#' @title R Client for Adobe Analytics v1.4 API
#' @author Willem Paling \email{willem.paling@gmail.com}

NULL

utils::globalVariables("RAA.Credentials")
utils::globalVariables("RAA.Debug")
RAA.Debug <<- FALSE