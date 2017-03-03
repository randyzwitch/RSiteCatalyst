#' BuildInnerBreakdownsRecursively
#'
#' Internal function - Builds inner breakdowns from nested report structure
#'
#' @param parent.element element containing breakdown rows
#' @param elements list of all elements included in the report
#' @param metrics list of all metrics included in the report
#' @param current.recursion.level current recursion level, initial call should set this to 0, this increments as the function is called recursively
#' @param context initially a blank list, with additional context added as we go further down the data structure
#' @param accumulator data frame used to accumulate the report data 
#' (yes, I use some for loops here, if anyone can find a better way to do this with hierarchical data, please submit a pull request)
#' @param date.range date range for the report, these columns are added if specified
#'
#' @importFrom plyr ldply
#'
#' @return Flat data frame containing all key report data
#'
#' @export
#' @keywords internal

BuildInnerBreakdownsRecursively <- function(parent.element,elements,metrics,
                                            current.recursion.level,context,accumulator=data.frame(),
                                            date.range='') {
  
  #Use this to try and fix #99
  if(nrow(parent.element) == 0){
    return(data.frame())
  }

  # loop through all elements and work our way to innermost elements
  for(i in 1:nrow(parent.element)){
    
    working.element <- parent.element[i,"breakdown"][[1]]
    context <- context[1:current.recursion.level-1]
    context <- append(context,parent.element[i,"name"])
    
    # check if we are at the lowest level, or if we need to continue deeper
    
    if(current.recursion.level<(nrow(elements)-1)) {
      
      # we need to go deeper, so we add this level to the context 
      # and call BuildInnerBreakdownsRecursively again
      # we should not go deeper if we have a NULL breakdown
      #RZ 8/29: Modified SC.Debug logic for referencing hidden credentials list
      #Not sure I actually understand what this is doing, but it seems right
      if(is.null(working.element)||length(working.element)==0) {
        if(!is.null(AdobeAnalytics$SC.Credentials)&&!is.null(AdobeAnalytics$SC.Credentials$SC.Debug)&&AdobeAnalytics$SC.Credentials$SC.Debug==TRUE) {
          print(paste0("DEBUG: Warning! NULL breakdown returned for ",context))
        }
      } else {
        accumulator <- BuildInnerBreakdownsRecursively(working.element,elements,metrics,current.recursion.level+1,context,accumulator,date.range=date.range)
      }
      
    } else {

      if(is.null(working.element)||length(working.element)==0) {
        if(!is.null(AdobeAnalytics$SC.Credentials)&&!is.null(AdobeAnalytics$SC.Credentials$SC.Debug)&&AdobeAnalytics$SC.Credentials$SC.Debug==TRUE) {
          print(paste0("DEBUG: Warning! NULL breakdown returned for ",context))
        }
      } else {
        # we are at the lowest level
        # build our list of metrics
        working.metrics <- ldply(working.element$counts)
        names(working.metrics) <- metrics
        
        # check if we have anomaly detection
        if("forecasts" %in% colnames(working.element)) {
          forecasts.df <- ldply(working.element$forecasts)
          names(forecasts.df) <- paste("forecast.",metrics,sep="")
          working.metrics <- cbind(working.metrics,forecasts.df)
        }
        
        # convert all count columns to numeric
        for(i in 1:ncol(working.metrics)) {
          working.metrics[,i] <- as.numeric(working.metrics[,i])
        }
        
        # build our list of elements
        outer.elements <- working.element$name
        names(outer.elements) <- "name"

        # build named elements, with classifications if they exist
        #RZ: Have no idea why is.na and is.null and nchar is here, with is.na fixing GitHub #207
        elements.named <- elements$id
        for(i in 1:nrow(elements)) {
          if(!is.na(elements[i,]$classification) && !is.null(elements[i,]$classification) && nchar(elements[i,]$classification) > 0) {
            elements.named[i] <- paste0(elements[i,]$id,": ",elements[i,]$classification)
          }
        }
        
        # if we have a valid date range, apply it to all rows
        if(length(date.range)==2){
          working.elements.breakdown <- data.frame(matrix(ncol=nrow(elements)+2, nrow=length(outer.elements)))
          names(working.elements.breakdown) <- append(c("date.start","date.end"),elements.named)
          working.elements.breakdown$date.start <- date.range[1]
          working.elements.breakdown$date.end <- date.range[2]   
        } else {
          working.elements.breakdown <- data.frame(matrix(ncol=nrow(elements), nrow=length(outer.elements)))
          names(working.elements.breakdown) <- elements.named
        }
        
        # apply context to all rows
        for(j in 1:(nrow(elements)-1)) {
          working.elements.breakdown[j] <- context[j]
        }
        
        # apply outer element to all rows
        working.elements.breakdown[j+1] <- outer.elements
        
        # bind to the accumulator
        temp <- cbind(working.elements.breakdown,working.metrics)
        accumulator <- rbind(accumulator,temp)  
      }
      
    }
  }
  return(accumulator)
}
