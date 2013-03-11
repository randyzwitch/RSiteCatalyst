\name{QueueTrended}
\alias{QueueTrended}
\title{
    Run a QueueTrended Report
}
\description{
    A QueueTrended report is a report where a single metric is retrieved, broken down by an element such as page, eVar, prop, etc. and with a time component. Within the 'element' list, either the "Top X" number of elements can be received or you can specify the specific elements you are interested in (such as 3 specific page names).
    
The QueueTrended report is analogous to pulling a "trended" report within SiteCatalyst, but without the limitation of only 5 elements as in the SiteCatalyst interface.
}
\usage{
    QueueTrended(reportSuiteID, dateFrom, dateTo, dateGranularity, metric, element, top, startingWith, selected, segment_id)
}
\arguments{
    \item{reportSuiteID}{
        Report Suite ID
    }
    \item{dateFrom}{
        Report Start Date in "YYYY-MM-DD" format
    }
    \item{dateTo}{
        Report End Date in "YYYY-MM-DD" format
    }
    \item{dateGranularity}{
        "Day"", "Week"", "Month"", "Quarter"" or "Year"" (case-insensitive).
    }
    \item{metric}{
       The metric you want to trend.
    }
    \item{element}{
       The element (page, browser eVar, prop) for the report to be broken down by.
    }
    \item{top}{
       How many results you want trended. Used in combination with "startingWith". Not used if "selected" argument used.
    }
    \item{startingWith}{
       The first ranked number you want in the report. Used in combination with "top". Not used if "selected" argument used.
    }
    \item{selected}{
       List of selected values, such as specific pages or eVar values. Not used if "top" and "startingWith" arguments are used
    }
    \item{segment_id}{
        Optional. If no segment_id is specified, metrics will be reported for all visitors.
    }
}
\details{

Because of the Reporting API structure, this function first requests the report, then checks the reporting queue to see if the report is completed, and when the report returns as "done" pulls the report from the API. This checking process can occur up to 10 times, with a 10 second delay between status checks. If the report does not return as "done" after 10 tries, the function returns an error.

Note: Because of the multiple argument type ("top" and "startingWith" OR "selected"), keyword arguments are generally needed towards the end of the function call instead of just positional arguments.
   
}
\value{
    Data Frame
}

\seealso{
   \code{\link{GetAvailableMetrics}} \cr
   \code{\link{GetSegments}} \cr
   \code{\link{GetAvailableElements}}
     
}
\examples{
\dontrun{

#Pageviews for the Top 100 pages by Day in the Loyal_Visitors segment
pages_by_day <-  
QueueTrended("keystonerandy", "2013-02-13", "2013-02-19", "day", 
"pageviews", "page", top="100", startingWith= "1", segment_id= "Loyal_Visitors")


#Specifying two specific pages, trended by hour for pageviews in the Loyal_Visitors segment
specific_pages_by_hour <-  
QueueTrended("keystonerandy", "2013-02-13", "2013-02-19", "hour", 
"pageviews", "page", selected = c("http://randyzwitch.com", "http://randyzwitch.com/about"), segment_id = "Loyal_Visitors")
  
)
   }
}
\keyword{QueueTrended}