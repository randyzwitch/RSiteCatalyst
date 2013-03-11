\name{QueueRanked}
\alias{QueueRanked}
\title{
    Run a QueueRanked Report
}
\description{
    A QueueRanked report is a report that shows the ranking of values for one or more elements relative to a metric, aggregated over the time period selected.
    
The QueueRanked report is analogous to pulling a "ranked" report within SiteCatalyst. Currently, the QueueRanked function returns a data frame equivalent to pulling a Ranked report with no correlations (traffic breakdown) or subrelations (eVar breakdown) in SiteCatalyst.
}
\usage{
    QueueRanked(reportSuiteID, dateFrom, dateTo, metrics, elements, top, startingWith, segment_id, selected)
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
    \item{metrics}{
       The metric you want in the report.
    }
    \item{elements}{
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

#Top 100 pages viewed/visits for Loyal_Visitors segment
top100_pages <- 
QueueRanked("keystonerandy", "2013-02-13","2013-02-28", c("pageviews", "visits"),'page',
top= "100", startingWith= "1", segment_id= "Loyal_Visitors")


#Get just the elements you want from the ranked list, instead of "Top"
single_pages <- 
QueueRanked("keystonerandy", "2013-02-13","2013-02-19", c("pageviews", "visits"),'page',
selected = c("http://randyzwitch.com", "http://randyzwitch.com/about"))    

   }
}
\keyword{QueueRanked}