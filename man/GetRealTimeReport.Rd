\name{GetRealTimeReport}
\alias{GetRealTimeReport}
\title{
    Get Real-Time Report
}
\description{
Get real-time report
   
}
\usage{
    GetRealTimeReport(report_suite, metrics, elements, periodMinutes, 
    periodCount, periodOffset, algorithm, algorithmArgument, firstRankPeriod, 
    floorSensitivity)
}
\arguments{
    \item{report_suite}{
        Report Suite ID
    }
    \item{metrics}{
        A list of the real time metrics to include in the report. A report must specify at least one metric, and each metric must be configured previously using ReportSuite.SaveRealTimeConfiguration. (Ranked/Overtime reports support one or more metrics. Trended reports support only one metric.) For example:metrics = [ {id = "pageviews"},{id = "visits"} ]
    }
    \item{elements}{
        A list of elements that breaks down (organizes) the metrics data in the report. Each element must be configured previously using ReportSuite.SaveRealTimeConfiguration.

If no elements are provided, an overtime report is generated for the provided metric(s).
    }
    \item{periodMinutes}{
        Number of minutes for one period. Default is 1.
    }
    \item{periodCount}{
        The number of periods of data to return. Default is 15.
    }
    \item{periodOffset}{
        Number of minutes before the current minute to run the report. If set to 10, the most recent result will be from ten minutes prior to the request. Default is 0.
    }
    \item{algorithm}{
       Type of dimensions to return, one of the following three values: 'gainers', 'losers', 'most popular'. Default is 'most popular'.
    }
    \item{algorithmArgument}{
        Specifies how to order the values for Most Popular, Gainers or Losers. Specify either percent, or linear. Default is linear.
    }
    \item{firstRankPeriod}{
        Computes the ranking of elements by considering the element's counts from the firstRankPeriod to the final period.
With this argument you can rank from the first period (0) to periodCount - 1 (most popular) or periodCount - 3 (gainers/losers) or anywhere in between. The firstRankPeriod is 0 based.
    }
    \item{floorSensitivity}{
        A factor between 0 and 1 that is used to cut off low-count items from percentage ranking. Relative only to gainers/losers by percent. Default is .25.
    }
}
\details{

GetRealTimeReport returns a Data Frame with the results from the real-time report specified. To find out valid values for the inputs of this function, run GetRealTimeConfiguration to get the current report setup. To change the real-time report setup, use SaveRealTimeConfiguration.
   
}
\value{
    Data Frame
}

\seealso{
   \code{\link{GetRealTimeConfiguration}}\cr
   \code{\link{SaveRealTimeConfiguration}}\cr
     
}

\references{
See documentation for in-depth documentation:

https://developer.omniture.com/en_US/documentation/sitecatalyst-reporting/r-realtimereportdescription

}
\examples{
\dontrun{
  
  #Minimum example - Equivalent to Overtime report
  GetRealTimeReport("keystonerandy", "instances")

   }
}
\keyword{GetRealTimeConfiguration}