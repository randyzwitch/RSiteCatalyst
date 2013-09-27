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
        metrics
    }
    \item{elements}{
        elements
    }
    \item{periodMinutes}{
        periodMinutes
    }
    \item{periodCount}{
        periodCount
    }
    \item{periodOffset}{
        periodOffset
    }
    \item{algorithm}{
        algorithm
    }
    \item{algorithmArgument}{
        algorithmArgument
    }
    \item{firstRankPeriod}{
        firstRankPeriod
    }
    \item{floorSensitivity}{
        floorSensitivity
    }
}
\details{

GetRealTimeReport returns a Data Frame with the results from the real-time report specified. To find out valid values for the inputs of this function, run GetRealTimeConfiguration to get the current report setup. To change the real-time report setup, use SaveRealTimeConfiguration.


   
}
\value{
    Data Frame
}

\seealso{
   \code{\link{GetRealTimeConfiguration}} \cr
   \code{\link{SaveRealTimeConfiguration}} \cr
     
}
\examples{
\dontrun{
  
  #Minimum example - Equivalent to Overtime report
  GetRealTimeReport("keystonerandy", "instances")

   }
}
\keyword{GetRealTimeConfiguration}