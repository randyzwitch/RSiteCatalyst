\name{GetAvailableMetrics}
\alias{GetAvailableMetrics}
\title{Get Available Metrics within a Report Suite}
\description{
    Get All Available Metrics for a given Report Suite. This list can be used to inform other functions having a "metrics" parameter.
}
\usage{
    GetAvailableMetrics(report_suites)
}
\arguments{
    \item{report_suites}{
        Character vector containing one or more valid Report Suite names
    }
  
}
\details{
    This function requires having a character vector with one or more valid Report Suites specified.
}
\value{
    Data Frame
}

\examples{
\dontrun{

    GetAvailableMetrics("keystonejowanza")
    GetAvailableMetrics(c("keystonejowanza", "keystonerandy", "keystonetraining"))
    
    }
    
    
    
}
\references{
Official Adobe Documentation about Metrics list\cr
https://developer.omniture.com/en_US/documentation/sitecatalyst-reporting/r-metrics-1
}
\keyword{metrics}