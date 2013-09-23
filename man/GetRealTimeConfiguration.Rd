\name{GetRealTimeConfiguration}
\alias{GetRealTimeConfiguration}
\title{
    Get Current Configuration for Real-Time Reports
}
\description{
Get current configuration for real-time reports
   
}
\usage{
    GetRealTimeConfiguration(report_suite)
}
\arguments{
    \item{report_suite}{
        Report Suite ID
    }

}
\details{

GetRealTimeConfiguration returns a Data Frame with the current set up of real-time reports within the Adobe Analytics Real-Time API. To change configuration settings, use SaveRealTimeConfiguration function.


   
}
\value{
    Data Frame
}

\seealso{
   \code{\link{SaveRealTimeConfiguration}} \cr
     
}
\examples{
\dontrun{
  
  GetRealTimeConfiguration("keystonerandy")

   }
}
\keyword{GetRealTimeConfiguration}