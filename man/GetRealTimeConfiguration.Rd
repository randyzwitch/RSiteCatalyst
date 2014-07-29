\name{GetRealTimeConfiguration}
\alias{GetRealTimeConfiguration}
\title{Get Current Configuration for Real-Time Reports}
\usage{
GetRealTimeConfiguration(report_suite)
}
\arguments{
  \item{report_suite}{Report Suite ID}
}
\value{
Data Frame
}
\description{
Get current configuration for real-time reports
}
\details{
GetRealTimeConfiguration returns a Data Frame with the
current set up of real-time reports within the Adobe
Analytics Real-Time API. To change configuration settings,
use SaveRealTimeConfiguration function.
}
\examples{
\dontrun{

  GetRealTimeConfiguration("keystonerandy")

   }
}
\seealso{
\code{\link{SaveRealTimeConfiguration}} \cr
}
\keyword{GetRealTimeConfiguration}

