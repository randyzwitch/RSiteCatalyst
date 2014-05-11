\name{SaveRealTimeSettings}
\alias{SaveRealTimeSettings}
\title{Save Configuration for Real-Time Report}
\usage{
SaveRealTimeSettings(reportsuite.ids = "")
}
\arguments{
  \item{reportsuite.ids}{Report Suite ID}
}
\value{
Message returned to console
}
\description{
Selects the metrics and elements (dimensions) on which you
want real time reports enabled. Realtime configuration
changes take 15 minutes to be reflected in reports.
}
\details{
SaveRealTimeSettings should be called each time you want to
modify the structure of your real-time reports. If you are
unsure of your current setup of your real-time reports, use
GetRealTimeSettings to find out your current setup.

Changes can take up to 15 minutes to be reflected.
}
\examples{
\dontrun{

   }
}
\seealso{
\code{\link{GetRealTimeSettings}} \cr
}
\keyword{SaveRealTimeSettings}

