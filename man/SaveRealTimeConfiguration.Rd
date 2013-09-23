\name{SaveRealTimeConfiguration}
\alias{SaveRealTimeConfiguration}
\title{
    Save Configuration for Real-Time Report
}
\description{
Selects the metrics and elements (dimensions) on which you want real time reports enabled.
Realtime configuration changes take 15 minutes to be reflected in reports.
   
}
\usage{
    SaveRealTimeConfiguration(report_suite, metric1, elements1, metric2, 
    elements2, metric3, elements3)
}
\arguments{
    \item{report_suite}{
        Report Suite ID
    }
     \item{metric1}{
        Metric to use in real-time report slot 1
    }
    \item{elements1}{
        Up to three elements to use for real-time report slot 1
    }
     \item{metric2}{
        Metric to use in real-time report slot 2
    }    
    \item{elements2}{
        Up to three elements to use for real-time report slot 2
    }
     \item{metric3}{
        Metric to use in real-time report slot 3
    }
    \item{elements3}{
        Up to three elements to use for real-time report slot 3
    }

}
\details{

SaveRealTimeConfiguration should be called each time you want to modify the structure of your real-time reports. If you are unsure of your current setup of your real-time reports, use
GetRealTimeConfiguration to find out your current setup.

Changes can take up to 15 minutes to be reflected.
   
}
\value{
    Message returned to console
}

\seealso{
   \code{\link{GetRealTimeConfiguration}} \cr
     
}
\examples{
\dontrun{

SaveRealTimeConfiguration("keystonerandy", 
metric1 = "instances", 
elements1 = c("page", "referringdomain", "sitesection"),
metric2 = "revenue",
elements2 = c("referringdomain", "sitesection")
)
  

   }
}
\keyword{SaveRealTimeConfiguration}