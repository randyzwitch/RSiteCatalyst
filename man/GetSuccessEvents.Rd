\name{GetSuccessEvents}
\alias{GetSuccessEvents}
\title{Get Success Events Associated with a Report Suite}
\description{
    Get Success Events associated with one or more Report Suites.
}
\usage{
    GetSuccessEvents("keystonejowanza")
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
    GetSuccessEvents("keystonejowanza")
    GetSuccessEvents(c("keystonejowanza", "keystonerandy", "keystonetraining"))
    
    
    
}
\keyword{events}