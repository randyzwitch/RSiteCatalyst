\name{GetEVars}
\alias{GetEVars}
\title{Get Commerce Variables (eVars) Associated with a Report Suite}
\description{
    Get Commerce Variables (eVars) Associated with a Report Suite.
}
\usage{
    GetEVars(report_suites)
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
    
    GetEVars("keystonejowanza")
    GetEVars(c("keystonejowanza", "keystonerandy", "keystonetraining"))
    
    }
    
}
\keyword{eVar}