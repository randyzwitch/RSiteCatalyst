\name{GetAvailableElements}
\alias{GetAvailableElements}
\title{Get Available Elements within a Report Suite}
\description{
    Get All Available Elements for a given Report Suite. This list can be used to inform other functions having an "element" parameter.
}
\usage{
    GetAvailableElements(report_suites)
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

    GetAvailableElements("keystonejowanza")
    GetAvailableElements(c("keystonejowanza", "keystonerandy", "keystonetraining"))
    }
    
    
}
\references{
Official Adobe Documentation about Elements list\cr
https://developer.omniture.com/en_US/documentation/sitecatalyst-reporting/r-elements-1
}
\keyword{elements}