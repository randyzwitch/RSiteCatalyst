\name{GetSegments}
\alias{GetSegments}
\title{Get Segments Defined within a Report Suite}
\description{
    Get Segments defined within a Report Suite. Segments include Data Warehouse segments, pre-defined segments, and user-defined segments
}
\usage{
    GetSegments(report_suites)
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
    
    GetSegments("keystonejowanza")
    GetSegments(c("keystonejowanza", "keystonerandy", "keystonetraining"))
}    
    
    
}
\keyword{segments}