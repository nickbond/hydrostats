\name{Cooper}
\docType{data}
\alias{Cooper}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cooper Creek flow data
}
\description{
Daily discharge time series in megalitres per day (ML/day) for Coopers Creek @ Currareva (Gauge No. 003101), Qld, Australia, from 1967-1987.

}
\usage{
data(Cooper)
}
%- maybe also 'usage' for other objects documented here.

\format{
A data frame with 7670 observations (from 1967-1987) on 2 variables.

[,1] date (format dd/mm/yy)
[,2] discharge (ML/day)  
}

\source{
Data provided by the State of Queensland, Department of Natural Resources and Mines under creative commons licence agreement. Details available at http://watermonitoring.dnrm.qld.gov.au/wini/copyright.htm  

}
\examples{
data(Cooper)
Cooper<-ts.format(Cooper)
plot(Cooper[[1]],Cooper[[2]],type="l", xlab="Date",ylab="Discharge (ML/day)")


}
