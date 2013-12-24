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
Cooper
}
%- maybe also 'usage' for other objects documented here.

\format{
A data frame with 7670 observations (from 1967-1987) on 2 variables.

[,1] date (format dd/mm/yy)
[,2] discharge (ML/day)  
}

\source{
Approval yet to be granted.

Department of Environment and Resource Management, Qld Government, Australia.

%From another paper here's what the Qld government say about using their data. The Department of Natural Resources and Mines gives no %warranty in relation to the data (including accuracy, reliability, completeness or suitability) and %accepts no liability (including without limitation, liability in negligence) for any loss, damage or %costs (including consequential damage) relating to any use of the data. It is acknowledged that all %products and images generated using data from the State of Queensland are based on or contain data %provided by the Department of Natural Resources and Mines, Queensland which gives no warranty in %relation to the data (including accuracy, reliability, completeness or suitability) and accepts no %liability (including without limitation, liability in negligence) for any loss, damage or cost (including consequential damage) relating to any use of the data.
}
\examples{
data(Cooper)
Cooper<-ts.format(Cooper)
plot(Cooper[[1]],Cooper[[2]],type="l", xlab="Date",ylab="Discharge (ML/day)")


}
