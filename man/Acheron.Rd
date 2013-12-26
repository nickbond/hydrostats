\name{Acheron}
\docType{data}
\alias{Acheron}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Acheron River flow data
}
\description{
Daily discharge time series in megalitres per day (ML/day) for Acheron River @ Taggerty (Gauge No. 405209), Victoria, Australia, from 1971-2000.

}
\usage{
data(Acheron)
}
%- maybe also 'usage' for other objects documented here.

\format{
A data frame with 10944 observations (from 1971-2000) on 2 variables.

[,1] date (format dd/mm/yy)
[,2] discharge (ML/day)  
}

\source{
Approval yet to be granted.

Department of Sustainability and Environment, Victorian Government, Australia.

}
\examples{
data(Acheron)
Acheron<-ts.format(Acheron)
plot(Acheron[[1]],Acheron[[2]],type="l", xlab="Date",ylab="Discharge (ML/day)")


}
