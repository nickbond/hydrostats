\name{hydrostats}
\alias{hydrostats}
\alias{hydrostats}
\docType{package}
\title{
calculate hydrologic statistics
}
\description{
A package to calculate a range of hydrologic statistics based on daily time series data and which are widely used in hydrology and ecological applications.
}
\details{
\tabular{ll}{
Package: \tab hydrostats\cr
Type: \tab Package\cr
Version: \tab 0.2\cr
Date: \tab 2014-08-14\cr
License: \tab GPL (>= 2) \cr
}

Data must be provided as a dataframe in which the date is in POSIXct format. The function \code{\link[hydrostats:ts.format]{ts.format}} can be used to specify the Date and discharge columns in a dataframe and convert dates to POSIXct format. By default Dates are assumed to be in the first column and discharge data in the second column.

Includes several sample datasets.\cr
data(Cooper) - Flow data for Coopers Creek, Australia. Gauge 003101@Currareva \cr
data(Acheron) - Flow data for Acheron River, Australia, Gauge 405209@Taggerty

}
\author{
Nick Bond
Maintainer: Nick Bond <n.bond@griffith.edu.au>

}
\references{
%~~ Literature or other references for background information ~~
}
\keyword{ package 
~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
~~ the R documentation directory ~~
}
\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
%~~ simple examples of the most important functions ~~
}
