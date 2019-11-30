\name{hydrostats}
\alias{hydrostats}
\alias{hydrostats}
\docType{package}
\title{
calculate hydrologic statistics
}
\description{
Calculate a range of hydrologic statistics based on daily time series data and which are widely used in hydrology and ecological applications.
}
\details{
\tabular{ll}{
Package: \tab hydrostats\cr
Type: \tab Package\cr
Version: \tab 0.2.4\cr
Date: \tab 2015-10-16\cr
License: \tab GPL (>= 2) \cr
}

Data must be provided as a dataframe in which the date is in POSIXct format. The function \code{\link[hydrostats:ts.format]{ts.format}} can be used to specify the Date and discharge columns (named Date and Q respectively) in a dataframe, and convert dates to POSIXct format. The date and discharge data must be in columns labelled "Date" and "Q" for the functions to work.

Includes several sample datasets.\cr
data(Cooper) - Flow data for Coopers Creek, Australia. Gauge 003101@Currareva \cr
data(Acheron) - Flow data for Acheron River, Australia, Gauge 405209@Taggerty

}
\author{
Nick Bond
Maintainer: Nick Bond <n.bond@latrobe.edu.au>
}

\examples{
data(Acheron)
Acheron<-ts.format(Acheron)
with(Acheron, plot(Q~Date))

high.spell.lengths(Acheron, threshold=50000)


}
