\name{day.dist}
\alias{day.dist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate average timing of events (doy)
}
\description{
Calculates the mean and standard deviation of the timing of annual events. Given a dataframe consisting of years in column one and a day of year (0-365 [366 for leap years]) in column two, day.dist returns the mean day of the year (doy) and standard deviation of days around the mean. 

Circular statistics are used to account for the proximity of days close to the start and the end of the year (i.e. numbers close to 0 and 365), which would notionally have a mean of approximiately 182 (see Bayliss and Jones (1993)). The mean that is returned can be interpreted as a calendar day, and data that are strongly directional will have a standard deviation close to zero.
}
\usage{
day.dist(Dates, days, years)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Dates}{
A vector of POSIX dates from which days and years are extracted. If Dates are not provided, days and years must be }

\item{days}{
A vector of days in numeric format. Not required if POSIXct dates are provided}

\item{years}{
A vector of years in numeric format. Not required if POSIXct dates are provided}
}

\value{
A dataframe with two columns.
  \item{mean.doy }{mean day of year events occur on}
  \item{sd.doy }{standard deviation indicating the spread of event timing}

}
\references{
Bayliss, A. C., Jones, R. C. (1993) Peaks-over-threshold flood database: Summary statistics and seasonality. Institute of Hydrology. Wallingford, UK.
}
\author{
Nick Bond <n.bond@griffith.edu.au>
}
\examples{
days<-c(366,1,365,1,366)
years<-c("1968","1975","1983","1990","2004")
day.dist(days=days, years=years)

days<-c(170,180,1,365,170)
day.dist(days=days, years=years)

dates<-c("1968-06-18", "1975-06-29", "1983-01-01", "1990-12-31", "2004-06-18")
dates<-as.POSIXct(dates, format = "\%Y-\%m-\%d", tz="")
day.dist(Dates=dates)

}
