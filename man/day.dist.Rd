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
day.dist(years, days)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{years}{
%%     ~~Describe \code{years} here~~
}
  \item{days}{
%%     ~~Describe \code{days} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a list of length 2
%%  If it is a LIST, use
  \item{mean.doy }{mean day of year events occur on}
  \item{sd.doy }{standard deviation indicating the spread of event timing}
%% ...
}
\references{
Bayliss, A. C., Jones, R. C. (1993) Peaks-over-threshold flood database: Summary statistics and seasonality. Institute of Hydrology. Wallingford, UK.
}
\author{
Nick Bond
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
years<-c("1968","1975","1983","1990","2004")
days<-c(366,1,365,1,366)
day.dist(years,days)
days<-c(170,180,1,365,170)
day.dist(years,days)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
