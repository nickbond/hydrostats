\name{day.dist}
\alias{day.dist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate average timing of events (doy)
}
\description{
A helper function for calculating the mean and standard deviation in the timing of annual events. Given a dataframe consisting of years in column one and a day of year (0-365 [366 for leap years]) in column two, day.dist returns the mean day of year (doy) and standard deviation of the mean day of year. Circular statistics are used to account for the proximity of days close to the start and the end of the year (i.e. numbers close to 0 and 365.
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
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Bayliss, A. C., Jones, R. C. (1993) Peaks-over-threshold flood database: Summary statistics and seasonality. Institute of Hydrology.
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (years, days) 
{
    year.lengths <- sapply(years, getDays)
    theta <- c()
    theta <- days * (2 * pi/year.lengths)
    x <- sum(sin(theta))
    y <- sum(cos(theta))
    m.theta <- atan2(x, y)
    m.deg <- (180 * m.theta/pi)
    if (m.deg < 0) 
        m.deg <- m.deg + 360
    R <- sqrt((x^2) + (y^2))
    Rm <- R/length(theta)
    rad.sd <- 1 - Rm
    return(cbind(m.deg, rad.sd))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
