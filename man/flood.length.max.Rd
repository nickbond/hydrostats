\name{flood.length.max}
\alias{flood.length.max}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates the maximum flood length in a time series. 
}
\description{
Calculates the maximum flood length above a user defined threshold in a time series. Used with ddply (from the plyr package) it will return a vector of maximum annual flood lengths (see examples). 

Alternatively, the function high.spell.lengths can be used to return the length of all events above a threshold, not just the longest event in each year.}
\usage{
flood.length.max(flow.ts, threshold, ind.days = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow.ts}{
Dataframe consisting of date in column one and flow in column two. Date must be in POSIX format (see ts.format). 
}
  \item{threshold}{
A user supplied threshold for defining spells. This would typically be derived from hydraulic models or similar knowledge pertaining to a gauge site.}
  \item{ind.days}{
Periods between spells of less than ind.days (default 5) are considered to be 'in spell' for the purpose of further calculations. A value of 0 means spells 1 day apart are considered indpedendent.}
}


\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
returns a dataframe with 1 column (Maximum spell duration)
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Nick Bond}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

data(Cooper)
Cooper<-ts.format(Cooper)

flood.length.max(Cooper,50000, ind.days=5)

# Return annual maximum flood length based on calendar year using ddply (from plyr package) 
require(plyr)
Cooper$Year=format(Cooper$Date, format="\%Y")
ddply(Cooper, .(Year), flood.length.max, 50000)

# Based on hydrologic year.
Cooper<-hydro.year(Cooper)
ddply(Cooper, .(Year), flood.length.max, 50000)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
