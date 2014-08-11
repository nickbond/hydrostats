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
Dataframe with date and discharge data in columns named "Date" and "Q" respectively. Date must be in POSIX format (see ts.format). 
}
  \item{threshold}{
A user supplied threshold for defining spells. This would typically be derived from hydraulic models or similar knowledge pertaining to a gauge site.}
  \item{ind.days}{
Periods between spells of less than ind.days (default 5) are considered to be 'in spell' for the purpose of further calculations. A value of 0 means spells 1 day apart are considered indpedendent.}
}

\value{
returns a dataframe with 1 column (Maximum spell duration)
}
\author{
Nick Bond <n.bond@griffith.edu.au>
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
