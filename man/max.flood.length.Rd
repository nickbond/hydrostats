\name{max.flood.length}
\alias{max.flood.length}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates the maximum flood length in a time series. 
}
\description{
Calculates the maximum flood length above a user defined threshold in a time series. Used with ddply (from the plyr package) it will return a vector of maximum annual flood lengths (see examples). 

Alternatively, the function high.spell.lengths can be used to return the length of all events above a threshold, not just the longest event in each year.}
\usage{
max.flood.length(flow.ts, flow.threshold, ind.days = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow.ts}{
Dataframe consisting of date in column one and flow in column two. Date must be in POSIX format (see ts.format). 
}
  \item{flow.threshold}{
A user supplied threshold for defining spells. This would typically be derived from hydraulic models or similar knowledge pertaining to a gauge site.}
  \item{ind.days}{
Periods between spells of less than ind.days (default 5) are considered to be 'in spell' for the purpose of further calculations. A value of 0 means spells 1 day apart are considered indpedendent.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
returns a vector of length 1. Maximum spell duration.
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

max.flood.length(Cooper,50000, ind.days=5)

# Return annual maximum flood length based on calendar year\cr# using ddply (from plyr package) 
ddply(Cooper, .(Year=strptime(Cooper$Date,format="\%Y")),max.flood.length,50000)

# Based on hydrologic year.
Cooper<-hydro.year(Cooper)
ddply(Cooper, .(Year),max.flood.length,50000)


## The function is currently defined as
function (flow.ts, flow.threshold, ind.days = 5) 
{
    high.flows <- ifelse(flow.ts[, 2] > flow.threshold, 1, 
        0)
    high.flow.runs <- rle(high.flows)
    too.short <- which(high.flow.runs$lengths < ind.days & high.flow.runs$values == 
        0)
    spell.factor <- rep(seq_along(high.flow.runs$lengths), times = high.flow.runs$lengths)
    add.to.spell <- which(spell.factor \%in\% too.short)
    high.flows[add.to.spell] <- 1
    high.flow.runs <- rle(high.flows)
    if (length(which(high.flow.runs$values == 1)) == 0) {
        max.duration <- 0
    }
    else {
        max.duration <- max(high.flow.runs$lengths[which(high.flow.runs$values == 
            1)], na.rm = T)
    }
    max.duration <- data.frame(max.duration = max.duration)
    return(max.duration)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
