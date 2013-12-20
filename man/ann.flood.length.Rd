\name{ann.flood.length}
\alias{ann.flood.length}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ann.flood.length(flow.ts, flow.threshold, ind.days = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow.ts}{
%%     ~~Describe \code{flow.ts} here~~
}
  \item{flow.threshold}{
%%     ~~Describe \code{flow.threshold} here~~
}
  \item{ind.days}{
%%     ~~Describe \code{ind.days} here~~
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
%% ~put references to the literature/web site here ~
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
function (flow.ts, flow.threshold, ind.days = 5) 
{
    high.flows <- ifelse(flow.ts[, "Q"] > flow.threshold, 1, 
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
