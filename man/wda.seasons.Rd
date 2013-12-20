\name{wda.seasons}
\alias{wda.seasons}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Divide series into wet, dry and average years for subsequent analysis
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
wda.seasons(flow.ts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow.ts}{
%%     ~~Describe \code{flow.ts} here~~
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
function (flow.ts) 
{
    flow.ts$Date <- ymd(flow.ts[, "Date"])
    flow.ts$year <- year(flow.ts[, "Date"])
    flow.ts.mdf <- ddply(flow.ts, .(year), summarize, MDF = mean(Q))
    flow.ts.quants <- quantile(flow.ts.mdf$MDF, c(0.3, 0.7))
    flow.ts.mdf$season <- ifelse(flow.ts.mdf$MDF < flow.ts.quants[1], 
        "dry", ifelse(flow.ts.mdf$MDF >= flow.ts.quants[2], "wet", 
            "average"))
    flow.ts$season <- flow.ts.mdf$season[match(flow.ts$year, 
        flow.ts.mdf$year)]
    return(flow.ts)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
