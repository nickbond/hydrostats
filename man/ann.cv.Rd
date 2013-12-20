\name{ann.cv}
\alias{ann.cv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculate interannual coefficient of variation}
\description{
test
}
\usage{
ann.cv(flow.ts)
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
    flow.ts$year <- strftime(flow.ts$Date, format = "\%Y")
    ann.stats <- ddply(flow.ts, "year", summarize, mean = mean(Q, 
        na.rm = T), sd = sd(Q, na.rm = T))
    out <- sd(ann.stats$mean, na.rm = T)/mean(ann.stats$mean, 
        na.rm = T) * 100
    return(list(ann.cv = out))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
