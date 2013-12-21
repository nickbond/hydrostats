\name{daily.cv}
\alias{daily.cv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Caculate daily cofficient of variation
}
\description{
This function takes a daily time series and returns the coefficient of variation of daily flows expressed as a percentage. 

i.e. (sd/mean)*100

Missing values are ignored.
}
\usage{
daily.cv(flow.ts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow.ts}{Dataframe consisting of date in column one and flow in column two. Date must be in POSIX format (see ts.format).}
  {
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
Nick Bond}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(cooper)
cooper<-ts.format(cooper)

daily.cv(Cooper)

## The function is currently defined as
function (flow.ts) 
{
    daily.cv <- (sd(flow.ts$Q, na.rm = T)/mean(flow.ts$Q, na.rm = T)) * 
        100
    return(list(daily.cv = daily.cv))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
