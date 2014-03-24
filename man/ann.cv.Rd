\name{ann.cv}
\alias{ann.cv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate interannual coefficient of variation}
\description{
This function takes a daily time series and returns the coefficient of variation of mean annual flow expressed as a percentage. 

i.e. (sd/mean)*100

Missing values are ignored.
}
\usage{
ann.cv(flow.ts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow.ts}{Dataframe consisting of date in column one and flow in column two. Date must be in POSIX format (see ts.format). Missing values are ignored.
  }
}
\details{
Requires date to be in POSIX format. See ts.format
}
\value{
A list of length 1
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
Nick Bond <nrb.ski@gmail.com>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(Cooper)
cooper<-ts.format(Cooper)
ann.cv(Cooper)


}
%## The function is currently defined as
%function (flow.ts) 
%{
%    flow.ts$year <- strftime(flow.ts$Date, format = "\%Y")
%    ann.stats <- ddply(flow.ts, "year", summarize, mean = mean(Q, 
%        na.rm = T), sd = sd(Q, na.rm = T))
%    out <- sd(ann.stats$mean, na.rm = T)/mean(ann.stats$mean, 
%        na.rm = T) * 100
%    return(list(ann.cv = out))
%  }
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ coefficient of variation }
%\keyword{ interannual variability }% __ONLY ONE__ keyword per line
