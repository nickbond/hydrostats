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
  \item{flow.ts}{Dataframe with date and discharge data in columns named "Date" and "Q" respectively. Date must be in POSIX format (see ts.format).}
  {
%%     ~~Describe \code{flow.ts} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A dataframe with 1 column (daily CV).
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
data(Cooper)
Cooper<-ts.format(Cooper)

daily.cv(Cooper)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
