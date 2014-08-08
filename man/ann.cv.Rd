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
A dataframe with 1 column (ann.cv).
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
