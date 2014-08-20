\name{monthly.cv}
\alias{monthly.cv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate monthly coefficient of variation
}
\description{
This function takes a daily time series and returns the coefficient of variation of mean monthly flow expressed as a percentage.}
\usage{
monthly.cv(flow.ts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow.ts}{Dataframe with date and discharge data in columns named "Date" and "Q" respectively. Date must be in POSIX format (see ts.format). Missing values are ignored.}
}

\value{
a dataframe with 1 column (monthly.cv)
}

\author{
Nick Bond <n.bond@griffith.edu.au>
}

\examples{
data(Cooper)
cooper<-ts.format(Cooper)
ann.cv(Cooper)
}
