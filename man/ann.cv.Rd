\name{ann.cv}
\alias{ann.cv}
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
\arguments{
  \item{flow.ts}{Dataframe with date and discharge data in columns named "Date" and "Q" respectively. Date must be in POSIX format (see ts.format). Missing values are ignored.
  }
}
\details{
Requires date to be in POSIX format. See ts.format
}
\value{
A dataframe with 1 column (ann.cv).
}

\author{
Nick Bond <nrb.ski@gmail.com>
}

\examples{
data(Cooper)
cooper<-ts.format(Cooper)
ann.cv(Cooper)

}
