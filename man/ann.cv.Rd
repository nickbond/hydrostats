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
\value{
A dataframe with one column (ann.cv).
}

\author{
Nick Bond <n.bond@latrobe.edu.au>
}

\examples{
data(Cooper)
cooper<-ts.format(Cooper)
ann.cv(Cooper)

}
