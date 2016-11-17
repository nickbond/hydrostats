\name{daily.cv}
\alias{daily.cv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate daily cofficient of variation
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
  \item{flow.ts}{Dataframe with date and discharge data in columns named "Date" and "Q" respectively. Date must be in POSIX format (see ts.format)}
}
\value{
A dataframe with one column (daily.cv).
}

\author{
Nick Bond <n.bond@latrobe.edu.au>
}

\examples{
data(Cooper)
Cooper<-ts.format(Cooper)

daily.cv(Cooper)

}
