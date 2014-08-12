\name{seasonality}
\alias{seasonality}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate seasonality of discharge
}
\description{
Returns statistics decribing seasonal variation in runoff. }
\usage{
seasonality(flow.ts, monthly.range = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow.ts}{
Dataframe with date and discharge data in columns named "Date" and "Q" respectively. Date must be in POSIX format (see ts.format). Missing values are ignored.
}
  \item{monthly.range}{
logical. If FALSE (default), the function returns the percentage of runoff occurring during the average driest 6 month period (as defined across all years). If true, additional statistics describing cumulative average monthly flows, the range between the runoff in the wettest and driest months, and the average number of months between the wettest and driest periods of runoff.}
}

\value{
If monthly.range=FALSE (default) the function returns a dataframe with 1 column describing the percentage of annual runoff delivered during the average driest 6 month period.

If monthly.range=TRUE, the function returns a list with the following elements:

\item{seasonality}{The percentage of annual runoff delivered during the driest 6 months}
\item{monthly.means}{Average flow in each month of the year}
\item{avg.ann.month.range}{Average difference between the monthly minimum and maximum}
\item{max.min.time.dif}{Average number of months between the highest and lowest monthly runoff}
}

\author{
Nick Bond <n.bond@griffith.edu.au>
}

\examples{

data(Cooper)
Cooper<-ts.format(Cooper)

seasonality(Cooper, monthly.range=TRUE)

}
