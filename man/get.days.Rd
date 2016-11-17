\name{get.days}
\alias{get.days}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to determine the number of days 365 or 366 in a given year.
}
\description{
A helper function for circular statistic functions. Determines the number of days in any given year (i.e. 365 or 366)}
\usage{
get.days(year)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{year}{
A vector of years in numeric format}
}

\value{
A vector containing the number of days in each year in the input vector
}

\author{
Nick Bond <n.bond@latrobe.edu.au>
}

\examples{
years<-c("1968","1975","1983","1990","2004")
get.days(years)

}