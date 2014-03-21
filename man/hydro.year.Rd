\name{hydro.year}
\alias{hydro.year}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determine hydrologic year for calculating indices
}
\description{
Defines a hydrologic year to minimise the risk that defined spells are interrupted by transitions between calendar years.
}
\usage{
hydro.year(flow.ts, hydro.year = "hydro")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow.ts}{
Dataframe consisting of date in column one and flow in column two. Date must be in POSIX format (see ts.format).}
  \item{hydro.year}{
hydro.year="hydro" calculates the hydrologic year and returns a dataframe with an additional column indiating the hydrologic year to which each observation belongs. The hydrologic year is defined as starting in the first month of the average driest 6 month period across all years. This maximises the likelihood that low-flow and high-flow spells will be contained within a rolling 12 month period.

Other options may be added in the future.

%%     ~~Describe \code{alt.start.date} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
Returns the original dataframe with an added column "Year" indicating the hydrologic year to which each case belongs.
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
Cooper<-hydro.year(Cooper)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

