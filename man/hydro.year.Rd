\name{hydro.year}
\alias{hydro.year}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determine hydrologic year for the purposes of deriving spell characteristics
}
\description{
Defines a hydrologic year to minimise the risk that defined spells are interrupted by transitions between calendar years. The function can be called by several other functions in the hydrostats package (e.g. \code{\link{high.spells}}, \code{\link{low.spells}}, \code{\link{high.spell.lengths}}.
}
\usage{
hydro.year(flow.ts, hydro.year = "hydro", year.only=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow.ts}{
Dataframe consisting of date in column one and flow in column two. Date must be in POSIX format (see ts.format).}
  \item{hydro.year}{
hydro.year="hydro" calculates the hydrologic year and returns a dataframe with an additional column indiating the hydrologic year to which each observation belongs. The hydrologic year is defined as starting in the first month of the average driest 6 month period across all years. This maximises the likelihood that low-flow and high-flow spells will be contained within a rolling 12 month period.

Other options may be added in the future.

}
	\item{year.only}{
logical. If FALSE (default), a column indicating the hydrologic year of each record is added to the original data.frame. If TRUE, a vector indicating the hydrologic year of each record is returned. 
	}
}


\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
If year.only=FALSE (default), the function returns the original dataframe with an added column "year" indicating the hydrologic year to which each case belongs. Otherwise, if year.only=TRUE, a vector of hydrologic years is returned.
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
\code{\link{high.spells}}, \code{\link{low.spells}}.
}
\examples{
data(Cooper)
Cooper<-ts.format(Cooper)

head(hydro.year(Cooper))
head(hydro.year(Cooper, year.only=TRUE))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

