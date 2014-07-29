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
Dataframe consisting of date in column one and flow in column two. Date must be in POSIX format (see ts.format). Missing values are ignored.
}
  \item{monthly.range}{
logical. If FALSE (default), the function returns the percentage of runoff occurring during the average driest 6 month period (as defined across all years). If true, additional statistics describing cumulative average monthly flows, the range between the runoff in the wettest and driest months, and the average number of months between the wettest and driest periods of runoff.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
monthly.range=FALSE\cr
a dataframe with 1 column describing the percentage of annual runoff delivered during the average driest 6 month period.

monthly.range=TRUE\cr
a list with the following elements
\item{Seasonality }{Description of 'comp1'}
\item{average monthly flow }{Description of 'comp2'}
\item{average annual monthly range }{Description of 'comp2'}
\item{average number of months between the highest and lowest monthly runoff }{Description of 'comp2'}
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

seasonality(Cooper, monthly.range=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
