\name{getDays}
\alias{getDays}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to determine the number of days 365 or 366 in a given year.
}
\description{
A helper function for circular statistic functions. Determines the number of days in any given year (i.e. 365 or 366)}
\usage{
getDays(year)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{year}{
A vector of years in numeric format}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
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
Nick Bond}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
years<-c("1968","1975","1983","1990","2004")
getDays(years)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
