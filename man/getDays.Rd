\name{getDays}
\alias{getDays}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function to find day of the year based on dates
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getDays(year)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{year}{
%%     ~~Describe \code{year} here~~
}
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (year) 
{
    days <- c()
    d1 <- as.Date(paste(year, "-01-01", sep = ""))
    d2 <- as.Date(paste(year, "-12-31", sep = ""))
    day <- difftime(d2, d1) + 1
    days <- rbind(day, days)
    return(days)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
