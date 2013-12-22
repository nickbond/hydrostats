\name{hydro.year}
\alias{hydro.year}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determine hydrologic year for calculating indices
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
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
data(Cooper)
Cooper<-ts.format(Cooper)
Cooper<-hydro.year(Cooper)


## The function is currently defined as
function (flow.ts, hydro.year = "hydro", alt.start.date) 
{
    begin <- head(flow.ts[[1]], 1)
    finish <- tail(flow.ts[[1]], 1)
 
 if (hydro.year == "hydro") {
        month.runs <- c()
        month.means <- aggregate(flow.ts[[2]], by = list(month = strftime(flow.ts[[1]], 
            format = "\%m")), sum, na.rm = T)
        month.runs[1] <- sum(month.means[1:6, 2])
        month.runs[2] <- sum(month.means[2:7, 2])
        month.runs[3] <- sum(month.means[3:8, 2])
        month.runs[4] <- sum(month.means[4:9, 2])
        month.runs[5] <- sum(month.means[5:10, 2])
        month.runs[6] <- sum(month.means[6:11, 2])
        month.runs[7] <- sum(month.means[7:12, 2])
        month.runs[8] <- sum(month.means[c(8:12, 1), 2])
        month.runs[9] <- sum(month.means[c(9:12, 1:2), 2])
        month.runs[10] <- sum(month.means[c(10:12, 1:3), 2])
        month.runs[11] <- sum(month.means[c(11:12, 1:4), 2])
        month.runs[12] <- sum(month.means[c(12, 1:5), 2])
        alt.month <- which.min(month.runs)
        year <- c()
        year <- ifelse(as.numeric(strftime(flow.ts[[1]], format = "\%m")) < 
            alt.month, as.numeric(strftime(flow.ts[[1]], format = "\%Y")) - 
            1, as.numeric(strftime(flow.ts[[1]], format = "\%Y")))
    }
    flow.ts <- cbind(flow.ts, Year = year)
    return(flow.ts)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
