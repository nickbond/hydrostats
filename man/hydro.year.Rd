\name{hydro.year}
\alias{hydro.year}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
determine hydrologic year for calculating indices
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hydro.year(flow.ts, hydro.year = "hydro", alt.start.date)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow.ts}{
%%     ~~Describe \code{flow.ts} here~~
}
  \item{hydro.year}{
%%     ~~Describe \code{hydro.year} here~~
}
  \item{alt.start.date}{
%%     ~~Describe \code{alt.start.date} here~~
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
function (flow.ts, hydro.year = "hydro", alt.start.date) 
{
    begin <- head(flow.ts[[1]], 1)
    finish <- tail(flow.ts[[1]], 1)
    if (hydro.year == "presample") {
        date.dif <- round(difftime(alt.start.date, paste(strftime(begin, 
            format = "\%Y"), 1, 1, sep = "-")))
        new.begin <- as.Date(begin) - date.dif
        new.begin <- seq.Date(new.begin, by = "year", len = 2)[2]
        new.Date <- seq.Date(new.begin, by = "day", len = length(flow.ts[[1]]))
    }
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
