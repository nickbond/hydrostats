\name{daily.reshape}
\alias{daily.reshape}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reshape daily data for plotting day of year (doy) plots in ggplot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
daily.reshape(flow.ts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow.ts}{
%%     ~~Describe \code{flow.ts} here~~
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
function (flow.ts) 
{
    flow.ts$Date <- as.Date(flow.ts$Date)
    flow.ts <- data.frame(flow.ts, day = as.character(strftime(flow.ts$Date, 
        format = "\%j")), month = strftime(flow.ts$Date, format = "\%b"), 
        year = as.numeric(strftime(flow.ts$Date, format = "\%Y")))
    flow.ts$month <- factor(flow.ts$month, levels = c("Jan", 
        "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
        "Oct", "Nov", "Dec"), ordered = T)
    flow.ts.melt <- melt(flow.ts, id.vars = c("Date", "day", 
        "month", "year"), measure.vars = c("Q"))
    flow.ts.sum <- cast(flow.ts.melt, day ~ variable, subset = variable == 
        "Q", quantile, c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = T)
    names(flow.ts.sum) <- c("day", "Q.05", "Q.25", "Q.50", "Q.75", 
        "Q.95")
    flow.ts.sum <- na.omit(flow.ts.sum)
    flow.ts.sum <- data.frame(all.dates = seq.Date(as.Date("2008-01-01"), 
        as.Date("2008-12-31"), by = 1), flow.ts.sum)
    return(flow.ts.sum)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
