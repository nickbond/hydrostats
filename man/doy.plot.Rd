\name{doy.plot}
\alias{doy.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
create day of year plot with ggplot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
doy.plot(flow.ts.sum, title = "", ylim = c(0.1, 10000))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow.ts.sum}{
%%     ~~Describe \code{flow.ts.sum} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
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
function (flow.ts.sum, title = "", ylim = c(0.1, 10000)) 
{
    plot <- ggplot(flow.ts.sum, aes(x = all.dates, group = 1))
    plot + scale_x_date(breaks = date_breaks("1 month"), labels = date_format("\%b")) + 
        geom_ribbon(aes(ymin = Q.05, ymax = Q.95), fill = "gray80") + 
        geom_ribbon(aes(ymin = Q.25, ymax = Q.75), fill = "gray60") + 
        coord_cartesian(ylim = ylim) + geom_line(aes(y = Q.50), 
        colour = "black") + labs(title = title, x = "Date", y = expression("Daily Discharge (Ml.day"^-1 * 
        ")"), vjust = 0.5) + theme_bw()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
