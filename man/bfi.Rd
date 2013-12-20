\name{bfi}
\alias{bfi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculate baseflow indices using the Lynne-Hollick filter
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bfi(flow.ts, a, ts = "mean")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow.ts}{
%%     ~~Describe \code{flow.ts} here~~
}
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{ts}{
%%     ~~Describe \code{ts} here~~
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
function (flow.ts, a, ts = "mean") 
{
    full.flow.ts <- flow.ts[, c("Date", "Q")]
    red.flow.ts <- full.flow.ts[complete.cases(full.flow.ts[, 
        "Date"], full.flow.ts[, "Q"]), ]
    Date <- red.flow.ts[, "Date"]
    Q <- red.flow.ts[, "Q"]
    bf <- lh3(Q, a)
    bfi <- ifelse(Q == 0, 0, bf/Q)
    out <- data.frame(Date, bf, bfi)
    out <- merge(full.flow.ts, out, by = "Date", all.x = T)
    names(out) <- c("Date", "Q", "bf", "bfi")
    if (ts == "daily") {
        return(out)
    }
    else {
        if (ts == "annual") {
            a.obs <- aggregate(full.flow.ts$Q, by = list(year = strftime(full.flow.ts$Date, 
                format = "\%Y")), function(x) {
                sum(!is.na(x))
            })
            names(a.obs) <- c("year", "obs")
            a.bf <- aggregate(out[2:4], by = list(year = strftime(out$Date, 
                format = "\%Y")), mean, na.rm = T)
            out <- merge(a.obs, a.bf, by = "year", all.x = T)
            return(out)
        }
        if (ts == "mean") {
            all.days <- nrow(full.flow.ts)
            obs <- nrow(red.flow.ts)
            prop.obs = obs/all.days
            return(list(prop.obs = prop.obs, MDF = mean(out[, 
                2], na.rm = T), Q50 = median(out[, 2], na.rm = T), 
                mean.bf = mean(out[, 3], na.rm = T), mean.bfi = mean(out[, 
                  3], na.rm = T)/mean(out[, 2], na.rm = T)))
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
