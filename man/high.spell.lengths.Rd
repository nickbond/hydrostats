\name{high.spell.lengths}
\alias{high.spell.lengths}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculate the length of high flow spells
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
high.spell.lengths(flow.ts, quant = 0.9, user.threshold = F, defined.threshold, ind.days = 5, ignore.zeros = T, ctf.threshold = 0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow.ts}{
%%     ~~Describe \code{flow.ts} here~~
}
  \item{quant}{
%%     ~~Describe \code{quant} here~~
}
  \item{user.threshold}{
%%     ~~Describe \code{user.threshold} here~~
}
  \item{defined.threshold}{
%%     ~~Describe \code{defined.threshold} here~~
}
  \item{ind.days}{
%%     ~~Describe \code{ind.days} here~~
}
  \item{ignore.zeros}{
%%     ~~Describe \code{ignore.zeros} here~~
}
  \item{ctf.threshold}{
%%     ~~Describe \code{ctf.threshold} here~~
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
function (flow.ts, quant = 0.9, user.threshold = F, defined.threshold, 
    ind.days = 5, ignore.zeros = T, ctf.threshold = 0.1) 
{
    if (ncol(flow.ts) > 2) {
        record.year <- flow.ts[, "Year"]
    }
    else {
        record.year <- strftime(flow.ts[[1]], format = "\%Y")
        flow.ts <- data.frame(flow.ts, year = record.year)
    }
    n.years <- nlevels(as.factor(record.year))
    if (user.threshold == T) {
        flow.threshold <- defined.threshold
    }
    else {
        if (ignore.zeros == T) {
            flow.threshold <- quantile(flow.ts[which(flow.ts[[2]] > 
                ctf.threshold), 2], quant, na.rm = T)
            names(flow.threshold) <- NULL
        }
        else {
            flow.threshold <- quantile(flow.ts[, 2], quant, na.rm = T)
            names(flow.threshold) <- NULL
        }
    }
    high.flows <- ifelse(flow.ts[, 2] > flow.threshold, 1, 0)
    if (ind.days > 0) {
        high.flow.runs <- rle(high.flows)
        too.short <- which(high.flow.runs$lengths < ind.days & 
            high.flow.runs$values == 0)
        spell.factor <- rep(seq_along(high.flow.runs$lengths), 
            times = high.flow.runs$lengths)
        add.to.spell <- which(spell.factor \%in\% too.short)
        high.flows[add.to.spell] <- 1
    }
    high.flow.runs <- rle(high.flows)
    good.high.flow.runs <- which(!is.na(high.flow.runs$values))
    flow.runs.values <- high.flow.runs$values[good.high.flow.runs]
    flow.runs.lengths <- high.flow.runs$lengths[good.high.flow.runs]
    spell.lengths <- cbind(flow.runs.values, flow.runs.lengths)
    flood.spell.lengths <- subset(spell.lengths, flow.runs.values == 
        1, select = flow.runs.lengths)
    return(data.frame(flood.duration = flood.spell.lengths))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
