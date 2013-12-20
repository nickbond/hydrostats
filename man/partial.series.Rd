\name{partial.series}
\alias{partial.series}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Derive partial and annual flood series
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
partial.series(flow.ts, ari = 2, ind.days = 7, duration = T, plot = F, volume = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow.ts}{
%%     ~~Describe \code{flow.ts} here~~
}
  \item{ari}{
%%     ~~Describe \code{ari} here~~
}
  \item{ind.days}{
%%     ~~Describe \code{ind.days} here~~
}
  \item{duration}{
%%     ~~Describe \code{duration} here~~
}
  \item{plot}{
%%     ~~Describe \code{plot} here~~
}
  \item{volume}{
%%     ~~Describe \code{volume} here~~
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
function (flow.ts, ari = 2, ind.days = 7, duration = T, plot = F, 
    volume = T) 
{
    n.years <- nlevels(as.factor(strftime(flow.ts$Date, format = "\%Y")))
    n.events <- ceiling(n.years/ari)
    p.series <- vector("list", n.events)
    rising <- flow.ts[2:nrow(flow.ts), 2] - flow.ts[1:nrow(flow.ts) - 
        1, 2]
    falling <- flow.ts[3:nrow(flow.ts), 2] - flow.ts[2:nrow(flow.ts) - 
        2, 2]
    peak.search <- data.frame(flow.ts, rising = c(NA, rising), 
        falling = c(falling, NA, NA))
    peaks <- flow.ts[which(peak.search[, "rising"] > 0 & peak.search[, 
        "falling"] < 0), ]
    peaks.ord <- peaks[order(peaks[, 2], decreasing = T), ]
    p.series[[1]] <- data.frame(peaks.ord[1, ])
    for (i in 2:n.events) {
        dif.time.test <- difftime(peaks.ord$Date[1], peaks.ord$Date)
        peaks.ord <- peaks.ord[which(abs(dif.time.test) > (ind.days * 
            24 * 60 * 60)), ]
        p.series[[i]] <- data.frame(peaks.ord[1, ])
        if (is.na(peaks.ord[1, 2]) == T) 
            NA
    }
    p.series <- do.call("rbind", p.series)
    flow.threshold <- tail(p.series[, 2], 1)
    if (plot == TRUE) {
        plot(flow.ts[, 2], type = "l")
        points(row.names(p.series), flow.ts[row.names(p.series), 
            2], col = "red")
        abline(h = (tail(p.series[2], 1) - 1))
    }
    if (duration == TRUE) {
        high.flows <- ifelse(flow.ts[, 2] >= flow.threshold, 
            1, 0)
        high.flow.runs <- rle(high.flows)
        avg.duration <- mean(high.flow.runs$lengths[which(high.flow.runs$values == 
            1)], na.rm = T)
        max.duration <- max(high.flow.runs$lengths[which(high.flow.runs$values == 
            1)], na.rm = T)
    }
    if (volume == TRUE) {
        spell.factor <- rep(seq_along(high.flow.runs$lengths), 
            times = high.flow.runs$lengths)
        spells <- split(flow.ts[[2]], spell.factor)
        spell.volumes <- flow.ts[[2]]
        spell.volumes <- sapply(spells, sum)
        spell.volumes <- spell.volumes[which(high.flow.runs$values == 
            1)]
        return(list(p.series = p.series, n.events = n.events, 
            flow.threshold = flow.threshold, avg.duration = avg.duration, 
            max.duration = max.duration, md.spell.volume = median(spell.volumes) - 
                flow.threshold))
    }
    else {
        return(list(p.series = p.series, n.events = n.events, 
            flow.threshold = flow.threshold, avg.duration = avg.duration, 
            max.duration = max.duration))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
