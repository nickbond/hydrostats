\name{low.spells}
\alias{low.spells}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate low flow spell statistics
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
low.spells(flow.ts, quant = 0.1, duration = T, volume = T, plot = T, annual.stats = T, ann.min.only = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow.ts}{
%%     ~~Describe \code{flow.ts} here~~
}
  \item{quant}{
%%     ~~Describe \code{quant} here~~
}
  \item{duration}{
%%     ~~Describe \code{duration} here~~
}
  \item{volume}{
%%     ~~Describe \code{volume} here~~
}
  \item{plot}{
%%     ~~Describe \code{plot} here~~
}
  \item{annual.stats}{
%%     ~~Describe \code{annual.stats} here~~
}
  \item{ann.min.only}{
%%     ~~Describe \code{ann.min.only} here~~
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
function (flow.ts, quant = 0.1, duration = T, volume = T, plot = T, 
    annual.stats = T, ann.min.only = F) 
{
    record.year <- flow.ts[[3]]
    n.years <- nlevels(as.factor(record.year))
    if (annual.stats == T) {
        flow.ts.comp <- na.omit(flow.ts)
        n.days <- tapply(flow.ts.comp[[2]], flow.ts.comp[[3]], 
            length)
        n.most.days <- which(n.days > 350)
        flow.ts.comp <- flow.ts.comp[which(flow.ts.comp[[3]] \%in\% 
            names(n.days)), ]
        record.year <- flow.ts.comp[[3]]
        n.years <- nlevels(as.factor(record.year))
        ann.min <- tapply(flow.ts.comp[[2]], flow.ts.comp[[3]], 
            min)
        ann.min.day.no <- tapply(rev(flow.ts.comp[[2]]), flow.ts.comp[[3]], 
            which.min) - 1
        ann.min.date <- as.Date(paste(names(ann.min.day.no), 
            1, 1, sep = "-")) + ann.min.day.no
        correct.ann.min.day <- day.dist(names(ann.min.day.no), 
            ann.min.day.no)
    }
    if (ann.min.only == T) {
        return(list(avg.min.ann = mean(ann.min[is.finite(ann.min)], 
            na.rm = T), timing.min.flow = correct.ann.min.day[1], 
            min.timing.predictability = correct.ann.min.day[2]))
    }
    else {
        flow.threshold <- quantile(flow.ts[, 2], quant, na.rm = T)
        names(flow.threshold) <- NULL
        low.flows <- ifelse(flow.ts[, 2] <= flow.threshold, 1, 
            0)
        low.flow.av <- mean(flow.ts[which(low.flows == 1), 2])
        low.flow.sd <- sd(flow.ts[which(low.flows == 1), 2])
        low.flow.runs <- rle(low.flows)
        low.spell.days <- as.numeric(strftime(flow.ts[which(low.flows == 
            1), 1], format = "\%j"))
        good.low.flow.runs <- which(!is.na(low.flow.runs$values))
        flow.runs.values <- low.flow.runs$values[good.low.flow.runs]
        flow.runs.lengths <- low.flow.runs$lengths[good.low.flow.runs]
        low.spell.frequency <- length(flow.runs.values[flow.runs.values == 
            1])/n.years
        if (duration == TRUE) {
            avg.duration <- mean(low.flow.runs$lengths[which(low.flow.runs$values == 
                1)], na.rm = T)
            max.duration <- max(low.flow.runs$lengths[which(low.flow.runs$values == 
                1)], na.rm = T)
            sd.duration <- sd(low.flow.runs$lengths[which(low.flow.runs$values == 
                1)], na.rm = T)
            cv.duration <- sd.duration/avg.duration
        }
        if (volume == TRUE) {
            spell.factor <- rep(seq_along(low.flow.runs$lengths), 
                times = low.flow.runs$lengths)
            spells <- split(flow.ts[[2]], spell.factor)
            spell.volumes <- flow.ts[[2]]
            spell.volumes <- sapply(spells, sum)
            spell.volumes <- spell.volumes[which(low.flow.runs$values == 
                1)]
        }
        if (plot == TRUE) {
            plot(flow.ts[[2]], type = "l")
            points(which(low.flows == 1), flow.ts[which(low.flows == 
                1), 2], col = "red")
            abline(h = flow.threshold)
        }
    }
    return(list(low.spell.threshold = flow.threshold, avg.low.spell.duration = avg.duration, 
        max.low.duration = max.duration, low.spell.freq = low.spell.frequency, 
        avg.min.ann = mean(ann.min[is.finite(ann.min)], na.rm = T), 
        timing.min.flow = correct.ann.min.day[1], pred.min.flow = correct.ann.min.day[2]))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
