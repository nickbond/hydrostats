\name{high.spells}
\alias{high.spells}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculate high spell statistics
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
high.spells(flow.ts, quant = 0.9, user.threshold = F, defined.threshold, ind.days = 5, duration = T, volume = T, plot = T, ignore.zeros = F, ctf.threshold = 0.1, ann.max.stats = T, ann.max.only = F, inter.flood = F)
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
  \item{duration}{
%%     ~~Describe \code{duration} here~~
}
  \item{volume}{
%%     ~~Describe \code{volume} here~~
}
  \item{plot}{
%%     ~~Describe \code{plot} here~~
}
  \item{ignore.zeros}{
%%     ~~Describe \code{ignore.zeros} here~~
}
  \item{ctf.threshold}{
%%     ~~Describe \code{ctf.threshold} here~~
}
  \item{ann.max.stats}{
%%     ~~Describe \code{ann.max.stats} here~~
}
  \item{ann.max.only}{
%%     ~~Describe \code{ann.max.only} here~~
}
  \item{inter.flood}{
%%     ~~Describe \code{inter.flood} here~~
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
    ind.days = 5, duration = T, volume = T, plot = T, ignore.zeros = F, 
    ctf.threshold = 0.1, ann.max.stats = T, ann.max.only = F, 
    inter.flood = F) 
{
    if (ncol(flow.ts) > 2) {
        record.year <- flow.ts[, "Year"]
    }
    else {
        record.year <- strftime(flow.ts[[1]], format = "\%Y")
        flow.ts <- data.frame(flow.ts, year = record.year)
    }
    n.years <- nlevels(as.factor(record.year))
    if (ann.max.stats == T) {
        flow.ts.comp <- na.omit(flow.ts)
        n.days <- tapply(flow.ts.comp[[2]], flow.ts.comp[[3]], 
            length)
        n.most.days <- which(n.days > 350)
        flow.ts.comp <- flow.ts.comp[which(flow.ts.comp[[3]] \%in\% 
            names(n.days)), ]
        record.year <- flow.ts.comp[[3]]
        n.years <- nlevels(as.factor(record.year))
        ann.max <- tapply(flow.ts.comp[[2]], record.year, max)
        ann.max.day.no <- tapply(flow.ts.comp[[2]], record.year, 
            which.max) - 1
        correct.ann.max.day <- day.dist(names(ann.max.day.no), 
            ann.max.day.no)
        max.ann.flow.threshold <- min(ann.max, na.rm = T)
        ann.max.spells <- ifelse(flow.ts[, 2] > max.ann.flow.threshold, 
            1, 0)
        ann.max.spell.runs <- rle(ann.max.spells)
        avg.ann.duration <- mean(ann.max.spell.runs$lengths[which(ann.max.spell.runs$values == 
            1)], na.rm = T)
        max.ann.duration <- max(ann.max.spell.runs$lengths[which(ann.max.spell.runs$values == 
            1)], na.rm = T)
        cv.ann.duration <- (sd(ann.max.spell.runs$lengths[which(ann.max.spell.runs$values == 
            1)], na.rm = T)/mean(ann.max.spell.runs$lengths[which(ann.max.spell.runs$values == 
            1)], na.rm = T)) * 100
    }
    if (ann.max.only == T) {
        return(list(avg.max.ann = mean(ann.max[is.finite(ann.max)], 
            na.rm = T), cv.max.ann = (sd(ann.max, na.rm = T)/mean(ann.max, 
            na.rm = T) * 100), flood.timing = correct.ann.max.day[1], 
            flood.predictability = correct.ann.max.day[2], flood.skewness = mean(ann.max, 
                na.rm = T)/mean(flow.ts[, 2], na.rm = T), avg.ann.duration = avg.ann.duration, 
            cv.ann.duration = cv.ann.duration))
    }
    else {
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
                flow.threshold <- quantile(flow.ts[, 2], quant, 
                  na.rm = T)
                names(flow.threshold) <- NULL
            }
        }
        high.flows <- ifelse(flow.ts[, 2] > flow.threshold, 1, 
            0)
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
        high.flow.av <- mean(flow.ts[which(high.flows == 1), 
            2], na.rm = T)
        high.flow.sd <- sd(flow.ts[which(high.flows == 1), 2], 
            na.rm = T)
        good.high.flow.runs <- which(!is.na(high.flow.runs$values))
        flow.runs.values <- high.flow.runs$values[good.high.flow.runs]
        flow.runs.lengths <- high.flow.runs$lengths[good.high.flow.runs]
        n.events <- length(flow.runs.values[flow.runs.values == 
            1])
        flood.frequency <- n.events/n.years
        if (duration == TRUE) {
            avg.duration <- mean(high.flow.runs$lengths[which(high.flow.runs$values == 
                1)], na.rm = T)
            med.duration <- quantile(high.flow.runs$lengths[which(high.flow.runs$values == 
                1)], 0.5, na.rm = T)
            max.duration <- max(high.flow.runs$lengths[which(high.flow.runs$values == 
                1)], na.rm = T)
            sd.duration <- sd(high.flow.runs$lengths[which(high.flow.runs$values == 
                1)], na.rm = T)
            cv.duration <- sd.duration/avg.duration * 100
        }
        if (inter.flood == TRUE) {
            avg.interval <- mean(high.flow.runs$lengths[which(high.flow.runs$values == 
                0)], na.rm = T)
            max.interval <- max(high.flow.runs$lengths[which(high.flow.runs$values == 
                0)], na.rm = T)
            min.interval <- min(high.flow.runs$lengths[which(high.flow.runs$values == 
                0)], na.rm = T)
            sd.interval <- sd(high.flow.runs$lengths[which(high.flow.runs$values == 
                0)], na.rm = T)
            cv.interval <- sd.interval/avg.interval * 100
            return(list(ofs.threshold = flow.threshold, avg.ofs.interval = avg.interval, 
                min.ofs.interval = min.interval, max.ofs.interval = max.interval))
        }
        if (volume == TRUE) {
            spell.factor <- rep(seq_along(high.flow.runs$lengths), 
                times = high.flow.runs$lengths)
            spells <- split(flow.ts[[2]], spell.factor)
            spell.volumes <- flow.ts[[2]]
            spell.volumes <- sapply(spells, sum)
            spell.volumes <- spell.volumes[which(high.flow.runs$values == 
                1)]
        }
        if (plot == TRUE) {
            plot(flow.ts[[2]], type = "l", main = i)
            points(which(high.flows == 1), flow.ts[which(high.flows == 
                1), 2], col = "red")
            abline(h = flow.threshold)
        }
    }
    if (ann.max.stats == F) {
        return(list(n.events = n.years/flood.frequency, high.spell.threshold = flow.threshold, 
            avg.high.spell.duration = avg.duration, med.high.spell.duration = med.duration, 
            max.high.spell.duration = max.duration))
    }
    else {
        return(list(n.years = n.years, high.spell.threshold = flow.threshold, 
            n.events = n.events, avg.high.spell.duration = avg.duration, 
            med.high.spell.duration = med.duration, max.high.spell.duration = max.duration, 
            spell.freq = flood.frequency, avg.spell.volume = mean(spell.volumes, 
                na.rm = T), avg.spell.peak = high.flow.av, sd.spell.peak = high.flow.sd, 
            avg.max.ann = mean(ann.max[is.finite(ann.max)], na.rm = T), 
            cv.max.ann = (sd(ann.max, na.rm = T)/mean(ann.max, 
                na.rm = T) * 100), flood.skewness = mean(ann.max, 
                na.rm = T)/mean(flow.ts[, 2], na.rm = T), flood.timing = correct.ann.max.day[1], 
            flood.predictability = correct.ann.max.day[2], avg.ann.duration = avg.ann.duration, 
            cv.ann.duration = cv.ann.duration))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
