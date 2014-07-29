\name{partial.series}
\alias{partial.series}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Partial and annual exceedence series.
}
\description{
Returns a partial or annual exceedence series (ari=1) based on a user defined recurrence interval (ari).

For analyses based on a defined threshold (rather than recurrence interval) use high.spells instead.
}

\usage{
partial.series(flow.ts, ari = 2, ind.days = 7, duration = T, plot = F, volume = T, 
series = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow.ts}{Dataframe consisting of date in column one and flow in column two. Date must be in POSIX format (see ts.format)
}
  \item{ari}{
The desired average return interval. As a partial series, an ari of 1 will return statistics for the n largest floods in n years of record, irrespective of timing. Annual series can be derived from high.spells with annual.stats=T. . }
  \item{ind.days}{
Spells of less than ind.days (default 7) are considered to be non-independent, and only the larger of the two spells is included in the results. This behaviour differs from high.spells, where periods below the determined spell threshold of less than the independence period are infilled for the purposes of determining spell duration. This behaviour may change in the future.}
  \item{duration}{
logical. If TRUE (default), statistics describing the duration of events are returned.}
  \item{plot}{
logical. If TRUE a plot is returned showing the events included in the partial series.}
  \item{volume}{
logical. If TRUE, statistics are returned describing the volume of spells (above the spell threshold).}
 \item{series}{
logical. If TRUE, the partial series is returned. If FALSE (default), only the indices describing the partial series are returned.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list or dataframe dependent on whether series = TRUE. If TRUE, a list is returned (see below). If FALSE a dataframe is returned with all indices but without the actual partial series (p.series).

%%  ~Describe the value returned
%%  If it is a LIST, use
Indices
\item{p.series }{A dataframe containing an ordered partial series\cr showing the timing and peak magnitude of events}
\item{n.events }{Number of events in the partial series}
\item{flow.threshold }{the peak volume of the smallest event include in the series }
\item{avg.duration }{the average duration of events in the series }
\item{max.duration }{the maximum duration of events in the series }
\item{med.spell.volume }{the median volume (above the threshold) of events in the series.}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Nick Bond}
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

partial.series(Cooper,ari=2)
partial.series(Cooper, ari=5, plot=TRUE, ind.days=2)
partial.series(Cooper, ari=5, plot=TRUE, ind.days=10)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
