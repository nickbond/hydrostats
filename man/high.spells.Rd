\name{high.spells}
\alias{high.spells}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates high flow spell statistics
}
\description{
Calculates a suite of statistics describing flood characteristics, such as the timing, frequency and duration of events. The event threshold can be defined as a flow quantile (e.g. upper 90th percentile) or a specific threshold volume (e.g. ML/day).

For the purpose of deriving annual flood statistics, the function can also be applied based on the hydrologic year. This is advisable where the high flow season spans years, such that prolonged spells may span years. This first requires the time series be processed using the hydro.year function. This adds an additional column indicating the hydrologic year to which each row belongs, which is used for deriving annual spell characteristics.

Missing values are allowed for convenience (NA's are removed and the time-series is concatenated before functions are applied), but of course may lead to biased results.



}
\usage{
high.spells(flow.ts, quant = 0.9, user.threshold = FALSE, defined.threshold=0.9,
ind.days = 5, duration = TRUE, volume = TRUE, plot = TRUE, ignore.zeros = FALSE,
ctf.threshold = 0.1, ann.stats = TRUE, ann.stats.only = FALSE, inter.flood = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{flow.ts}{Dataframe consisting of date in column one and flow in column two. Date must be in POSIX format (see ts.format).
    
    If a third column exists then this is assumed to provide a vector of years for the purpose of calculating annual spell statistics based on a predetermined hydrologic year.
    }
  \item{quant}{Percentile/quantile to use for defining event magnitude (default 0.9). A value of 0.9 is the upper 90th percentile (i.e. a volume exceeded 10\% of the time).
  }
  \item{user.threshold}{
logical. If TRUE, the function returns results based on a user defined volume to define spell events (see defined.threshold). 
}
  \item{defined.threshold}{
A user supplied threshold for defining spells. This would typically be derived from hydraulic models or similar knowledge pertaining to a gauge site.
}
  \item{ind.days}{
Periods between spells of less than ind.days (default 5) are considered to be 'in spell' for the purpose of further calculations. A value of 0 means spells 1 day apart are considered indpedendent.}
  \item{duration}{
logical. Should statistics describing spell duration be returned?}
  \item{volume}{
logical. Should statistics describing spell volumes be returned? Note that for days considered 'in-spell', the returned values have the threshold volume subtracted first, and hence reflect the amount of water that was flowing past the gauge above threshold. This is most useful in water planning scenarios.}
  \item{plot}{
logical. Should the time-series be plotted? Data points considered 'within spell' are identifed using red circles and the threshold is identified with a horizontal line.}
  \item{ignore.zeros}{
logical. If TRUE, days below a user defined cease-to-flow threshold (default 0.1) will be excluded when estimating the spell threshold for a given percentile. This is primarily of interest in highly ephemeral rivers, where flow may only occur for a small fraction of the time. In such cases, the inclusion of zeros will skew estimates of high flow events downwards, which may be undesirable.
}
  \item{ctf.threshold}{
values below this threshold are treated as zero for the purpose of percentile based calculations (see ignore zeros).
}
  \item{ann.stats}{
logical. If TRUE, the function returns results describing the annual series (i.e. that describing the characteristics of the largest flood event in each year of the time-series).
}
  \item{ann.stats.only}{
logical. If TRUE, statistics describing the annual series only are returned.}

  \item{inter.flood}{
logical. If TRUE, statistics describing inter-flood spell characteristics are reported.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
flood indices
\item{n.years }{Description of 'comp1'}
\item{high.spell.threshold }{Description of 'comp2'}
\item{n.events }{Description of 'comp2'}
\item{spell.freq}{Description of 'comp2'}
\item{ari}{Description}
\item{avg.high.spell.duration}{Description of 'comp2'}
\item{med.high.spell.duration}{Description of 'comp2'}
\item{max.high.spell.duration}{Description of 'comp2'}
\item{avg.spell.volume}{Description of 'comp2'}
\item{avg.spell.peak}{Description of 'comp2'}
\item{sd.spell.peak}{Description of 'comp2'}

interflood indices
\item{average.interval}{Description of 'comp1'}
\item{min.interval}{Description of 'comp2'}
\item{max.interval }{Description of 'comp2'}

Annual flood statistics
\item{avg.max.ann}{Description of 'comp2'}
\item{cv.max.ann}{Description of 'comp2'}
\item{flood.skewness}{Description of 'comp2'}
\item{flood.timing}{Description of 'comp2'}
\item{flood.predictability}{Description of 'comp2'}
\item{avg.ann.duration}{Description of 'comp2'}
\item{cv.ann.duration}{Description of 'comp2'}

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Nick Bond
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

high.spells(Cooper, quant=0.9)

high.spells(Cooper, quant=0.9, ann.stats=FALSE, plot=FALSE)

high.spells(Cooper, quant=0.9, ann.stats=FALSE, ignore.zeros=TRUE)

         

}

%%end function


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line