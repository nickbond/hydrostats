\name{high.spells}
\alias{high.spells}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates high flow spell statistics
}
\description{
Calculates a suite of statistics describing flood characteristics, such as the timing, frequency and duration of events. The event threshold can be defined as a flow quantile (e.g. upper 90th percentile) or a specific threshold volume (e.g. ML/day).

For the purpose of deriving annual flood statistics, the function can also be applied based on the hydrologic year. This is advisable where the high flow season spans years, such that prolonged spells may span years. This first requires the time series be processed using the hydro.year function. This adds an additional column indicating the hydrologic year to which each row belongs, which is used for deriving annual spell characteristics.

It is possible for there to be multiple days with the same annual maximum flow value (although less likely than for low flows). In estimating the average timing (and sd of timing) of minimum flows, the function calculates the average day of year (DOY) of minimum flows in each year first, before calculating the average across years. Circular functions are used to address the proximity between days toward the beginning and end of the year.

Missing values are allowed for convenience (NA's are removed and the time-series is concatenated before functions are applied), but of course may lead to biased results. For the purpose of the annual statistics years with fewer than 350 days of available record are ignored.

When used with ddply to compute outputs for multiple gauges or time periods simultaneously, results, icluding graphs are produced for each factor level, including graphs. Note the funtion will return warnings if annual stats are calculated when year is used as a factor.

}
\usage{
high.spells(flow.ts, quant = 0.9, threshold = NULL,
ind.days = 5, duration = TRUE, volume = TRUE, plot = TRUE, ignore.zeros = FALSE,
ctf.threshold = 0.1, ann.stats = TRUE, ann.stats.only = FALSE, inter.flood = FALSE, 
hydro.year=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{flow.ts}{Dataframe consisting of date in column one and flow in column two. Date must be in POSIX format (see ts.format).
    
    If a third column exists then this is assumed to provide a vector of years for the purpose of calculating annual spell statistics based on a predetermined hydrologic year.
    }
  \item{quant}{Percentile/quantile to use for defining event magnitude (default 0.9). A value of 0.9 is the upper 90th percentile (i.e. a volume exceeded 10\% of the time).
  }
  \item{threshold}{
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
	\item{hydro.year}{
logical. If TRUE, each record is first assigned to a hydrologic year based on the timing of minimum flows. See \code{\link{hydro.year}} for further details.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a dataframe with the following columns.

flood indices
\item{n.years }{The number of years of record in the series}
\item{high.spell.threshold }{The high spell threshold applied in the analysis)}
\item{n.events }{The number of events in the series greater than or equal to the high.spell.threshold}
\item{spell.freq}{The frequency of spell events (no. per year)}
\item{ari}{Average Recurrence Interval of events in years (1/spell.freq)}
\item{avg.high.spell.duration}{Average duration of spell events}
\item{med.high.spell.duration}{Median duration of spell events}
\item{max.high.spell.duration}{Maximum duration of spell events}
\item{avg.spell.volume}{Average spell volume (volumes above the threshold only)}
\item{avg.spell.peak}{Average spell peak}
\item{sd.spell.peak}{Standard deviation in spell speaks}
\item{avg.rise}{Average absolute rate of daily rise during spell events}
\item{avg.fall}{Average absolute rate of daily fall during spell events}

interflood indices
\item{average.interval}{The average time between spells (years)}
\item{min.interval}{The mininum time between spells (years)}
\item{max.interval}{The maximum time between spells (years)}

Annual flood statistics
\item{avg.max.ann}{The average annual maximum flow}
\item{cv.max.ann}{The coefficient of variation of annual maximum flows}
\item{flood.skewness}{The average annual maximum / mean daily flow}
\item{ann.max.timing}{The average day of the year (0-366) on which maximum flows occur}
\item{ann.max.timing.sd}{circular standard deviation of the average timing of annual maximum flows}
\item{avg.ann.duration}{The average duration of the annual maximum spells}
\item{cv.ann.duration}{The coefficient of variation of the duration of annual maximum spells}

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

high.spells(Cooper, quant=0.9, ann.stats=FALSE, ignore.zeros=TRUE, hydro.year=TRUE)

%generate results for each year
require(plyr)
Cooper$year<-strftime(Cooper$Date, format="\%Y")
ddply(Cooper, .(year), function(x) high.spells(x, ann.stats=F)

%generate seperate results before/after 1980.
Cooper$time.period <- ifelse(Cooper$year<1980,"pre_1980","post_1980")

ddply(Cooper, .(time.period), function(x) low.spells(x, ann.stats=F))

}

%%end function


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line