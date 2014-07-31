\name{low.spells}
\alias{low.spells}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate low flow spell statistics
}
\description{
Calculates a suite of statistics describing low-flow spell characteristics, such as the timing, frequency and duration of events below a threshold. The event threshold can be defined as a flow quantile (e.g. the 10th  percentile, that is flows exceeded 90\% of the time) or a specific threshold volume (e.g. ML/day).

For the purpose of deriving annual low-flow spell statistics, the function can also be applied based on the hydrologic year. This is advisable where the low flow season spans calendar years, such that prolonged spells may be split at the transition from one calendar year to the next. This first requires the time series be processed using the hydro.year function. This adds an additional column indicating the hydrologic year to which each row belongs, which is used for deriving annual spell characteristics.

It is possible for there to be multiple days with the same low flow value (especially zero flows). In estimating the average timing (and sd) of of minimum flows function calculates the average doy of minimum flows in each year first, before calculating the average across years. Circular functions are used to address the proximity between days toward the beginning and end of the year.

Missing values are allowed for convenience (NA's are removed and the time-series is concatenated before functions are applied), but of course may lead to biased results. For the purpose of the annual statistics years with fewer than 350 days of available record are ignored.

Note that at this stage the timing and predictability statistics are not computed seperately when a factor is used to split the time-series into different periods, for example using ddply. (e.g.: ddply(df, .(facs), function(df) low.spells(df))).

}
\usage{
low.spells(flow.ts, quant = 0.1, defined.threshold=NULL, 
duration = T, volume = T, plot = T, ann.stats = T, ann.stats.only = F, 
hydro.year=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow.ts}{
Dataframe consisting of date in column one and flow in column two. Date must be in POSIX format (see ts.format). 

	If a third column exists then this is assumed to provide a vector of years for the purpose of calculating annual spell statistics based on a predetermined hydrologic year.}
  \item{quant}{
Percentile/quantile to use for defining event magnitude (default 0.1). A value of 0.1 is the lower 10th percentile (i.e. a volume exceeded 90\% of the time).}
  \item{defined.threshold}{
A user supplied threshold for defining spells. This would typically be derived from hydraulic models or similar knowledge pertaining to a gauge site.
}
  \item{duration}{
logical. Should statistics describing spell duration be returned?}
  \item{volume}{
logical. Should statistics describing spell volumes be returned?}
  \item{plot}{
logical. Should the time-series be plotted? Data points considered 'within spell' are identifed using red circles and the threshold is identified with a horizontal line.}
  \item{ann.stats}{
logical. If TRUE, the function returns results describing the annual series (i.e. that describing the characteristics of the spells associated with the lowest annual daily flow).}
  \item{ann.stats.only}{
logical. If TRUE, statistics describing the annual series only are returned.}
	\item{hydro.year}{
logical. If TRUE, each record is first assigned to a hydrologic year based on the timing of minimum flows. See \code{\link{hydro.year}} for further details.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}

\value{
Returns a dataframe with the following columns.

low flow indices
\item{low.spell.threshold }{The low spell threshold applied in the analysis}
\item{avg.low.spell.duration }{Average duration of spell events}
\item{max.low.duration }{Maximum duration of spell events}
\item{low.spell.freq}{The frequency of spell events (no. per year)}

Annual low flow statistics
\item{avg.min.ann}{The average annual minimum flow}
\item{timing.min.flow}{The average day of the year (0-366) on which the minimum flow occurs.}
\item{pred.min.flow}{circular standard deviation of the average timing of annual minimum flows}
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

low.spells(Cooper, quant=0.1)

low.spells(Cooper, quant=0.1, hydro.year=TRUE)

Cooper$year<-year(Cooper$Date)
ddply(Cooper, .(year), function(x) low.spells(x))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
