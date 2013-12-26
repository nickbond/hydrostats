\name{low.spells}
\alias{low.spells}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate low flow spell statistics
}
\description{
Calculates a suite of statistics describing low-flow spell characteristics, such as the timing, frequency and duration of events below a threshold. The event threshold can be defined as a flow quantile (e.g. the 10th  percentile, that is flows exceeded 90\% of the time) or a specific threshold volume (e.g. ML/day).

For the purpose of deriving annual low-flow spell statistics, the function can also be applied based on the hydrologic year. This is advisable where the low flow season spans years, such that prolonged spells may span years. This first requires the time series be processed using the hydro.year function. This adds an additional column indicating the hydrologic year to which each row belongs, which is used for deriving annual spell characteristics.

Missing values are allowed for convenience (NA's are removed and the time-series is concatenated before functions are applied), but of course may lead to biased results.
}
\usage{
low.spells(flow.ts, quant = 0.1, duration = T, volume = T, plot = T,
annual.stats = T, ann.stats.only = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow.ts}{
Dataframe consisting of date in column one and flow in column two. Date must be in POSIX format (see ts.format). 

	If a third column exists then this is assumed to provide a vector of years for the purpose of calculating annual spell statistics based on a predetermined hydrologic year.}
  \item{quant}{
Percentile/quantile to use for defining event magnitude (default 0.1). A value of 0.1 is the lower 10th percentile (i.e. a volume exceeded 90\% of the time).}
%  \item{user.threshold}{
%User-defined thresholds are not implemented at this stage. 
%}
%  \item{defined.threshold}{
%User-defined thresholds are not implemented at this stage.
%}
  \item{duration}{
logical. Should statistics describing spell duration be returned?}
  \item{volume}{
logical. Should statistics describing spell volumes be returned?}
  \item{plot}{
logical. Should the time-series be plotted? Data points considered 'within spell' are identifed using red circles and the threshold is identified with a horizontal line.}
  \item{annual.stats}{
logical. If TRUE, the function returns results describing the annual series (i.e. that describing the characteristics of the spells associated with the lowest annual daily flow).}
  \item{ann.stats.only}{
logical. If TRUE, statistics describing the annual series only are returned.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}

\value{
%%  ~Describe the value returned
low flow indices
\item{low.spell.threshold }{Description of 'comp1'}
\item{avg.low.spell.duration }{Description of 'comp2'}
\item{max.low.duration }{Description of 'comp2'}
\item{low.spell.freq}{Description of 'comp2'}

Annual low flow statistics
\item{avg.min.ann}{Description of 'comp2'}
\item{timing.min.flow}{Description of 'comp2'}
\item{pred.min.flow}{Description of 'comp2'}
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
