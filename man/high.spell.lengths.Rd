\name{high.spell.lengths}
\alias{high.spell.lengths}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the length of all high flow spells
}
\description{
Returns the length (and start date) of all flow spells above (or below) a given percentile or user defined threshold. 

Independence criteria allow short periods below the spell threshold to be ignored and flows below a threshold (e.g. zero flows) can be ignored when calculating percentile flows (useful in ephemeral rivers).
}
\usage{
high.spell.lengths(flow.ts, quant = 0.9, threshold,
ind.days = 5, ignore.zeros = T, ctf.threshold = 0.1, inter.flood=FALSE)
}

\arguments{
  \item{flow.ts}{
Dataframe with date and discharge data in columns named "Date" and "Q" respectively. Date must be in POSIX format (see ts.format).
}
  \item{quant}{
Percentile/quantile to use for defining event magnitude (default 0.9). A value of 0.9 is the upper 90th percentile (i.e. a volume exceeded 10\% of the time).}
  \item{threshold}{
A user supplied threshold for defining spells. This would typically be derived from hydraulic models or similar knowledge pertaining to a gauge site.
}
  \item{ind.days}{
Periods between spells of less than ind.days (default 5) are considered to be 'in spell' for the purpose of further calculations. A value of 0 means spells 1 day apart are considered indpedendent.}
  \item{ignore.zeros}{
logical. If TRUE, days below a user defined cease-to-flow threshold (default 0.1) will be excluded when estimating the spell threshold for a given percentile. This is primarily of interest in highly ephemeral rivers, where flow may only occur for a small fraction of the time. In such cases, the inclusion of zeros will skew estimates of high flow events downwards, which may be undesirable.}
  \item{ctf.threshold}{
values below this threshold are treated as zero for the purpose of percentile based calculations (see ignore zeros).}
	\item{inter.flood}{
logical. If TRUE, the function returns the spell lengths and start dates for periods below (rather than above) the defined threshold.
	}
}

\value{
Returns a dataframe of spell lengths and their associated starting dates.

Note that spells will always end at NAs.
}

\author{
Nick Bond <n.bond@griffith.edu.au>
}
\examples{
data(Cooper)
Cooper<-ts.format(Cooper)

high.spell.lengths(Cooper, threshold=50000)

}
