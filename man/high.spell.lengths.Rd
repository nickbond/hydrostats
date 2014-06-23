\name{high.spell.lengths}
\alias{high.spell.lengths}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the length of all high flow spells
}
\description{
Returns the length of all high flow spell events above a given percentile or user defined threshold. 

Independence criteria allow short periods below the spell threshold to be ignored and flows below a threshold (e.g. zero flows) can be ignored when calculating percentile flows (useful in ephemeral rivers).
}
\usage{
high.spell.lengths(flow.ts, quant = 0.9, user.threshold = F, defined.threshold,
ind.days = 5, ignore.zeros = T, ctf.threshold = 0.1, hydro.year=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow.ts}{
Dataframe consisting of date in column one and flow in column two. Date must be in POSIX format (see ts.format).
}
  \item{quant}{
Percentile/quantile to use for defining event magnitude (default 0.9). A value of 0.9 is the upper 90th percentile (i.e. a volume exceeded 10\% of the time).}
  \item{user.threshold}{
logical. If TRUE, the function returns results based on a user defined volume to define spell events (see defined.threshold). }
  \item{defined.threshold}{
A user supplied threshold for defining spells. This would typically be derived from hydraulic models or similar knowledge pertaining to a gauge site.
}
  \item{ind.days}{
Periods between spells of less than ind.days (default 5) are considered to be 'in spell' for the purpose of further calculations. A value of 0 means spells 1 day apart are considered indpedendent.}
  \item{ignore.zeros}{
logical. If TRUE, days below a user defined cease-to-flow threshold (default 0.1) will be excluded when estimating the spell threshold for a given percentile. This is primarily of interest in highly ephemeral rivers, where flow may only occur for a small fraction of the time. In such cases, the inclusion of zeros will skew estimates of high flow events downwards, which may be undesirable.
}
  \item{ctf.threshold}{
values below this threshold are treated as zero for the purpose of percentile based calculations (see ignore zeros).
}
	\item{hydro.year}{
logical. If TRUE, each record is first assigned to a hydrologic year based on the timing of minimum flows. See \code{\link{hydro.year}} for further details.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
Returns a dataframe of spell lengths and their associated starting dates.
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

high.spell.lengths(Cooper, user.threshold=TRUE, defined.threshold=50000)

high.spell.lengths(Cooper, user.threshold=TRUE, defined.threshold=50000, hydro.year=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
