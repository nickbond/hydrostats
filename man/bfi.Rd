\name{bfi}
\alias{bfi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate baseflow indices using the Lynne-Hollick filter
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bfi(flow.ts, a, ts = "mean")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow.ts}{
Dataframe consisting of date in column one and flow in column two. Date must be in POSIX format (see ts.format). Missing values are ignored.
}
  \item{a}{
The alpha value used in the Lynne-Hollick filter for digital baseflow separation. Default value is 0.975  
}
  \item{ts}{
ts="mean" returns means for the entire time series
annual returns annual averages. Note this function does not currently use hydrologic years even when defined
daily returns complete series}
}
\details{
Technically the LH filter cannot be calculated where there are missing data. Here the function removes missing values and is applied to a concatenated version of the time series. Missing dates are reinserted after the filter has been applied for the purpose of returning annual or daily series. The function further reports the number of missing values leaving the user to decide on the reliability of the baseflow estimates.
}
\value{
ts="mean" 
 \item{prop.obs }{proportion of non-missing observations}
 \item{MDF }{mean daily flow}
 \item{Q50 }{median daily flow}
 \item{mean.bf }{mean baseflow volume}
 \item{mean.bfi }{mean baseflow index}
 
ts="annual"
	\item{}{A dataframe containing columns "year", "no.obs",  "Q", "bf", "bfi", where no.obs is the number of observations, and bf and bfi are the mean baseflow and baseflow index, respectively, for each year in the record.}
	
ts="daily"
	\item{}{The original dataframe with appended columns "bf" and "bfi". See ts="annual" for details.}
%% ...
}
\references{
Lynne, V., Hollick, M. (1979) Stochastic time-variable rainfall-runoff modelling. In: pp. 89-93 Institute of Engineers Australia National Conference. Perth.
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
cooper<-ts.format(Cooper)

bfi(Cooper,a=0.975, ts="mean")
bfi(Cooper,a=0.975, ts="annual")
bfi(Cooper,a=0.975, ts="daily")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
