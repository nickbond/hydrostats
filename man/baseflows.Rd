\name{baseflows}
\alias{baseflows}

\title{
Measures of central tendency and baseflow via the Lynne-Hollick baseflow filter
}
\description{
Calculate measure of central tendency and baseflow indices using the Lynne-Hollick filter
}
\usage{
baseflows(flow.ts, a, ts = "mean")
}

\arguments{
  \item{flow.ts}{
Dataframe with date and discharge data in columns named "Date" and "Q" respectively. Date must be in POSIX format (see ts.format). Missing values are ignored.
}
  \item{a}{
The alpha value used in the Lynne-Hollick filter for digital baseflow separation. Default value is 0.975  
}
  \item{ts}{
ts="mean" returns means for the entire time series \cr
ts="annual" returns annual averages. Note this function does not currently use hydrologic years even when defined\cr
ts="daily" returns complete series}
}
\details{
Technically the LH filter cannot be calculated where there are missing data. Here the function removes missing values and is applied to a concatenated version of the time series. Missing dates are reinserted after the filter has been applied for the purpose of returning annual or daily series. The function further reports the number of missing values leaving the user to decide on the reliability of the baseflow estimates.
}
\value{
Returns a dataframe. See below for details.

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

}
\references{
Lynne, V., Hollick, M. (1979) Stochastic time-variable rainfall-runoff modelling. In: pp. 89-93 Institute of Engineers Australia National Conference. Perth.
}
\author{
Nick Bond
}

\examples{
data(Cooper)
Cooper<-ts.format(Cooper)

baseflows(Cooper,a=0.975, ts="mean")
baseflows(Cooper,a=0.975, ts="annual")
baseflows(Cooper,a=0.975, ts="daily")


}
