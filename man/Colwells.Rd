\name{Colwells}
\alias{Colwells}

\title{
Colwells indices }
\description{
Calculates Colwell's (1974), which provide a measure of the seasonal predictability of environmental phenomena. Defined in terms of Predictability (P), Constancy (C) and Contingency (M). For detailed information on the calculation and description of Colwell's indices refer to (Colwell, 1974).}
\usage{
Colwells(flow.ts, fn = "mean",
boundaries = "transform", s = 11,
base.binning = 2, from = 0.5, by = 0.25, base.entropy=2, indices.only=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow.ts}{
Dataframe with date and discharge data in columns named "Date" and "Q" respectively. Date must be in POSIX format (see ts.format). Missing values are ignored.
}
  \item{fn}{
The function used to summarise daily data (default mean) and for scaling break points when binning data. Can also use median, min, max.}
  \item{boundaries}{
The method used to define break points when binning data.

boundaries="equal"  splits the data into s equal sized bins

boundaries="transform" (default) first applies a log10(x+1) transformation and then splits the data into s equal size bins

boundaries="log_class_size"  generates breaks based on logarithmic scale (default base 2) with a roughly equal number of bins above and below 1

boundaries="weighted_log_class_size" generates breaks based on logarithmic scale (default base 2) * mean (or other summary statistic) of the variable. A roughly equal number of bins occur above and below the mean (or other summary statistic)

boundaries="Gan" creates bins that match those of Gan et al. (1991). Requires: from (default 0.25), by (default 0.25) and s (number of bins)
}

 	\item{s}{
The number of classes the flow data is broken into (default 11)
}

  \item{base.binning}{
The base integer for defining classes when using the "log_class_size" or "weighted_log_class_size" boundaries
}

  \item{from}{
The lowest break point for defining classes when using the "Gan" boundaries (default 0.25)
}

  \item{by}{
The bin width when using the "Gan" boundaries (default 0.25)
}

  \item{base.entropy}{
The base integer used for the entropy calculations (default=2)
}
  \item{indices.only}{
Logical. If FALSE (default), the function returns a list of length 3, including the breaks, a table of frequencies, and Colwell's indices. If TRUE, the function returns just a dataframe of indices, useful for combining output with that from other functions.}
}
\details{
Predictability measures how tightly an event is linked to a season; Constancy measures how uniformly the event occurs through all seasons, and Contingency measures the repeatability of season patterns. Predictability is the sum of Constancy and Contingency, and reflects the likelihood of being able to predict a flow occurrence. It is maximized when the flow is constant throughout the year (Constancy Maximised), or if the pattern of high or low flow occurrence is repeated across all years (Contingency maximized).
}
\value{
A list or dataframe (see above).

 \item{breaks }{shows the break points used between classes. Not returned for all boundary options. The upper and lower classes are always open even if -Inf/Inf are not shown.}
 \item{flow.table }{Table showing the number of times the monthly flows fall into each flow class in each month. Useful for examining the results of different binning techniques.}
 \item{P }{Predictability}
 \item{C }{Constancy}
 \item{M }{Contingency}
 \item{CP }{C/P}
 \item{MP }{M/P}

}
\references{
Colwell, R.K. 1974. Predictability, constancy, and contingency of periodic phenomena. Ecology 55(5): 1148-53.

Gan, K.C., McMahon, T.A., and Finlayson, B.L. 1991. Analysis of periodicity in streamflow and rainfall data by Colwell's indices. Journal of Hydrology 123(1-2): 105-18.
}
\author{
Nick Bond <n.bond@latrobe.edu.au>
}
\examples{
data(Cooper)
Cooper<-ts.format(Cooper)

Colwells(Cooper, s=5)
Colwells(Cooper, boundaries="equal", s=11)
Colwells(Cooper, boundaries="log_class_size", s=11)
Colwells(Cooper, boundaries="weighted_log_class_size", s=11)

Colwells(Cooper, boundaries="Gan", from=1,by=1, s=4)
Colwells(Cooper, boundaries="Gan", from=0.25,by=0.25, s=9)

Colwells(Cooper, boundaries="Gan", from=0.25,by=0.25, s=9, indices.only=TRUE)


%Calculate indices for two gauges (Acheron River and Cooper Creek) simultaneously using the plyr package.
require(plyr)
data(Acheron)
Acheron<-ts.format(Acheron)
flow.ts<-rbind(data.frame(River="Acheron", Acheron), data.frame(River="Cooper", Cooper))

ddply(flow.ts, .(River), function(x)
Colwells(x, boundaries="weighted_log_class_size", s=11, indices.only=TRUE))
}
