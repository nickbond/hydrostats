\name{seasonality}
\alias{seasonality}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate seasonality of discharge
}
\description{
Returns statistics decribing seasonal variation in runoff. }
\usage{
seasonality(flow.ts, month.range = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow.ts}{
Dataframe consisting of date in column one and flow in column two. Date must be in POSIX format (see ts.format). Missing values are ignored.
}
  \item{month.range}{
logical. If FALSE (default), the function returns the percentage of runoff occurring during the average driest 6 month period (as defined across all years). If true, additional statistics describing cumulative average monthly flows, the range between the runoff in the wettest and driest months, and the average number of months between the wettest and driest periods of runoff.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
monthly.range=F\cr
a vector of length 1. Percentage of annual runoff delivered during the average driest 6 month period.

monthly.range=T\cr
\item{Seasonality }{Description of 'comp1'}
\item{average monthly flow }{Description of 'comp2'}
\item{average annual monthly range }{Description of 'comp2'}
\item{average number of months between the highest and lowest monthly runoff }{Description of 'comp2'}
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

Data(Cooper)
Cooper<-ts.format(Cooper)

seasonality(Cooper, monthly.range=T)



## The function is currently defined as
function (flow.ts, month.range = F) 
{
    month.runs <- c()
    month.means <- aggregate(flow.ts[[2]], by = list(month = strftime(flow.ts[[1]], 
        format = "\%m")), mean, na.rm = T)
    month.runs[1] <- sum(month.means[1:6, 2], na.rm = T)
    month.runs[2] <- sum(month.means[2:7, 2], na.rm = T)
    month.runs[3] <- sum(month.means[3:8, 2], na.rm = T)
    month.runs[4] <- sum(month.means[4:9, 2], na.rm = T)
    month.runs[5] <- sum(month.means[5:10, 2], na.rm = T)
    month.runs[6] <- sum(month.means[6:11, 2], na.rm = T)
    month.runs[7] <- sum(month.means[7:12, 2], na.rm = T)
    month.runs[8] <- sum(month.means[c(8:12, 1), 2], na.rm = T)
    month.runs[9] <- sum(month.means[c(9:12, 1:2), 2], na.rm = T)
    month.runs[10] <- sum(month.means[c(10:12, 1:3), 2], na.rm = T)
    month.runs[11] <- sum(month.means[c(11:12, 1:4), 2], na.rm = T)
    month.runs[12] <- sum(month.means[c(12, 1:5), 2], na.rm = T)
    month.runs.sort <- sort(month.runs)
    low.6.months <- sum(month.runs.sort[1:6])
    out <- low.6.months/sum(month.runs) * 100
    if (month.range == T) {
        month.year.means <- tapply(flow.ts[, "Q"], flow.ts[, 
            c("year", "month")], sum)
        month.year.means <- na.omit(month.year.means)
        monthly.means <- apply(month.year.means, 2, mean, na.rm = T)
        month.range <- apply(month.year.means, 1, range, na.rm = T)
        min.months <- apply(month.year.means, 1, which.min)
        max.months <- apply(month.year.means, 1, which.max)
        av.ann.month.range <- mean(month.range[2, ] - month.range[1, 
            ])
        month.difs <- abs(max.months - min.months)
        month.difs <- recode(month.difs, "11='1'; 10='2'; 9='3'; 8='4'; 7='5'")
        return(list(seasonality = out, monthly.means, avg.ann.month.range = av.ann.month.range, 
            max.min.time.dif = mean(month.difs)))
    }
    else {
        return(out)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
