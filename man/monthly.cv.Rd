\name{monthly.cv}
\alias{monthly.cv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate monthly coefficient of variation
}
\description{
This function takes a daily time series and returns the coefficient of variation of mean monthly flow expressed as a percentage.}
\usage{
monthly.cv(flow.ts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow.ts}{Dataframe with date and discharge data in columns named "Date" and "Q" respectively. Date must be in POSIX format (see ts.format). Missing values are ignored.
}
}

\value{
%%  ~Describe the value returned
Dataframe with 1 column (monthly CV).
}

\author{
Nick Bond <n.bond@griffith.edu.au>
}

\examples{
data(Cooper)
cooper<-ts.format(Cooper)
ann.cv(Cooper)

## The function is currently defined as
function (flow.ts) 
{
    monthly.cv <- (sd(flow.ts$Q, na.rm = T)/mean(flow.ts$Q, na.rm = T)) * 
        100
    return(list(monthly.cv = monthly.cv))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
