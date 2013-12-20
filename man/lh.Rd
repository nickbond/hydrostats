\name{lh}
\alias{lh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Lynne Hollick Filter
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lh(Q, a)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Q}{
%%     ~~Describe \code{Q} here~~
}
  \item{a}{
%%     ~~Describe \code{a} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Q, a) 
{
    qf <- rep(Q[1], length(Q))
    qb <- rep(Q[1], length(Q))
    qf[1] <- (Q[2] - Q[1]) * ((1 + a)/2)
    for (i in 2:length(Q)) {
        qf[i] <- a * qf[i - 1] + ((Q[i] - Q[i - 1]) * ((1 + a)/2))
    }
    for (i in 1:length(Q)) {
        if (qf[i] < 0) {
            qb[i] <- Q[i]
        }
        else if (Q[i] > qf[i]) {
            qb[i] <- Q[i] - qf[i]
        }
        else qb[i] <- 0
    }
    return(qb)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
