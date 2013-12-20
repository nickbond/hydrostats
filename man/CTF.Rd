\name{CTF}
\alias{CTF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate cease-to-flow (CTF) spell statistics
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CTF(flow.ts, threshold = 0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow.ts}{
%%     ~~Describe \code{flow.ts} here~~
}
  \item{threshold}{
%%     ~~Describe \code{threshold} here~~
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
function (flow.ts, threshold = 0.1) 
{
    flow.ts <- na.omit(flow.ts)
    if (length(flow.ts[, "Q"]) == 0 | sum(flow.ts[, "Q"]) == 
        0) {
        return(list(p.CTF = 1, avg.CTF = NA, med.CTF = NA, min.CTF = NA, 
            max.CTF = NA))
    }
    if (sum(flow.ts[, "Q"] < threshold) < 1) {
        return(list(p.CTF = 0, avg.CTF = 0, med.CTF = 0, min.CTF = 0, 
            max.CTF = 0))
    }
    else {
        CTF.ts <- ifelse(flow.ts[, "Q"] > threshold, 1, 0)
        CTF.runs <- rle(CTF.ts)
        p.CTF <- sum(flow.ts[, "Q"] < threshold)/length(flow.ts[, 
            "Q"])
        avg.CTF <- mean(CTF.runs$lengths[which(CTF.runs$values == 
            0)], na.rm = T)
        med.CTF <- median(CTF.runs$lengths[which(CTF.runs$values == 
            0)], na.rm = T)
        min.CTF <- min(CTF.runs$lengths[which(CTF.runs$values == 
            0)], na.rm = T)
        max.CTF <- max(CTF.runs$lengths[which(CTF.runs$values == 
            0)], na.rm = T)
    }
    return(list(p.CTF = p.CTF, avg.CTF = avg.CTF, med.CTF = med.CTF, 
        min.CTF = min.CTF, max.CTF = max.CTF))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
