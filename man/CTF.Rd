\name{CTF}
\alias{CTF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cease-to-flow (CTF) spell statistics
}
\description{
Calculates summary statistics describing cease-to-flow spell characteristics.}
\usage{
CTF(flow.ts, threshold = 0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flow.ts}{
Dataframe consisting of date in column one and flow in column two. Date must be in POSIX format (see ts.format).}
  \item{threshold}{
values below this threshold (default 0.1) are treated as zero for the purpose of defining cease to flow spells to account for the fact that cease to flow levels are poorly defined for many gauging sites.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A dataframe with 4 columns (see below).

\item{p.CTF }{Fraction time cease to flows occur}
\item{avg.CTF }{Average cease-to-flow spell duration}
\item{med.CTF }{Median cease-to-flow spell duration}
\item{min.CTF }{Minimum cease-to-flow spell duration}
\item{max.CTF }{Maximum cease-to-flow spell duration}

%% ...
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

CTF(Cooper)
CTF(Cooper, threshold=0)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
