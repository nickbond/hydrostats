\name{lh}
\alias{lh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Single-pass Lynne Hollick filter
}
\description{
Applies a single pass of the Lynne Hollick Filter (Lynne & Hollick, 1979).}
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
Lynne, V., Hollick, M. (1979) Stochastic time-variable rainfall-runoff modelling. In: pp. 89–93 Institute of Engineers Australia National Conference. Perth.
}
\author{
Nick Bond
}
\note{
Generally used with \code{\link{bfi}}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(Cooper)
Cooper<-ts.format(Cooper)

lh(Cooper$Q, a=0.975)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
