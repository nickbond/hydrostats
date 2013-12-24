\name{lh3}
\alias{lh3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Helper function to bfi}
\description{
Applies 3 repeat passes (forward, back, forward) of the Lynne Hollick filter (Lynne & Hollick, 1979) to determine baseflow contribution.
}
\usage{
lh3(Q, a)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Q}{
Time series of river flows}
  \item{a}{
alpha value }
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
Cooper<-ts.format(Cooper)

lh3(Cooper[,2], a=0.975)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
