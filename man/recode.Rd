\name{recode}
\alias{recode}
\title{recode}
\description{
Recodes values in a vector based on original and new values provided as two vectors.
}
\usage{
recode(x, oldvalue, newvalue)}
\arguments{
  \item{x}{
A vector with values to be replaced.}
	\item{oldvalue}{
A vector of original values to be recoded.
}
	\item{newvalue}{
A vector of replacement values of the same length as oldvalue.
}
}

\value{
A vector of same length as input.
}


\author{
Nick Bond <n.bond@griffith.edu.au>
}
\examples{
x<-seq(1:10)
recode(x, c(1,5,10), c(-1,-5,-10))
}
