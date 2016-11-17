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
Dataframe with date and discharge data in columns named "Date" and "Q" respectively. Date must be in POSIX format (see ts.format).}
  \item{threshold}{
values below this threshold (default 0.1) are treated as zero for the purpose of defining cease to flow spells to account for the fact that cease to flow levels are poorly defined for many gauging sites.}
}

\value{
A dataframe with 5 columns (see below).

\item{p.CTF }{Fraction time cease to flows occur}
\item{avg.CTF }{Average cease-to-flow spell duration}
\item{med.CTF }{Median cease-to-flow spell duration}
\item{min.CTF }{Minimum cease-to-flow spell duration}
\item{max.CTF }{Maximum cease-to-flow spell duration}

}

\author{
Nick Bond <n.bond@latrobe.edu.au>
}

\examples{
data(Cooper)
Cooper<-ts.format(Cooper)

CTF(Cooper)
CTF(Cooper, threshold=0)

}
