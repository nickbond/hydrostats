\name{ts.format}
\alias{ts.format}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Format dates as POSIXct
}
\description{
Converts dates from class character (format dd/mm/yyyy or other as specified) into class POSIXct and renames columns for use with other functions in the hydrostats package.}
\usage{
ts.format(x, format="\%d/\%m/\%Y", cols=c(1,2))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Dataframe including date and discharge data. Dates are assumed to be of class character (see format). The columns containing date and discharge data are required (defaults to renaming columns 1 and 2 to Date and Q respectively if no other columns are specified (see cols)).}
  \item{format}{
Format of dates in existing data frame.}
  \item{cols}{
A vector of column indices for the date and discharge data. Used to rename columns.}
}

\details{
Default assumes the date is of class character and in the first column, with discharge in the second column of the data frame. These columns can be specified if the defaults are not appropriate. The date and discharge columns are renamed to 'Date' and 'Q' respectively. For more flexibility in formatting dates/times see the lubridate package.}
\value{
A dataframe with the dates formatted as POSIXct and named columns Date and Q.  
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Nick Bond <n.bond@griffith.edu.au>
}
\examples{
data(Cooper)
Cooper<-ts.format(Cooper)
}
