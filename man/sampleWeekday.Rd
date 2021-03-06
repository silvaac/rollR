% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/utility.r
\name{sampleWeekday}
\alias{sampleWeekday}
\title{Resample a xts object into weekday}
\usage{
sampleWeekday(R, weekday = 2)
}
\arguments{
\item{R}{input xts time series.}

\item{weekday}{weekday to be as a number form 1:sunday to 7}
}
\value{
xts matrix with resampled time series
}
\description{
This function is approx xts::to.weekly but makes sure to use only mondays, ....
}
\examples{
sampleWeekday(xts(1:100,Sys.Date()+1:100))
}

