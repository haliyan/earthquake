% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/earthquake_data_cleaning.R
\name{bc_date}
\alias{bc_date}
\title{A function to deal with BC dates}
\usage{
bc_date(y, m = 1, d = 1)
}
\arguments{
\item{y}{A year number as a character string}

\item{m}{A month number (1-12) as a character string. Defaults to 1
(January)}

\item{d}{A day number as a character string. Defaults to 1 (the 1st).}
}
\value{
A BC/BCE date of class Date in y-m-d format
}
\description{
This function handles BC/BCE or "negative" dates.
It takes as input a BC year, month, and day. If no month and day are
inputted, the month defaults to the first month (January) and the day
defaults to the first (1st).
The function calculates the BC year by subtracting the positive
version of the inputted negative year from the year 0000.
}
\examples{
\dontrun{bc_date(y=-2000)}
\dontrun{bc_date(y=-611, m=2)}

}
