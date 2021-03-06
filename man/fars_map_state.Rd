% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Graphical map of state fatalities}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{The state number as integer, or character to be coerced}

\item{year}{The year as an integer}
}
\value{
A plot rendered and shown
}
\description{
This function produces a graphical map of the state and shows the location of motor vehicle
fatalities in the year. The map is then auto plotted to the plot window.
}
\details{
The data for the year is first read in and a check is made to determine if state data
exists. If this data is available it is filtered and, if accidents have occurred, their locations
are plotted on the state map. If NA's exist in the LONGITUD or LATITUDE columns they
are imputed.
}
\note{
Year cannot be vectorized or this function will produce an error

If the state number is invalid or doesn't exist in the data this function will produce an error

If there are no accidents in the state, a message will be produced and no plot will be rendered
}
\examples{
fars_map_state(1,2015)

}
