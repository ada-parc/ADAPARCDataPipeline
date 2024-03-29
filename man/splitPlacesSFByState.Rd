% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/splitPlacesSFByState.R
\name{splitPlacesSFByState}
\alias{splitPlacesSFByState}
\title{splitPlacesSFByState}
\usage{
splitPlacesSFByState(places, places_pop_est)
}
\arguments{
\item{places}{original dataframe of places SF}

\item{places_pop_est}{dataframe containing places and their population}
}
\value{
a dataframe of places, split by state code
}
\description{
Split a Places SF dataframe into a split dataframe according to states. Note: "Places" is a geography defined by the US Census. We filter according to population that is 3500+
}
