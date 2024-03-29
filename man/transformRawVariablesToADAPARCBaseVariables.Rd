% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transformRawVariablesToADAPARCBaseVariables.R
\name{transformRawVariablesToADAPARCBaseVariables}
\alias{transformRawVariablesToADAPARCBaseVariables}
\title{transformRawVariablesToADAPARCBaseVariables}
\usage{
transformRawVariablesToADAPARCBaseVariables(
  ada_parc_base_to_source_var_map,
  raw_data,
  source
)
}
\arguments{
\item{ada_parc_base_to_source_var_map}{dataframe with ADA-PARC base variables mapped to source variables}

\item{raw_data}{dataframe with raw data from source}

\item{source}{source of data (ACS, HUD, etc.)}
}
\value{
a dataframe with ADA-PARC base data across all years
}
\description{
Transform Raw Variables to ADA-PARC Base Variables. This function assumes \code{GEOID}, \code{NAME}, \code{ABBR}, \code{year}as column names.
}
