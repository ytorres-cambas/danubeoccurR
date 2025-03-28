% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_global_identifier.R
\name{generate_global_identifier}
\alias{generate_global_identifier}
\title{Generate Persistent Global Identifiers (UUID)}
\usage{
generate_global_identifier(n = 1, namespace = NULL)
}
\arguments{
\item{n}{Integer, the number of UUIDs to generate. Default is 1.}

\item{namespace}{A character string representing a custom namespace. Default is NULL.}
}
\value{
A vector of UUIDs.
}
\description{
This function generates a persistent global identifier (UUID) for each record,
which can be used as a unique identifier for Darwin Core records or other data.
}
\examples{
# Generate a single UUID
generate_global_identifier()

# Generate 5 UUIDs
generate_global_identifier(n = 5)

}
