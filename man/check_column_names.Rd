% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_column_names.R
\name{check_column_names}
\alias{check_column_names}
\title{Check Data Frame Column Names Against Darwin Core Standard Terms}
\usage{
check_column_names(
  df_input,
  standard_terms = dwc_names,
  verbose = TRUE,
  merge = FALSE
)
}
\arguments{
\item{df_input}{A data frame or a list of data frames to check.}

\item{standard_terms}{A character vector containing the standard column names (default is \code{dwc_names}).}

\item{verbose}{A logical value indicating whether to output the columns that do not match the standard terms (default is TRUE).}

\item{merge}{A logical value indicating whether to merge data frames if all have the same names (default is FALSE).}
}
\value{
If merging is enabled and all data frames have the same column names, it returns a merged data frame.
If verbosity is enabled, it prints out any columns that do not match the standard terms.
}
\description{
This function checks that the column names of a data frame (or multiple data frames)
are part of a set of standard terms from the Darwin Core. It will output the columns
that do not follow the standard names and can merge data frames if specified.
}
\examples{
# Example usage with a single data frame
check_column_names(fish_data)

# Example usage with a list of data frames and same column names
check_column_names((list(fish_data, fish_data),merge = T))

# Example usage with a list of data frames and different column names
fish_data1 <- fish_data
fish_data2 <- fish_data \%>\%
rename(latitude = decimalLatitude, longitude = decimalLongitude)
check_column_names(list(fish_data1, fish_data2), merge = T)

}
