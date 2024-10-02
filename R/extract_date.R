#' Extract Day, Month, and Year from Date Column
#'
#' This function takes a column of dates that can be in various common formats,
#' and extracts the day, month, and year, returning them in a data frame.
#'
#' @param date_col A character vector representing the dates in various formats.
#' @param date_format Optional. A character string specifying the date format ("dmy" or "mdy") to resolve ambiguities.
#' @import lubridate
#' @return A data frame with three columns: day, month, and year.
#' @export
#'
#' @examples
#' # mdy format
#' dates <- c("October 2, 2024", "10-02-2024", "10/02/2024")
#' result <- extract_date(dates, date_format = "mdy")
#' print(result)

# dmy format
#' dates <- c("2 October 2024", "02-10-2024", "02/10/2024")
#' result <- extract_date(dates, date_format = "dmy")
#' print(result)

extract_date <- function(date_col, date_format = "dmy") {
  # Load required library
  # if (!requireNamespace("lubridate", quietly = TRUE)) {
  #   stop("The 'lubridate' package is required but not installed. Please install it.")
  # }

  # Initialize vectors to hold extracted components
  days <- numeric(length(date_col))
  months <- numeric(length(date_col))
  years <- numeric(length(date_col))

  # Adjust the date parsing format according to the specified date_format
  parse_orders <- switch(date_format,
                         "dmy" = c("dmy", "dmy HMS"),
                         "mdy" = c("mdy", "mdy HMS"),
                         c("ymd", "ymd HMS", "dmy", "mdy"))

  # Loop through each date and extract components
  for (i in seq_along(date_col)) {
    # Try parsing the date based on the orders provided
    date <- lubridate::parse_date_time(date_col[i], orders = parse_orders)

    if (!is.na(date)) {
      days[i] <- lubridate::day(date)
      months[i] <- lubridate::month(date)
      years[i] <- lubridate::year(date)
    } else {
      days[i] <- NA
      months[i] <- NA
      years[i] <- NA
    }
  }

  # Create a data frame with the results
  result <- data.frame(day = days, month = months, year = years, stringsAsFactors = FALSE)

  return(result)
}
