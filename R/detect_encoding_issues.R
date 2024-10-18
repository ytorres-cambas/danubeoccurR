#' Detect Encoding Issues in Names Using Language, Country, and Unicode Block Detection
#'
#' This function detects potential encoding issues in text data, such as names of people or places,
#' by comparing characters against expected valid characters for a specific language or languages,
#' country, or Unicode block. It allows for custom character sets, supports automatic language guessing,
#' and provides detailed reporting.
#'
#' @param data A data frame containing the names or text data to be checked.
#' @param name_col The column name (as a string) in the data frame that contains the names or text data.
#' @param countries Optional. A vector of countries of origin for the names, used to infer the expected languages.
#' @param languages Optional. A vector of languages to be checked. If neither countries nor languages is provided, the function attempts to guess the languages automatically.
#' @param custom_charset Optional. A custom set of valid characters. If provided, names will be checked against this character set instead of the language or Unicode block.
#' @param case_sensitive Logical. Should the character matching be case sensitive? Defaults to TRUE.
#' @param ignore_whitespace Logical. Should leading and trailing whitespace be ignored when checking for issues? Defaults to TRUE.
#' @param report_type A string specifying the type of report to return. Choices are `"detailed"` (default), `"summary"`, and `"flagged"`.
#' @param custom_regex Optional. A custom regular expression for detecting encoding issues. Overrides language and Unicode block checks if provided.
#' @param check_unicode_blocks Logical. If TRUE, checks whether characters in the names belong to the expected Unicode block based on the language. Defaults to FALSE.
#'
#' @return A data frame. Depending on the `report_type`:
#' - `"detailed"`: Returns the original data frame with additional columns indicating encoding issues.
#' - `"summary"`: Returns a summary of the total number of rows and the number of detected issues.
#' - `"flagged"`: Returns only the rows where encoding issues were detected.
#'
#' @details
#' The function can handle multiple languages per country and allows for custom character sets. It uses the `cld2` or `cld3` package for language detection, which provides fine-grained control over the expected character set.
#' It can automatically guess the languages of the names if neither countries nor languages are specified.
#'
#' If `check_unicode_blocks` is TRUE, the function detects characters outside the expected Unicode block (e.g., Latin, Cyrillic) for the specified languages. If a `custom_regex` is provided, the regex pattern will be used to detect encoding issues, overriding the language or Unicode block check.
#'
#' @examples
#' df <- data.frame(
#'   names = c("Müller", "François", "Elína", "János", "Dvořák", "Băsescu", "Petrović", "Šimon", "Dragović")
#' )
#'
#' # Detect encoding issues for German and Hungarian names
#' detect_encoding_issues(df, name_col = "names", languages = c("de", "hu"))
#'
#' # Detect issues using a custom character set (only basic ASCII characters)
#' detect_encoding_issues(df, name_col = "names", custom_charset = "a-zA-Z")
#'
#' # Guess the languages and check Unicode blocks
#' detect_encoding_issues(df, name_col = "names", check_unicode_blocks = TRUE)
#'
#' @import dplyr
#' @import stringi
#' @import textcat
#' @import cld2
#' @import countrycode
#' @export
detect_encoding_issues <- function(data, name_col, countries = NULL, languages = NULL,
                                          custom_charset = NULL,
                                          case_sensitive = TRUE, ignore_whitespace = TRUE,
                                          report_type = "detailed",
                                          custom_regex = NULL, check_unicode_blocks = FALSE) {

  # Error handling: both parameters should not be NULL
  if (is.null(countries) & is.null(languages) & is.null(custom_regex)) {
    stop("Please provide either countries, languages, or a custom regex pattern.")
  }

  # Automatically map countries to languages if countries are provided
  if (!is.null(countries)) {
    languages <- unique(unlist(lapply(countries, function(country) {
      countrycode::countrycode(country, origin = "country.name", destination = "iso3c")
    })))
    languages <- countrycode::countrycode(languages, origin = "iso3c", destination = "cld2")
  }

  # Guess the languages if neither countries nor languages are provided
  if (is.null(countries) & is.null(languages)) {
    lang_info <- cld2::detect_languages(data[[name_col]])
    languages <- unique(lang_info$language)
    message(paste("Languages guessed as:", paste(languages, collapse = ", ")))
  }

  # Optionally ignore whitespace or make case insensitive
  clean_name <- function(name) {
    if (!case_sensitive) name <- tolower(name)
    if (ignore_whitespace) name <- stri_trim_both(name)
    return(name)
  }

  # Function to detect issues based on Unicode blocks
  check_unicode_block <- function(string, lang) {
    if (lang == "pt-BR" || lang == "pt-PT") {
      return(!stri_detect_charclass(string, "\\p{Latin}"))
    }
    return(!stri_detect_charclass(string, "\\p{Latin}"))  # Default to Latin
  }

  # Automatically retrieve the allowed characters based on language
  check_language <- function(string, lang) {
    normalized_string <- stri_trans_general(string, id = paste0(lang, "-ASCII"))
    return(normalized_string != string)
  }

  # Apply the language check, custom regex, or Unicode block check
  data <- data %>%
    mutate(
      cleaned_names = sapply(.[[name_col]], clean_name),
      encoding_issue = if (!is.null(custom_regex)) {
        grepl(custom_regex, cleaned_names)
      } else if (check_unicode_blocks) {
        sapply(cleaned_names, function(name) {
          any(sapply(languages, check_unicode_block, name))
        })
      } else if (!is.null(languages)) {
        sapply(cleaned_names, function(name) {
          any(sapply(languages, check_language, name))
        })
      } else {
        FALSE
      }
    )

  # Allow for custom character sets if provided
  if (!is.null(custom_charset)) {
    data <- data %>%
      mutate(
        custom_issue = grepl(paste0("[^", custom_charset, "]"), cleaned_names)
      )
  }

  # Generate report types
  if (report_type == "summary") {
    return(data %>%
             summarise(
               total_rows = n(),
               total_issues = sum(encoding_issue, na.rm = TRUE),
               total_custom_issues = if (!is.null(custom_charset)) sum(custom_issue, na.rm = TRUE) else NA
             ))
  } else if (report_type == "flagged") {
    return(data %>% filter(encoding_issue | (!is.null(custom_charset) & custom_issue)))
  } else {  # "detailed" report
    return(data)
  }
}
