#' Translate text to English using LibreTranslate
#'
#' This function detects the language of a given text and translates it to English
#' using the LibreTranslate API. If the text is already in English, no translation
#' is performed.
#'
#' @param text A character string of text to be translated.
#'
#' @return A character string containing the text translated to English (or the
#'   original text if it is already in English).
#'
#' @import httr
#' @examples
#' \dontrun{
#' # Translate a Spanish phrase to English
#' translated_text <- translate_to_english("Buenos días")
#' print(translated_text)  # Expected output: "Good morning"
#' }
#' @export
translate_to_english <- function(text) {
  # Skip translation if the text is NA
  if (is.na(text)) {
    return(text)
  }

  # Call LibreTranslate API to detect language
  url <- "https://libretranslate.com/detect"

  # Detect the language of the input text
  response <- httr::POST(url, body = list(
    q = text
  ), encode = "json")

  # Check if the response was successful
  if (httr::status_code(response) != 200) {
    stop("Error in language detection API request")
  }

  # Parse the response
  content_response <- httr::content(response, "parsed", simplifyVector = TRUE)

  # Ensure the response contains the expected data
  if (!is.null(content_response$language)) {
    detected_lang <- content_response$language
  } else {
    stop("Failed to detect the language of the text.")
  }

  # If the detected language is already English, return the original text
  if (detected_lang == "en") {
    return(text)
  }

  # Otherwise, call LibreTranslate API to translate the text to English
  translate_url <- "https://libretranslate.com/translate"

  translation_response <- httr::POST(translate_url, body = list(
    q = text,
    source = detected_lang,  # Use the detected language
    target = "en"            # Translate to English
  ), encode = "json")

  # Check if the translation request was successful
  if (httr::status_code(translation_response) != 200) {
    stop("Error in translation API request")
  }

  # Parse the translated response
  translated_text <- httr::content(translation_response, "parsed", simplifyVector = TRUE)

  # Ensure the translated text is available
  if (!is.null(translated_text$text)) {
    return(translated_text$text)
  } else {
    stop("Failed to get translated text.")
  }
}
