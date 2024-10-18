#' Generate Persistent Global Identifiers (UUID)
#'
#' This function generates a persistent global identifier (UUID) for each record,
#' which can be used as a unique identifier for Darwin Core records or other data.
#'
#' @param n Integer, the number of UUIDs to generate. Default is 1.
#' @param namespace A character string representing a custom namespace. Default is NULL.
#' @return A vector of UUIDs.
#'
#' @importFrom uuid UUIDgenerate
#' @examples
#' # Generate a single UUID
#' generate_global_identifier()
#'
#' # Generate 5 UUIDs
#' generate_global_identifier(n = 5)
#'
#' @export
generate_global_identifier <- function(n = 1, namespace = NULL) {
  # Check if 'n' is a positive integer
  if (!is.numeric(n) || n <= 0) {
    stop("'n' must be a positive integer.")
  }

  # Generate UUIDs
  uuid_vector <- sapply(1:n, function(x) {
    if (!is.null(namespace)) {
      uuid::UUIDgenerate(namespace = namespace)
    } else {
      uuid::UUIDgenerate()
    }
  })

  return(uuid_vector)
}
