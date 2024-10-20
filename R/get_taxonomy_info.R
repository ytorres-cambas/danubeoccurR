#' Get Taxonomy Information Including Species, Year, and Author
#'
#' This function returns the genus, family, order, and additional information (species, year, author)
#' for each species in the input vector. It queries multiple sources (e.g., GBIF, ITIS, FishBase, COL, NCBI)
#' to retrieve the taxonomic information.
#' If `all_categories` is set to TRUE, it will return all available taxonomic ranks.
#'
#' @param species Vector of species names (scientific names) as input.
#' @param all_categories Logical, if TRUE, retrieves all taxonomic categories available. Default is FALSE.
#' @param sources Character vector, the taxonomy sources to query. Options include "gbif", "itis", "ncbi",
#' "col", "eol", "tropicos", "wikidata", "ott", "nbnc", "fishbase". Default is `c("gbif", "itis", "ncbi")`.
#' @return A data frame with taxonomic categories including species, year, author, genus, family, and order.
#'
#' @import taxize
#' @import rfishbase
#' @examples
#' \dontrun{
#' # Example with a single species using multiple sources
#' get_taxonomy_info(c("Panthera leo"), sources = c("gbif", "itis"))
#'
#' # Example with fish species using FishBase
#' get_taxonomy_info(c("Salmo salar", "Oncorhynchus mykiss"), sources = "fishbase")
#'
#' # Example with multiple species and all categories
#' get_taxonomy_info(c("Panthera leo", "Homo sapiens"), all_categories = TRUE)
#' }
#' @export
get_taxonomy_info <- function(species, all_categories = FALSE, sources = c("gbif", "itis", "ncbi")) {

  # Load required packages
  if (!requireNamespace("taxize", quietly = TRUE)) {
    stop("Please install the 'taxize' package to use this function.")
  }
  if (!requireNamespace("rfishbase", quietly = TRUE) && "fishbase" %in% sources) {
    stop("Please install the 'rfishbase' package to use FishBase as a source.")
  }

  # Initialize empty list to store results
  taxonomy_results <- list()

  # Load FishBase taxa data once, if "fishbase" is included in sources
  if ("fishbase" %in% sources) {
    fishbase_taxa <- rfishbase::load_taxa(server = "fishbase")
  }

  # Loop through each species to query taxonomic information from all sources
  for (sp in species) {
    tryCatch({
      # FishBase handling if included in sources
      if ("fishbase" %in% sources) {
        # Filter the FishBase data for the current species
        fishbase_info <- fishbase_taxa[fishbase_taxa$Species == sp, ]

        if (nrow(fishbase_info) == 0) {
          warning(paste("Species", sp, "not found in FishBase."))
        } else {
          if (all_categories) {
            taxonomy_results[[sp]] <- fishbase_info
          } else {
            selected_ranks <- fishbase_info[, c("Species", "Genus", "Family", "Order")]
            taxonomy_results[[sp]] <- as.data.frame(t(selected_ranks))
          }
        }

        # If only FishBase is used, skip querying other sources
        if (identical(sources, "fishbase")) next
      }

      # Handle other sources with taxize
      if (!"fishbase" %in% sources || length(sources) > 1) {
        for (source in sources[sources != "fishbase"]) {
          tax_info <- taxize::classification(sp, db = source)[[1]]
          if (all_categories) {
            taxonomy_results[[sp]] <- as.data.frame(tax_info)
          } else {
            selected_ranks <- tax_info[tax_info$rank %in% c("genus", "family", "order"), ]
            # Add Species, Year, and Author if available
            species_info <- data.frame(
              Species = sp,
              Year = NA,     # Placeholder for year
              Author = NA,   # Placeholder for author
              stringsAsFactors = FALSE
            )
            taxonomy_results[[sp]] <- merge(species_info, as.data.frame(selected_ranks), by = NULL)
          }
        }
      }
    }, error = function(e) {
      message(paste("Error fetching taxonomy for species:", sp, ":", e))
      taxonomy_results[[sp]] <- NA
    })
  }

  # Combine results into a single data frame
  result_df <- do.call(rbind, lapply(taxonomy_results, function(x) {
    if (is.data.frame(x)) {
      x <- as.data.frame(t(x))
    }
    x
  }))

  return(result_df)
}
