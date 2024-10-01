#' Danube River Basin
#'
#' A polygon of the Danube River Basin
#'
#' @format ## `danube_basin`
#' A simple feature collection with 1 feature and o fields
#' Geometry type: MULTIPOLYGON
#' Bounding box:  xmin: 8.1525 ymin: 42.08333 xmax: 29.73583 ymax: 50.245
#' Geodetic CRS:  WGS 84
#'
#' @source <https://hydrography.org/>
"danube_basin"


#' Danube River Basin
#'
#' A polygon of the Danube River Basin. Made with QGIS.
#'
#' @format ## `bbox_danube_basin`
#' A simple feature collection with 1 feature and o fields
#' Geometry type: MULTIPOLYGON
#' Bounding box:  xmin: 8.1525 ymin: 42.08333 xmax: 29.73583 ymax: 50.245
#' Geodetic CRS:  WGS 84
#'
"bbox_danube_basin"


#' Fish species occurrence records
#'
#' Occurrence records of Hucho hucho, Alburnoides bipunctatus and
#' Chondrostoma nasus downloaded from GBIF
#'
#' @format ## `fish_data`
#' A data frame with 1685 rows and 50 columns:
#' \describe{
#'   \item{gbifID}{Unique GBIF occurrence identifier.}
#'   \item{datasetKey}{UUID identifying the dataset in GBIF.}
#'   \item{occurrenceID}{An identifier for the occurrence (often globally unique).}
#'   \item{kingdom}{The kingdom to which the organism belongs.}
#'   \item{phylum}{The phylum classification of the organism.}
#'   \item{class}{The class of the organism.}
#'   \item{order}{The order of the organism.}
#'   \item{family}{The family of the organism.}
#'   \item{genus}{The genus of the organism.}
#'   \item{species}{The species name of the organism.}
#'   \item{infraspecificEpithet}{The infraspecific epithet, such as subspecies or variety.}
#'   \item{taxonRank}{The taxonomic rank of the organism.}
#'   \item{scientificName}{The full scientific name including the genus and species.}
#'   \item{verbatimScientificName}{The original scientific name as recorded in the data.}
#'   \item{verbatimScientificNameAuthorship}{The original authorship of the scientific name.}
#'   \item{countryCode}{The ISO country code where the occurrence was recorded.}
#'   \item{locality}{The locality description for where the occurrence was recorded.}
#'   \item{stateProvince}{The state or province where the occurrence was recorded.}
#'   \item{occurrenceStatus}{The status of the occurrence (e.g., present, absent).}
#'   \item{individualCount}{The number of individuals recorded.}
#'   \item{publishingOrgKey}{UUID identifying the organization that published the data.}
#'   \item{decimalLatitude}{The latitude where the occurrence was recorded, in decimal degrees.}
#'   \item{decimalLongitude}{The longitude where the occurrence was recorded, in decimal degrees.}
#'   \item{coordinateUncertaintyInMeters}{Uncertainty of the coordinates in meters.}
#'   \item{coordinatePrecision}{Precision of the coordinates provided.}
#'   \item{elevation}{The elevation where the occurrence was recorded, in meters.}
#'   \item{elevationAccuracy}{Accuracy of the elevation measurement, in meters.}
#'   \item{depth}{The depth at which the occurrence was recorded, in meters.}
#'   \item{depthAccuracy}{Accuracy of the depth measurement, in meters.}
#'   \item{eventDate}{The date the occurrence was recorded.}
#'   \item{day}{The day the occurrence was recorded.}
#'   \item{month}{The month the occurrence was recorded.}
#'   \item{year}{The year the occurrence was recorded.}
#'   \item{taxonKey}{The GBIF taxonomic key for the taxon.}
#'   \item{speciesKey}{The GBIF taxonomic key for the species.}
#'   \item{basisOfRecord}{The basis of the record (e.g., HumanObservation, MachineObservation).
#'   }
#

#'
#' @source <https://www.gbif.org/>
"fish_data"
