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
#'
#' @source <https://www.gbif.org/>
"fish_data"

#' Darwin Core Terms for Occurrence Datasets
#'
#' This vector contains column names that conform to the Darwin Core Standard,
#' which are commonly used in occurrence datasets, particularly from GBIF.
#'
#' @format ## `dwc_names`
#' A character vector with the following terms:
#' \describe{
#'   \item{occurrenceID}{A unique identifier for the occurrence record}
#'   \item{basisOfRecord}{The type of record (e.g., HumanObservation, MachineObservation)}
#'   \item{catalogNumber}{An identifier for the record within the dataset or collection}
#'   \item{recordNumber}{A number assigned by the collector of the record}
#'   \item{recordedBy}{A list of names of people, groups, or organizations responsible for recording the occurrence}
#'   \item{individualCount}{The number of individuals observed}
#'   \item{sex}{The sex of the individuals represented in the occurrence}
#'   \item{lifeStage}{The life stage of the individuals (e.g., adult, juvenile)}
#'   \item{establishmentMeans}{The process by which the organism became established at the location (e.g., native, introduced)}
#'   \item{occurrenceStatus}{The status of the occurrence (e.g., present, absent)}
#'   \item{preparations}{A list of preparations (e.g., dry specimen, DNA extract)}
#'   \item{disposition}{The current state of the specimen (e.g., in collection, missing)}
#'   \item{associatedMedia}{URLs to images, videos, or sound recordings}
#'   \item{associatedOccurrences}{A list of other occurrence records associated with this one}
#'   \item{associatedReferences}{A list of references associated with the occurrence}
#'   \item{associatedSequences}{A list of genetic sequences associated with the occurrence}
#'   \item{associatedTaxa}{A list of taxa associated with the occurrence}
#'   \item{eventID}{An identifier for the event}
#'   \item{parentEventID}{An identifier for the broader event associated with this one}
#'   \item{samplingProtocol}{The method or protocol used to collect the data}
#'   \item{samplingEffort}{The amount of effort expended during the event (e.g., time spent, area surveyed)}
#'   \item{eventDate}{The date of the event}
#'   \item{eventTime}{The time of the event}
#'   \item{startDayOfYear}{The start of the event in terms of day of the year}
#'   \item{endDayOfYear}{The end of the event in terms of day of the year}
#'   \item{year}{The year of the event}
#'   \item{month}{The month of the event}
#'   \item{day}{The day of the event}
#'   \item{verbatimEventDate}{The original event date string before parsing}
#'   \item{locationID}{An identifier for the location}
#'   \item{higherGeographyID}{An identifier for the higher-level geography}
#'   \item{higherGeography}{A string describing the higher-level geography}
#'   \item{continent}{The name of the continent}
#'   \item{waterBody}{The name of the water body}
#'   \item{islandGroup}{The name of the island group}
#'   \item{island}{The name of the island}
#'   \item{country}{The full name of the country}
#'   \item{countryCode}{The ISO 3166-1 alpha-2 code of the country}
#'   \item{stateProvince}{The name of the state or province}
#'   \item{county}{The name of the county or similar subdivision}
#'   \item{municipality}{The name of the municipality}
#'   \item{locality}{The specific locality of the event}
#'   \item{verbatimLocality}{The original locality string}
#'   \item{verbatimCoordinates}{The original coordinates string}
#'   \item{verbatimLatitude}{The latitude as originally provided}
#'   \item{verbatimLongitude}{The longitude as originally provided}
#'   \item{verbatimCoordinateSystem}{The coordinate system as originally provided}
#'   \item{decimalLatitude}{The latitude of the occurrence}
#'   \item{decimalLongitude}{The longitude of the occurrence}
#'   \item{coordinateUncertaintyInMeters}{The uncertainty in the coordinates, in meters}
#'   \item{coordinatePrecision}{The precision of the coordinates}
#'   \item{pointRadiusSpatialFit}{The spatial fit for a radius around the coordinates}
#'   \item{footprintWKT}{The Well-Known Text (WKT) representation of the footprint}
#'   \item{footprintSRS}{The Spatial Reference System for the footprint}
#'   \item{footprintSpatialFit}{The spatial fit of the footprint}
#'   \item{georeferencedBy}{The name of the person or group who georeferenced the occurrence}
#'   \item{georeferencedDate}{The date when the georeferencing was performed}
#'   \item{georeferenceProtocol}{The protocol used to determine the coordinates}
#'   \item{georeferenceSources}{The sources used to determine the coordinates}
#'   \item{georeferenceVerificationStatus}{The status of the georeference verification}
#'   \item{georeferenceRemarks}{Any additional information about the georeferencing process}
#'   \item{scientificName}{The full scientific name including authorship}
#'   \item{acceptedNameUsage}{The currently accepted name for the taxon}
#'   \item{taxonID}{An identifier for the taxon}
#'   \item{acceptedTaxonID}{An identifier for the currently accepted taxon}
#'   \item{parentNameUsageID}{An identifier for the parent taxon}
#'   \item{scientificNameID}{An identifier for the scientific name}
#'   \item{kingdom}{The kingdom of the organism}
#'   \item{phylum}{The phylum of the organism}
#'   \item{class}{The class of the organism}
#'   \item{order}{The order of the organism}
#'   \item{family}{The family of the organism}
#'   \item{genus}{The genus of the organism}
#'   \item{specificEpithet}{The specific epithet of the organism}
#'   \item{infraspecificEpithet}{The infraspecific epithet of the organism}
#'   \item{taxonRank}{The taxonomic rank of the most specific name in the scientificName}
#'   \item{verbatimTaxonRank}{The rank as originally provided}
#'   \item{scientificNameAuthorship}{The authorship information for the scientificName}
#'   \item{vernacularName}{The common name associated with the taxon}
#'   \item{nomenclaturalCode}{The code governing the scientific name}
#'   \item{taxonomicStatus}{The status of the name (e.g., accepted, synonym)}
#'   \item{nomenclaturalStatus}{The nomenclatural status (e.g., valid, invalid)}
#'   \item{identificationID}{An identifier for the identification event}
#'   \item{identificationQualifier}{A brief statement about the uncertainty of the identification}
#'   \item{typeStatus}{The type status of the specimen (e.g., holotype, syntype)}
#'   \item{identifiedBy}{The person who identified the organism}
#'   \item{dateIdentified}{The date the identification was made}
#'   \item{identificationRemarks}{Remarks about the identification process}
#'   \item{datasetID}{An identifier for the dataset}
#'   \item{datasetName}{The name of the dataset}
#'   \item{institutionCode}{The code identifying the institution that holds or publishes the data}
#'   \item{collectionCode}{The code identifying the collection from which the data came}
#'   \item{ownerInstitutionCode}{The code identifying the institution that owns the data}
#'   \item{rightsHolder}{The entity that holds the rights over the data}
#'   \item{license}{The license under which the data is provided}
#'   \item{rights}{Information about the rights associated with the data}
#'   \item{accessRights}{Information about who can access the data}
#'   \item{bibliographicCitation}{A citation for the resource as a whole}
#'   \item{references}{A URL or identifier to a related resource or publication}
#' }
#'
#' @source <https://dwc.tdwg.org/terms/>
"dwc_names"


