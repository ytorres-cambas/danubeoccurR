## code to prepare `dwc_names` dataset

dwc_names <- c(
  # Occurrence-related terms
  "occurrenceID",                        # A unique identifier for the occurrence record
  "basisOfRecord",                       # The type of record (e.g., HumanObservation, MachineObservation)
  "catalogNumber",                       # An identifier for the record within the dataset or collection
  "recordNumber",                        # A number assigned by the collector of the record
  "recordedBy",                          # A list of names of people, groups, or organizations responsible for recording the occurrence
  "recordedByID",                        # An identifier for the person or group who recorded the occurrence
  "individualCount",                     # The number of individuals observed
  "individualID",                        # An identifier for an individual organism
  "organismQuantity",                    # A quantity or amount of organisms
  "organismQuantityType",                # The type of measurement for the quantity of organisms
  "sex",                                 # The sex of the individuals represented in the occurrence
  "lifeStage",                           # The life stage of the individuals (e.g., adult, juvenile)
  "establishmentMeans",                  # The process by which the organism became established at the location (e.g., native, introduced)
  "occurrenceStatus",                    # The status of the occurrence (e.g., present, absent)
  "preparations",                        # A list of preparations (e.g., dry specimen, DNA extract)
  "disposition",                         # The current state of the specimen (e.g., in collection, missing)
  "associatedMedia",                     # URLs to images, videos, or sound recordings
  "associatedOccurrences",               # A list of other occurrence records associated with this one
  "associatedReferences",                # A list of references associated with the occurrence
  "associatedSequences",                 # A list of genetic sequences associated with the occurrence
  "associatedTaxa",                      # A list of taxa associated with the occurrence
  "materialSampleID",                    # An identifier for the material sample
  "occurrenceRemarks",                   # Comments or notes about the occurrence

  # Event-related terms
  "eventID",                             # An identifier for the event
  "parentEventID",                       # An identifier for the broader event associated with this one
  "samplingProtocol",                    # The method or protocol used to collect the data
  "samplingEffort",                      # The amount of effort expended during the event (e.g., time spent, area surveyed)
  "eventDate",                           # The date of the event
  "eventTime",                           # The time of the event
  "startDayOfYear",                      # The start of the event in terms of day of the year
  "endDayOfYear",                        # The end of the event in terms of day of the year
  "year",                                # The year of the event
  "month",                               # The month of the event
  "day",                                 # The day of the event
  "verbatimEventDate",                   # The original event date string before parsing

  # Location-related terms
  "locationID",                          # An identifier for the location
  "higherGeographyID",                   # An identifier for the higher-level geography
  "higherGeography",                     # A string describing the higher-level geography
  "continent",                           # The name of the continent
  "waterBody",                           # The name of the water body
  "islandGroup",                         # The name of the island group
  "island",                              # The name of the island
  "country",                             # The full name of the country
  "countryCode",                         # The ISO 3166-1 alpha-2 code of the country
  "stateProvince",                       # The name of the state or province
  "county",                              # The name of the county or similar subdivision
  "municipality",                        # The name of the municipality
  "locality",                            # The specific locality of the event
  "verbatimLocality",                    # The original locality string
  "verbatimCoordinates",                 # The original coordinates string
  "verbatimLatitude",                    # The latitude as originally provided
  "verbatimLongitude",                   # The longitude as originally provided
  "verbatimCoordinateSystem",            # The coordinate system as originally provided
  "decimalLatitude",                     # The latitude of the occurrence
  "decimalLongitude",                    # The longitude of the occurrence
  "coordinateUncertaintyInMeters",       # The uncertainty in the coordinates, in meters
  "coordinatePrecision",                 # The precision of the coordinates
  "geodeticDatum",                       # The geodetic datum used for the coordinates
  "pointRadiusSpatialFit",               # The spatial fit for a radius around the coordinates
  "footprintWKT",                        # The Well-Known Text (WKT) representation of the footprint
  "footprintSRS",                        # The Spatial Reference System for the footprint
  "footprintSpatialFit",                 # The spatial fit of the footprint
  "locationRemarks",                     # Comments or notes about the location
  "georeferencedBy",                     # The name of the person or group who georeferenced the occurrence
  "georeferencedDate",                   # The date when the georeferencing was performed
  "georeferenceProtocol",                # The protocol used to determine the coordinates
  "georeferenceSources",                 # The sources used to determine the coordinates
  "georeferenceVerificationStatus",      # The status of the georeference verification
  "georeferenceRemarks",                 # Any additional information about the georeferencing process

  # Taxon-related terms
  "scientificName",                      # The full scientific name including authorship
  "acceptedNameUsage",                   # The currently accepted name for the taxon
  "taxonID",                             # An identifier for the taxon
  "acceptedTaxonID",                     # An identifier for the currently accepted taxon
  "parentNameUsageID",                   # An identifier for the parent taxon
  "scientificNameID",                    # An identifier for the scientific name
  "kingdom",                             # The kingdom of the organism
  "phylum",                              # The phylum of the organism
  "class",                               # The class of the organism
  "order",                              # The order of the organism
  "family",                              # The family of the organism
  "genus",                               # The genus of the organism
  "specificEpithet",                     # The specific epithet of the organism
  "infraspecificEpithet",                # The infraspecific epithet of the organism
  "taxonRank",                           # The taxonomic rank of the most specific name in the scientificName
  "verbatimTaxonRank",                   # The rank as originally provided
  "scientificNameAuthorship",            # The authorship information for the scientificName
  "vernacularName",                      # The common name associated with the taxon
  "nomenclaturalCode",                   # The code governing the scientific name
  "taxonomicStatus",                     # The status of the name (e.g., accepted, synonym)
  "nomenclaturalStatus",                 # The nomenclatural status (e.g., valid, invalid)
  "verbatimIdentification",              # The taxon name as it appears in the original record, exactly as provided

  # Identification-related terms
  "identificationID",                    # An identifier for the identification event
  "identificationQualifier",             # A brief statement about the uncertainty of the identification
  "typeStatus",                          # The type status of the specimen (e.g., holotype, syntype)
  "identifiedBy",                        # The person who identified the organism
  "dateIdentified",                      # The date the identification was made
  "identificationRemarks",               # Remarks about the identification process

  # Data resource-related terms
  "datasetID",                           # An identifier for the dataset
  "datasetName",                         # The name of the dataset
  "institutionCode",                     # The code identifying the institution that holds or publishes the data
  "collectionCode",                      # The code identifying the collection from which the data came
  "ownerInstitutionCode",                # The code identifying the institution that owns the data
  "rightsHolder",                        # The entity that holds the rights over the data
  "license",                             # The license under which the data is provided
  "rights",                              # Information about the rights associated with the data
  "accessRights",                        # Information about who can access the data
  "bibliographicCitation",               # A citation for the resource as a whole
  "references"                           # A URL or identifier to a related resource or publication
)





usethis::use_data(dwc_names, overwrite = TRUE)
