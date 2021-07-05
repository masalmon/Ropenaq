#' Providing data about distinct measurement locations.
#'
#' @importFrom lubridate ymd ymd_hms
#'
#' @template country
#' @template city
#' @template location
#' @template parameter
#' @template has_geo
#' @template latitude
#' @template longitude
#' @template radius
#' @template limit
#' @template page
#' @template siteType
#' @template activationDate
#' @template completeness
#' @template inletHeight

#' @return  A results data.frame (dplyr "tbl_df") with 12 columns:
#'  \itemize{
#'  \item the name of the location ("location"),
#'  \item the city it is in ("city"),
#'  \item the code of country it is in ("country"),
#'  \item the name of the source of the information ("sourceName"),
#'  \item the number of measures for this location in the platform ("count"),
#'  \item the last time and date at which measures were updated for this location ("lastUpdated"),
#'  \item the first time and date at which measures were updated for this location ("firstUpdated"),
#'  \item the parameters available for this location ("parameters"),
#'  \item its longitude ("longitude") and latitude if available ("latitude"),
#'  \item and finally an URL encoded version of the city name ("cityURL")
#'  \item and of the location name ("locationURL").
#'  }
#' and two attributes, a meta data.frame (dplyr "tbl_df") with 1 line and 5 columns:
#' \itemize{
#' \item the API name ("name"),
#' \item the license of the data ("license"),
#' \item the website url ("website"),
#' \item the queried page ("page"),
#' \item the limit on the number of results ("limit"),
#' \item the number of results found on the platform for the query ("found")
#' }
#' and a timestamp data.frame (dplyr "tbl_df") with the query time and the last time at which the data was modified on the platform.
#' @details For queries involving a city or location argument,
#' the URL-encoded name of the city/location (as in cityURL/locationURL),
#' not its name, should be used.
#'  You can query any nested combination of country/location/city (level 1, 2 and 3),
#'  with only one value for each argument.
#'   If you write inconsistent combination such as city="Paris" and country="IN", an error message will be returned.
#'   If you write city="Delhi", you do not need to write the code of the country, unless
#'   one day there is a city with the same name in another country.
#' @export
#'
#' @examples
#' \dontrun{
#' aq_locations(city='Delhi', parameter='co')
#' }
aq_locations <- function(country = NULL, city = NULL, location = NULL,# nolint
                         parameter = NULL, has_geo = NULL, limit = 10000,
                         latitude = NULL, longitude = NULL, radius = NULL,
                         page = NULL, siteType = NULL, completeness_from = NULL,
                         completeness_to = NULL, activation_date_from = NULL,
                         activation_date_to = NULL, inletHeight_from = NULL, inletHeight_to = NULL){

  ####################################################
  # BUILD QUERY base URL
  urlAQ <- paste0(base_url(), "locations")

  argsList <- buildQuery(country = country, city = city,
                         location = location,
                         parameter = parameter,
                         has_geo = has_geo,
                         limit = limit,
                         latitude = latitude,
                         longitude = longitude,
                         radius = radius,
                         page = page,
                         siteType = siteType,
                         completeness_from = completeness_from,
                         completeness_to = completeness_to,
                         activation_date_from = activation_date_from,
                         activation_date_to = activation_date_to,
                         inletHeight_from = inletHeight_from,
                         inletHeight_to = inletHeight_to)

  return(argsList)

  ####################################################
  # GET AND TRANSFORM RESULTS
  output <- getResults(urlAQ, argsList)
  locationsTable <- output
  # if no results
  if (nrow(locationsTable) != 0){



    locationsTable <- functionParameters(resTable =
                                           locationsTable)


    locationsTable <- addCityURL(resTable =
                                   locationsTable)
    locationsTable <- addLocationURL(resTable =
                                       locationsTable)


    locationsTable <- functionTime(resTable =
                                     locationsTable,
                                   "firstUpdated")
    locationsTable <- functionTime(resTable = locationsTable,
                                   "lastUpdated")

    names(locationsTable) <- gsub("coordinates\\.",
                                  "",
                                  names(locationsTable))
  }
  ####################################################
  # DONE!
  attr(locationsTable, "meta") <- attr(output, "meta")
  attr(locationsTable, "timestamp") <- attr(output, "timestamp")

  return(locationsTable)

}
