#' @importFrom stats setNames
######################################################################################
# base URL for all queries
base_url <- function() {
  "https://api.openaq.org/v1/"
}

######################################################################################
# checks arguments
# and if no error returns their list
buildQuery <- function(country = NULL, city = NULL, location = NULL,
                       parameter = NULL, has_geo = NULL, date_from = NULL,
                       date_to = NULL, value_from = NULL,
                       value_to = NULL, limit = NULL,
                       latitude = NULL, longitude = NULL,
                       attribution = NULL,
                       averaging_period = NULL,
                       source_name = NULL,
                       radius = NULL,
                       page = NULL){

  # limit
  if (!is.null(limit)) {
    if (limit > 10000) {
      stop(call. = FALSE, "limit cannot be more than 10,000")
    }
  }

  # location
  if (!is.null(location)) {

    locations <- aq_locations(country = country,
                           city = city)

    if (!(location %in% locations$locationURL)) {# nolint
      stop(call. = FALSE, "This location/city/country combination is not available within the platform. See ?aq_locations")# nolint
    }
    # make sure it won't be re-encoded
    Encoding(location) <- "UTF-8"
    class(location) <- c("character", "AsIs")
  }

  # city
  if (!is.null(city)) {
    cities <-  aq_cities(country = country)

    if (!(city %in% cities$cityURL)) {# nolint
      stop(call. = FALSE, paste0("This city/country combination is not available within the platform. See ?aq_cities."))# nolint
    }
    # make sure it won't be re-encoded
    Encoding(city) <- "UTF-8"
    class(city) <- c("character", "AsIs")

  }

  # country
  if (!is.null(country)) {

    if (!(country %in% aq_countries()$code)) {# nolint
      stop(call. = FALSE, "This country is not available within the platform. See ?aq_countries")
    }
  }

  # parameter
  if (!is.null(parameter)) {
    if (!(parameter %in% c("pm25", "pm10", "so2",
                           "no2", "o3", "co", "bc"))) {
      stop(call. = FALSE, "You asked for an invalid parameter: see list of valid parameters in the Arguments section of the function help")# nolint
    }

  locations <-  aq_locations(country = country,
                         city = city,
                         location = location)

    if (apply(locations[, parameter], 2, sum) == 0) {
      stop(call. = FALSE, "This parameter is not available for any location corresponding to your query. See ?aq_locations")# nolint
    }
  }

  # has_geo
  if (!is.null(has_geo)) {
    if (has_geo == TRUE) {
      has_geo <- "1"
    }
    if (has_geo == FALSE) {
      has_geo <- "false"
    }

  }

  # date_from
  if (!is.null(date_from)) {
    if (is.na(suppressWarnings(lubridate::ymd(date_from)))) {
      stop(call. = FALSE, "date_from and date_to have to be inputed as year-month-day.")
    }
  }
  # date_to
  if (!is.null(date_to)) {
    if (is.na(suppressWarnings(lubridate::ymd(date_to)))) {
      stop(call. = FALSE, "date_from and date_to have to be inputed as year-month-day.")
    }
  }

  # check dates
  if (!is.null(date_from) & !is.null(date_to)) {
    if (ymd(date_from) > ymd(date_to)) {
      stop(call. = FALSE, "The start date must be smaller than the end date.")
    }

  }

  # value_from
  if (!is.null(value_from)) {
    if (value_from < 0) {
      stop(call. = FALSE, "No negative value for value_from please!")
    }
  }

  # value_to
  if (!is.null(value_to)) {
    if (value_to < 0) {
      stop(call. = FALSE, "No negative value for value_to please!")
    }
  }

  # check values
  if (!is.null(value_from) & !is.null(value_to)) {
    if (value_to < value_from) {
      stop(call. = FALSE, "The max value must be bigger than the min value.")
    }

  }

  if(!is.null(latitude)|!is.null(longitude)){
    if(is.null(latitude)|is.null(longitude)){
      stop(call. = FALSE, "If you input a latitude or longitude, you have to input the other coordinate")
    }
    if (!dplyr::between(latitude, -90, 90)){
      stop(call. = FALSE, "Latitude should be between -90 and 90.")
    }
    if (!dplyr::between(longitude, -180, 180)){
      stop(call. = FALSE, "Longitude should be between -180 and 180.")
    }
    coordinates <- paste(latitude, longitude, sep = ",")
  }else{
    coordinates <- NULL
  }

  if(!is.null(radius)){
    if(is.null(latitude)){
      stop(call. = FALSE,
           "Radius has to be used with latitude and longitude.")
    }

  }

  argsList <- list(country = country,
                   city = replace_plus(city),
                   location = replace_plus(location),
                   parameter = parameter,
                   has_geo = has_geo,
                   date_from = date_from,
                   date_to = date_to,
                   value_from = value_from,
                   value_to = value_to,
                   limit = limit,
                   coordinates = coordinates,
                   radius = radius,
                   page = page)

  # argument only for measurements
  include_fields <- c(attribution,
                      averaging_period,
                      source_name)
  if(any(!is.null(include_fields))){

    fields <-  c("attribution",
                 "averagingPeriod",
                 "sourceName")
    fields <- fields[include_fields]
    fields <- toString(fields)
    fields <- gsub(", ", ",", fields)
    argsList[["include_fields"]] <- fields

  }



  return(argsList)
}

replace_plus <- function(x){
  if(is.null(x)){
    return(x)
  }else{
    gsub("\\+", " ", utils::URLdecode(x))
  }

}

############################################################
#                                                          #
#                       check status                       ####
#                                                          #
############################################################

status_url <- function() {
  "https://api.openaq.org/ping"
}

get_status <- function(){
  client <- crul::HttpClient$new(url = status_url())
  status <- client$retry("get")

  if (status$status_code >= 400) {
    return("red")
  } else {
    return(status$parse(encoding = "UTF-8"))
  }

  }

######################################################################################
# gets and parses
getResults <- function(urlAQ, argsList){
  if(!is.null(argsList$page)){
    getResults_bypage(urlAQ, argsList)
  }else{
    getResults_bymorepages(urlAQ, argsList)
  }

}
getResults_bypage <- function(urlAQ, argsList){
  client <- crul::HttpClient$new(url = urlAQ)
  argsList <- Filter(Negate(is.null), argsList)
  argsList <- argsList[argsList != ""]

  onwait <- function(resp, wait_time) {
    status <- get_status()

    if (!status %in% c("Healthy Connection", "green", "yellow", "unknown", "unavailable")) {
      stop("uh oh, the OpenAQ API seems to be having some issues, try again later")
    }
  }

  res <- client$retry(
    verb = "get",
    query = argsList,
    pause_base = 1,
    pause_cap = 60,
    pause_min = 1,
    times = 5,
    terminate_on = NULL,
    retry_only_on = NULL,
    onwait = onwait
  )

  if(argsList$limit == 0){
    contentPage <- suppressMessages(res$parse(encoding = "UTF-8"))
    # parse the data
    output <- jsonlite::fromJSON(contentPage)
    return(output$meta$found)
  }
  results <- treat_res(res)
  return(results)

  }

getResults_bymorepages <- function(urlAQ, argsList){
  argsList <- Filter(Negate(is.null), argsList)
  argsList <- argsList[argsList != ""]
  # find number of total pages
  argsList2 <- argsList
  argsList2$page <- 1
  argsList2$limit <- 1
  count <- attr(getResults_bypage(urlAQ, argsList2), "meta")$found
  if(is.na(argsList$limit)){
    limit <- 10000
  }else{
    limit <- argsList$limit
  }
  no_pages <- min(100, ceiling(count/limit))

  if(no_pages == 0){
    return(data.frame())
  }

  if(no_pages == 1){
    return(getResults_bypage(urlAQ, argsList))
  }
  queries <- lapply(1:no_pages,
                    add_page,
                    query = argsList)

  # 10 urls by request
  requests <- lapply(queries, create_request,
                     urlAQ = urlAQ)
  requests <- split(requests, ceiling(seq_along(requests)/10))

  res_list <- lapply(requests, get_res)
  bind_keeping_attr(res_list)

}

######################################################################################
# for getting URL encoded versions of city and locations
functionURL <- function(resTable, col1, newColName) {

  resTable[newColName] <- gsub(
    sapply(resTable[[col1]], utils::URLencode, reserved = TRUE),
    pattern = "\\%20", replacement = "+"
    )

  resTable
}

# encoding city name
addCityURL <- function(resTable){
  resTable <- functionURL(resTable,
                          col1 = "city",
                          newColName = "cityURL")

  return(resTable)
}

# encoding location name
addLocationURL <- function(resTable){
  resTable <- functionURL(resTable,
                          col1 = "location",
                          newColName = "locationURL")
  return(resTable)
}

######################################################################################
# transform a given column in POSIXct
functionTime <- function(resTable, newColName) {
  resTable[newColName] <- lubridate::ymd_hms(resTable[[newColName]])
  resTable
}

######################################################################################
# create the parameters column
functionParameters <- function(resTable) {

  resTable <- dplyr::mutate(resTable,
                            parameters = unlist(vapply(.data$parameters, toString, "")))
  resTable$pm25 <-  grepl("pm25", resTable$parameters)
  resTable$pm10 <-  grepl("pm10", resTable$parameters)
  resTable$no2 <-  grepl("no2", resTable$parameters)
  resTable$so2 <-  grepl("so2", resTable$parameters)
  resTable$o3 <-  grepl("o3", resTable$parameters)
  resTable$co <-  grepl("co", resTable$parameters)
  resTable$bc <-  grepl("bc", resTable$parameters)
  dplyr::select(resTable, - .data$parameters)

}


# dates abbreviation
func_date_headers <- function(date){
  date <- strsplit(date, ",")[[1]][2]
  date <- gsub("Jan", "01", date)
  date <- gsub("Feb", "02", date)
  date <- gsub("Mar", "03", date)
  date <- gsub("Apr", "04", date)
  date <- gsub("May", "05", date)
  date <- gsub("Jun", "06", date)
  date <- gsub("Jul", "07", date)
  date <- gsub("Aug", "08", date)
  date <- gsub("Sep", "09", date)
  date <- gsub("Oct", "10", date)
  date <- gsub("Nov", "11", date)
  date <- gsub("Dec", "12", date)
  lubridate::dmy_hms(date, tz = "GMT")
}


add_page <- function(page, query){
  query$page <- page
  return(query)
}

treat_res <- function(res){
  contentPage <- suppressMessages(res$parse(encoding = "UTF-8"))
  # parse the data
  output <- jsonlite::fromJSON(contentPage)

  coordinates <- output$results$coordinates
  date <- output$results$date
  averagingPeriod <- output$results$averagingPeriod

  if(!is.null(date)){
    date <- dplyr::rename(date, date.utc = .data$utc)
    if (!is.null(date[["local"]])) {
      date <- dplyr::rename(date, date.local = .data$local)
    }
  }
  if(!is.null(averagingPeriod)){

    averagingPeriod <- dplyr::rename(
      averagingPeriod,
      averagingPeriod.unit = .data$unit,
      averagingPeriod.value = .data$value
    )

  }
  results <- output$results

  if("averagingPeriod" %in% names(results)){
    results <- dplyr::select(results, - .data$averagingPeriod)
  }
  if("coordinates" %in% names(results)){
    results <- dplyr::select(results, - .data$coordinates)
  }
  if("date" %in% names(results)){
    results <- dplyr::select(results, - .data$date)
  }
  results <- dplyr::bind_cols(results, coordinates)
  results <- dplyr::bind_cols(results, date)
  results <- dplyr::bind_cols(results, averagingPeriod)

  results <- dplyr::as_tibble(results)


  # get the meta
  meta <- dplyr::as_tibble(
    as.data.frame(output$meta))
  #get the time stamps
  timestamp <- dplyr::as_tibble(data.frame(
    queriedAt = func_date_headers(res$response_headers$date)))

  attr(results, "meta") <- meta
  attr(results, "timestamp") <- timestamp
  attr(results, "url") <- res$url
  return(results)
}


create_request <- function(query, urlAQ){
  crul::HttpRequest$new(url = urlAQ)$get(query = query)
}

# get results for an
get_res <- function(async){
  res <- crul::AsyncVaried$new(.list = async)
  output <- res$request()
  try_number <- 1

  # rate limit
  if(any(res$status_code() == 429)){
    message("Too many requests, waiting 5 minutes.")
    Sys.sleep(60*5+5)
    output <- res$request()
  }
  while(any(res$status_code() >= 400) && try_number < 6) {
    status <- get_status()
  if(status %in% c("green", "yellow")){
      message(paste0("Server returned nothing, trying again, try number", try_number))
      Sys.sleep(2^try_number)
      output <- res$request()
      try_number <- try_number + 1
  }else{
    stop("uh oh, the OpenAQ API seems to be having some issues, try again later")
  }

  }

  res <- lapply(output, treat_res)

  bind_keeping_attr(res)

}

bind_keeping_attr <- function(df_list) {
  to_return <- dplyr::bind_rows(df_list)
  attr(to_return, "meta") <- attr(df_list[[1]], "meta")
  attr(to_return, "timestamp") <- attr(df_list[[1]], "timestamp")

  return(to_return)
}
