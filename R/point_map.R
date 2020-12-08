#' Map spatial point data
#'
#' Creates an interactive map of the datapoints provided. Latitude
#'  and longitude columns are found automatically.
#'
#' @param x dataframe with columns for latitude and longitude
#' @param ids numeric ids to filter dataset
#' @param coords character vector with column names for latitude and longitude.
#'  These will be guessed if not provided.
#'
#' @return a leaflet map
#' @export
#'
#' @examples
#' point_map(soil_data, 10500:10575)
#'
#' @importFrom dplyr filter rename all_of
point_map <- function(x, ids = NULL, coords = NULL) {

  checkmate::assert_data_frame(x)
  checkmate::assert_character(coords, len = 2)

  if (!is.null(ids)) {
    x <- x %>% dplyr::filter(id %in% ids)
  }

  if (!is.null(coords)) {
    lat_tag <- coords[1]
    lng_tag <- coords[2]

  } else {
    lat_vect <- c("latitude", "lat", "Y")
    lng_vect <- c("longitude", "lng", "long", "X")

    lat_tag <- lat_vect[which(lat_vect %in% names(x))[1]]
    lng_tag <- lng_vect[which(lng_vect %in% names(x))[1]]
  }

  x <- x %>% dplyr::rename(latitude = all_of(lat_tag),
                    longitude = all_of(lng_tag))

  leaflet::leaflet(data = x) %>%
    leaflet::addProviderTiles("Stamen.Watercolor",
                              options = leaflet::providerTileOptions(noWrap = TRUE)) %>%
    leaflet::addMarkers(lat = ~latitude,
                        lng = ~longitude,
                        popup = ~as.character(paste(sep = "", "<b>", "id: ",
                                                    id, "</b>", "<br/>", "Lat: ",
                                                    latitude, "<br/>", "Lon: ", longitude)),
                        clusterOptions = leaflet::markerClusterOptions()) %>%
    leaflet::addScaleBar(position = "topright")
}

id <- NULL
