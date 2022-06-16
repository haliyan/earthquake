#----------MAPPING------------
#' A function to create data labels
#'
#' This function takes a data.frame of data that is to be mapped with
#' \code{eq_map()}. It creates a column containing popup information
#' written in HTML format. This information includes the location name,
#' the earthquake magnitude, and total deaths resulting.
#' If any of this information is missing for a certain observation, it
#' will not be displayed in the popup.
#'
#' @param mapdata A data.frame of data to map
#'
#' @return A column of information written in HTML format which will
#'   appear as a popup on a map.
#'
#' @examples
#' \dontrun{data<-read.delim(system.file("extdata","noaa_earthquakes.tsv", package="earthquake")),header=TRUE
#'          data<-eq_raw_cleaner(data)
#'          data<-dplyr::mutate(data, popup_text=eq_create_label(data)}
#'
#' @importFrom stringr str_replace_all
#'
#' @export
eq_create_label<-function(mapdata){
  loc<-rep(NA, nrow(mapdata))
  mag<-rep(NA, nrow(mapdata))
  cas<-rep(NA, nrow(mapdata))
  for(i in 1:nrow(mapdata)){
    loc[i]<-paste("<b>Location Name:</b>",mapdata$`Location Name`[i],"<br>")
    mag[i]<-paste("<b>Magnitude:</b>",mapdata$Mag[i],"<br>")
    cas[i]<-paste("<b>Total Deaths:</b>",mapdata$`Total Deaths`[i],"<br>")
  }
  loc<-stringr::str_replace_all(string=loc,
                                pattern="<b>Location Name:</b> NA <br>",
                                replacement="")
  mag<-stringr::str_replace_all(string=mag,
                                pattern="<b>Magnitude:</b> NA <br>",
                                replacement="")
  cas<-stringr::str_replace_all(string=cas,
                                pattern="<b>Total Deaths:</b> NA <br>",
                                replacement="")
  popcol<-paste(loc,mag,cas)
  return(popcol)
}


#' A function to map earthquake data
#'
#' This function creates an interactive Leaflet map of earthquake data.
#' The locations of points on the map show the earthquake epicenters.
#' The sizes of points on the map show the earthquake magnitudes.
#' The points can be colored by a user-specified variable, and a legend is
#' included to explain the colors.
#' The user can also specify a column of the earthquake data to be shown
#' as a popup when each point is clicked.
#'
#' @param mapdata Data to be mapped
#' @param color The column to be colored by, indicated using the $
#'   operator. Defaults to no column, and every point is colored the
#'   same color in the magma color palette
#' @param annot_col The column which contains text for popups, indicated
#'   using the $ operator. Defaults to the Location Name column.
#'
#' @return NULL. Displays a map in the Viewer pane.
#'
#' @examples
#' \dontrun{data<-read.delim(system.file("extdata","noaa_earthquakes.tsv", package="earthquake")),header=TRUE
#'          data<-eq_raw_cleaner(data)
#'          eq_map(data)}
#'
#' @importFrom dplyr filter
#' @importFrom viridis magma
#' @importFrom leaflet colorFactor
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addCircleMarkers
#' @importFrom magrittr %>%
#'
#' @export
eq_map<-function(mapdata,
                 color=viridis::magma(1),
                 annot_col=mapdata$`Location Name`){
  mapdata<-dplyr::filter(mapdata, !is.na(Latitude), !is.na(Longitude))
  pal<-leaflet::colorFactor(viridis::magma(5), color)
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data=mapdata, radius = 0.01,
                              lng=~Longitude,
                              lat=~Latitude,
                              col="black")%>%
    leaflet::addCircleMarkers(data=mapdata, radius = ~Mag,
                              lng=~Longitude,
                              lat=~Latitude,
                              col=~pal(color),
                              popup=annot_col) %>%
    leaflet::addLegend(pal=pal, values=color)
}


