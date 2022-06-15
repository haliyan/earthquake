#----------- DATA CLEANING ---------

#' A function to clean raw NOAA data
#'
#' This function takes raw NOAA data, downloaded in tab-delimited format
#' and read into R using \code{readr::read_delim}.
#' It removes the unnecessary Search Parameters column, and  filters out
#' rows where Latitude, Longitude, or Year are missing (NA).
#' It creates a new Date column and a new Country column which are useful
#' for visualization purposes. It also cleans up the existing Location
#' Name column and ensures that the Latitude and Longitude columns are
#' numeric.
#'
#' @param data A tibble of raw NOAA data
#'
#' @return A data.frame of clean NOAA data
#'
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#'
#' @export
eq_raw_cleaner <- function (data) {
  data %>%
    dplyr::select(-`Search Parameters`) %>%
    dplyr::filter(!is.na(Year)) %>%
    dplyr::filter(!is.na(Latitude),!is.na(Longitude)) %>%
    dplyr::mutate(Date=date_sorter(Year, Mo, Dy)) %>%
    dplyr::select(-c(Year, Mo, Dy)) %>%
    dplyr::mutate(Latitude=as.numeric(Latitude)) %>%
    dplyr::mutate(Longitude=as.numeric(Longitude)) %>%
    country_finder(.) %>%
    eq_location_clean(.) %>%
    as.data.frame(.)
}

#' Function to clean Location Name column
#'
#' This function cleans the Location Name column of the NOAA data by
#' stripping away the country name which is placed before the colon :.
#' It keeps the location name that follows the colon :.
#' It also converts the column type from character to factor to
#' facilitate data visualization later on.
#'
#' @param data A tibble passed to the function through the pipe chain
#'   within the \code{eq_raw_cleaner} function.
#'
#' @return A tibble with the original Location Name column replaced by
#'   a cleaned Location Name column
#'
#' @importFrom stringr str_to_title
#'
#' @export
eq_location_clean<-function(data){
  names<-data$`Location Name`
  names<-stringr::str_to_title(names)
  for (i in 1:length(names)){
    if(grepl(":",names[i])){
      names[i]<-gsub("^.*?:","",names[i])
      names[i]<-gsub("^\\s+","",names[i])
    } else {
      names[i]<-names[i]
    }
  }
  data$`Location Name`<-as.factor(names)
  return(data)
}

#example
# clean<-eq_raw_cleaner(eq_data)

#----------HELPER FUNCTIONS--------
#' A function to paste together dates
#'
#' @param y A year number as a character string
#' @param m A month number (1-12) as a character string. Defaults to 1
#'   (January)
#' @param d A day number as a character string. Defaults to 1 (the 1st).
#'
#' @return A date of class Date in y-m-d format
#'
#' @importFrom tidyr replace_na
#' @importFrom lubridate ymd
#'
#' @export
date_paster<-function(y,m=1,d=1){
  m<-tidyr::replace_na(m,1)
  d<-tidyr::replace_na(d,1)
  date<-paste(y,m,d,sep="-")
  date<-lubridate::ymd(date)
  return(date)
}

#' A function to deal with BC dates
#'
#' This function handles BC/BCE or "negative" dates.
#' It takes as input a BC year, month, and day. If no month and day are
#' inputted, the month defaults to the first month (January) and the day
#' defaults to the first (1st).
#' The function calculates the BC date by subtracting the positive
#' version of the inputted negative year from January 1st, 0000.
#'
#' @inheritParams date_paster
#'
#' @return A BC/BCE date of class Date in y-m-d format
#'
#' @importFrom lubridate ymd
#' @importfrom lubridate years
bc_date<-function(y,m=1,d=1){
  bc_year= lubridate::ymd("0000-01-01") - lubridate::years(-1*as.numeric(y))
  return(bc_year)
}

#' A function to deal with 2- or 3- digit year dates
#'
#' This function handles 2- or 3- digit year dates.
#' It takes as input a year, month, and day. If no month and day are
#' inputted, the month defaults to the first month (January) and the day
#' defaults to the first (1st).
#' The function turns a 2- or 3- digit year into a 4-digit year by
#' padding the left side with zeros. It does not change years which are
#' already 4 digits long.
#'
#' @inheritParams date_paster
#'
#' @importFrom stringr str_pad
#'
#' @return An AD/CE date of class Date in y-m-d format
year_pad<-function(y,m=1,d=1){
  y<- stringr::str_pad(string=as.character(y), width = 4, side = "left",
                       pad = "0")
  date<-date_paster(y,m,d)
  return(date)
}

#' A function to sort dates
#'
#' This function uses the helper functions \code{bc_date()},
#' \code{year_pad()} and \code{date_paster()} to sort vectors of years,
#' months, and days (the columns Year, Mo, and Dy in the NOAA data).
#' It first checks for BC dates and handles those with the function
#' \code{bc_date()}. It then handles AD dates. First, it uses the
#' \code{year_pad()} function to handle 2- and 3- digit years, then uses
#' the \code{date_paster()} function on 4-digit years.
#'
#' @param yvec A numeric vector of years
#' @param mvec A numeric vector of months
#' @param dvec A numeric vector of days
#'
#' @return A column of type Date containing dates in the format y-m-d.
#'   All dates in this column are correctly formatted.
#'
date_sorter<-function(yvec,mvec,dvec){
  datecol<-rep("0000-01-01", length(yvec))
  datecol<-as.Date(datecol)
  for(i in 1:length(yvec)){
    if(yvec[i]<0){
      datecol[i]<-bc_date(yvec[i])
    } else if (yvec[i]>=0) {
      if(nchar(as.character(yvec[i]))<4){
        datecol[i]<-year_pad(y=yvec[i],
                             m=mvec[i],
                             d=dvec[i])
      } else {
        datecol[i]<- date_paster(yvec[i],
                                 mvec[i],
                                 dvec[i])
      }
    }
  }
  return(datecol)
}

#' Function to classify lon/lat points as countries
#'
#' This function was necessary as the author of this code did not have
#' access to the exact data set specified in the instructions.
#' See README for details.
#' The data set used did not have a Country column. This function works
#' around that problem by creating a Country column by classifying each
#' entry (by latitude and longitude) as a country as defined in the
#' package \code{rnaturalearth}, wherever possible.
#'
#' @param data A tibble passed to the function through the pipe chain
#'   within the \code{eq_raw_cleaner} function.
#' @return A tibble with a Country column
#'
#' @importFrom rnaturalearth ne_countries
#' @importFrom dplyr select
#' @importFrom sp coordinates
#' @importFrom sp proj4string
#' @importFrom sp CRS
#' @importFrom sp over
#' @importFrom dplyr mutate
country_finder<-function(data){
  world<-rnaturalearth::ne_countries()
  latlon<-dplyr::select(data, Latitude, Longitude)
  sp::coordinates(latlon)<-c("Longitude","Latitude")
  sp::proj4string(latlon)<-sp::CRS(proj4string(world))
  idx<-sp::over(latlon, world)
  data<-dplyr::mutate(data, Country=as.factor(idx$admin))
  return(data)
}
