#' Earthquake Information (2150 BC - Present)
#'
#' A dataset containing information about all recorded earthquakes,
#' starting in 2150 BC and ending on May 26, 2022.
#'
#' @format A data frame with 6312 rows and 39 columns
#' \describe{
#'   \item{Search Parameters}{An empty column to be removed in data cleaning}
#'   \item{Year}{The year in which the earthquake occurred}
#'   \item{Mo}{The month in which the earthquake occurred}
#'   \item{Dy}{The day on which the earthquake occurred}
#'   \item{Hr}{The time at which the earthquake occurred (hour)}
#'   \item{Mn}{The time at which the earthquake occurred (minute)}
#'   \item{Sec}{The time at which the earthquake occurred (second)}
#'   \item{Tsu}{}
#'   \item{Vol}{}
#'   \item{Location Name}{The name of the earthquake location}
#'   \item{Latitude}{Latitude of earthquake}
#'   \item{Longitude}{Longitude of earthquake}
#'   \item{Focal Depth (km)}{Focal depth of earthquake in km}
#'   \item{Mag}{Magnitude of the earthquake on Richter scale}
#'   \item{MMI Int}{Maximum MMI Intensity of earthquake}
#'   \item{Deaths}{Number of deaths due to earthquake}
#'   \item{Death Description}{The scale of deaths caused (0-4). 0=None, 1=Few(~1-50), 2=Some(~51-100),3=Many(~101-1000),4=Very Many(~1001+)}
#'   \item{Missing}{Number of missing persons due to earthquake}
#'   \item{Missing Description}{}
#'   \item{Injuries}{Number of injuries due to earthquake}
#'   \item{Injuries Description}{}
#'   \item{Damage ($Mil)}{Damage caused by earthquake in millions of dollars}
#'   \item{Damage Description}{The scale of damage caused (0-4). 0=None, 1=Limited(<1mil), 2=Moderate(~1-5mil), 3=Severe(~>5-24mil), 4=Extreme(~25mil+)}
#'   \item{Houses Destroyed}{Number of houses destroyed by earthquake}
#'   \item{Houses Destroyed Description}{}
#'   \item{Houses Damaged}{Number of houses damaged by earthquake}
#'   \item{Houses Damaged Description}{}
#'   \item{Total Deaths}{Total number of deaths due to earthquake and secondary effects}
#'   \item{Total Death Description}{The scale of deaths caused (0-4).0=None, 1=Few(~1-50), 2=Some(~51-100),3=Many(~101-1000),4=Very Many(~1001+)}
#'   \item{Total Missing}{Total number of people missing due to earthquake and secondary effects}
#'   \item{Total Missing Description}{}
#'   \item{Total Injuries}{Total number of injuries due to earthquake and secondary effects}
#'   \item{Total Injuries Description}{}
#'   \item{Total Damage ($Mil)}{Total damage in millions of dollars due to earthquake and secondary effects}
#'   \item{Total Damage Description}{The scale of damage caused (0-4).0=None, 1=Limited(<1mil), 2=Moderate(~1-5mil), 3=Severe(~>5-24mil),4=Extreme(~25mil+)}
#'   \item{Total Houses Destroyed}{Total houses destroyed by earthquake and secondary effects}
#'   \item{Total Houses Destroyed Description}{}
#'   \item{Total Houses Damaged}{Total houses damaged by earthquake and secondary effects}
#'   \item{Total Houses Damaged Description}{}
#'   }
#'
#' @source \url{www.ngdc.noaa.gov/hazel/view/hazards/earthquake/event-data}
"eq"
