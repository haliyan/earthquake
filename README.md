earthquake package README
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# earthquake

<!-- badges: start -->
<!-- badges: end -->

The goal of earthquake is to clean, map, and visualize raw NOAA
earthquake data.

## Installation

You can install the development version of earthquake like so: Enter
`install_github("haliyan/earthquake")` into the console.

*Note: using the `install_github()` function requires that the
`devtools` package be installed and loaded. `devtools` can be installed
using `install_packages("devtools")` and loaded using
`library(devtools)`.*

Once the `earthquake` package is installed, load it using the code
`library(earthquake)`.

## Data

The NOAA data were sourced from [this web
page](www.ngdc.noaa.gov/hazel/view/hazards/earthquake/event-data).

Download the most recent version of the data by clicking the download
button in the top left hand corner of the page. Once the data has been
downloaded, read it into R using `readr::read_delim()` as follows:

``` r
data_name<-readr::read_delim("download_name.tsv",delim="\t")
```

Substitute “data\_name” with the desired name for the data.frame in R
(e.g. earthquake\_data) and “download\_name” with the name of the
downloaded file.

Note that a version of the NOAA earthquake dataset is provided in this
package for examples and testing purposes. **This is NOT the most
up-to-date version of the data set and should NOT be used for analysis.
It is recommended that the user download the most recent version of the
earthquake data and load it into R as shown above.** To access the
version included in the package for examples or practice, use the
following code:

``` r
data(eq_data)
```

The object `eq_data` will now be saved into the environment and can be
cleaned and visualized.

## Examples

Data can be cleaned using the `eq_raw_cleaner` function.

``` r
eq_clean<-eq_raw_cleaner(eq_data)
```

Once cleaned, the data can be plotted as part of a ggplot object using
the timeline geom.  
The following code also demonstrates the `bc_date()` and `year_pad()`
functions in action.

``` r
ggplot2::ggplot(data=eq_clean, 
                ggplot2::aes(x=Date,
                             xmin=bc_date(y="-033"),
                             xmax=year_pad(y="11"),
                             colour=Latitude,
                             fill=Latitude)) +
  geom_timeline(size=5) +
  ggplot2::ggtitle("Earthquakes from 33BC to 11AD")
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

Another example of the timeline geom shows how the y-aesthetic can be
used.

``` r
ggplot2::ggplot(data=eq_clean, 
                ggplot2::aes(x=Date, 
                             xmin=as.Date("2022-02-25"),
                             xmax=as.Date("2022-03-22"),
                             y=Mag,
                             col=`Total Injuries`,
                             fill=`Total Injuries`)) +
                  geom_timeline(size=5) +
  ggplot2::ggtitle("Selection of 2022 Earthquakes by Magnitude")
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

The earthquake data can also be plotted with the timeline label geom.
The following plot shows some earthquakes that occurred in early 2022
with the 2 largest (by magnitude) earthquakes’ countries labeled.

``` r
ggplot2::ggplot(data=eq_clean,
                ggplot2::aes(x=Date,
                             xmin=as.Date("2022-01-07"),
                             xmax=as.Date("2022-03-27"),
                             max_by=Mag,
                             txt=Country,
                             n_max=3,
                             col=Mag,
                             fill=Mag)) +
  geom_timeline_label() +
  ggplot2::ggtitle("Early 2022 Earthquakes")
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

Finally, the data can be mapped in Leaflet with the `eq_map` function.

The `eq_create_label()` function can be used to create popup text (which
contains information about earthquake location, magnitude, and number of
deaths caused) for each point displayed on the map. The image included
is a screenshot of the map generated from the code below:

``` r
eq_popup_data<-dplyr::mutate(tail(eq_clean,10), text=eq_create_label(tail(eq_clean,10)))
eq_map(eq_popup_data, 
       color= eq_popup_data$Mag, 
       annot_col=eq_popup_data$text)
```

![Map Screenshot](man/figures/screenshot.png)

When the code is run in R, a fully interactive Leaflet map (with popups)
will be created.

## License

GPL