#----------TIMELINE GEOM---------
#' Create timeline stat function
#'
#' This creates the \code{stat_timeline()} function.
#' It uses the ggproto object StatTimeline.
#' The \code{stat_timeline} function takes data of a certain format,
#' processes it, and returns it in the correct format for the timeline
#' geom to plot.
#'
#' @param mapping Defaults to NULL
#' @param data Defaults to NULL, so the data is inherited from the plot
#'   data as specified in the \code{ggplot2::ggplot()} call
#' @param geom Defaults to "timeline"
#' @param position Defaults to "identity"
#' @param show.legend Defaults to TRUE
#' @param inherit.aes Defaults to TRUE
#' @param na.rm Defaults to FALSE
#' @param ... Arguments to be passed to methods
#'
#' @return a layer which renders the data to make it suitable for the
#'   timeline geom to plot
#'
#' @section Aesthetics:
#' \code{stat_timeline()} understands the following aesthetics:
#' \itemize{
#'   \item{x}{REQUIRED. A column of dates. Column type must be Date}
#'   \item{xmin}{REQUIRED. Minimum/starting date. Must be a Date object}
#'   \item{xmax}{REQUIRED. Maxmimum/ending date. Must be a Date object}
#'   }
#'
#' @examples
#'   \dontrun{ggplot2::ggplot(data=
#'                                 eq_raw_cleaner(tail(eq,100)),
#'                            ggplot2::aes(x=Date,
#'                                         xmin=as.Date("2022-01-14"),
#'                                         xmax=as.Date("2022-05-26"))) +
#'                                         stat_timeline(size=5)}
#'
#' @importFrom ggplot2 layer
#'
#' @export
stat_timeline<-function(mapping=NULL,data=NULL,geom="timeline",
                        position="identity",show.legend=TRUE,
                        inherit.aes=TRUE, na.rm=FALSE,...){
  ggplot2::layer(
    stat=StatTimeline,
    geom=geom,
    data=data,
    mapping=mapping,
    position=position,
    show.legend=show.legend,
    inherit.aes=inherit.aes,
    params=list(na.rm=na.rm,...))
}

#' Create timeline geom function
#'
#' This creates the \code{geom_timeline()} function.
#' It uses the ggproto object GeomTimeline.
#' This function allows the timeline and points (as specified in the
#' creation of the object GeomTimeline) to be plotted as a ggplot
#' layer.
#'
#' @param mapping Defaults to NULL
#' @param data Defaults to NULL, so the data is inherited from the plot
#'   data as specified in the \code{ggplot2::ggplot()} call
#' @param stat Defaults to "timeline", specifying that the statistical
#'   transformation to be used on the data for this layer is the
#'   timeline stat
#' @param position Defaults to "identity"
#' @param na.rm Defaults to FALSE
#' @param show.legend Defaults to TRUE
#' @param inherit.aes Defaults to TRUE
#' @param ... Arguments to be passed to methods
#'
#' @return a layer which plots the data as per the GeomTimeline
#'   object definition that can be added to a ggplot object
#'
#' @section Aesthetics:
#' \code{geom_timeline()} understands the following aesthetics:
#' \itemize{
#'   \item{x}{REQUIRED. A column of dates. Column type must be Date}
#'   \item{xmin}{REQUIRED. Minimum/starting date. Must be a Date object}
#'   \item{xmax}{REQUIRED. Maxmimum/ending date. Must be a Date object}
#'   \item{y}{optional, defaults to 0.3}
#'   \item{colour}{optional, defaults to black. Column to color points
#'     by}
#'   \item{fill}{optional, defaults to black. Column to fill points by}
#'   \item{size}{optional, defaults to 0pt. Column to size points by}
#'   \item{alpha}{optional, defaults to 1 (no transparency)}
#'   }
#' @importFrom ggplot2 layer
#'
#' @examples
#'   \dontrun{ggplot2::ggplot(data=
#'                                 eq_raw_cleaner(tail(eq,100)),
#'                            ggplot2::aes(x=Date,
#'                                         xmin=as.Date("2022-01-14"),
#'                                         xmax=as.Date("2022-05-26"))) +
#'                                         geom_timeline(size=5)}
#' @export
geom_timeline<-function(mapping=NULL,data=NULL, stat="timeline",
                        position="identity", na.rm=FALSE,
                        show.legend=TRUE,inherit.aes=TRUE,...){
  ggplot2::layer(
    data=data,
    mapping=mapping,
    stat=stat,
    geom=GeomTimeline,
    position=position,
    show.legend=show.legend,
    inherit.aes=inherit.aes,
    params=list(na.rm=na.rm,...))
}

#' Timeline Stat Object
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatTimeline<-ggplot2::ggproto("StatTimeline", ggplot2::Stat,
                               required_aes=c("x","xmin","xmax"),
                               compute_group=function(data,scales){
                                 out<-clipper(data)
                                 out
                               })

#' Timeline Geom Object
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomTimeline<-ggplot2::ggproto("GeomTimeline", ggplot2::GeomPoint,
                               required_aes=c("x","xmin","xmax"),
                               draw_panel=function(data,panel_params,coord){
                                 coords<-coord$transform(data,panel_params)
                                 timeline_draw(coords)
                               },
                               optional_aes=c("colour","fill", "alpha",
                                              "size","y"),
                               default_aes=ggplot2::aes(colour=1,fill=1,
                                               size=0,alpha=1,
                                               y=0.3),
                               draw_key=ggplot2::draw_key_point)

# example
# tail(clean,10) %>%
#   ggplot2::ggplot(ggplot2::aes(x=Date,xmin=as.Date("2022-02-25"),
#              xmax=as.Date("2022-05-26"),
#              col=`Total Deaths`,
#              fill=`Total Deaths`,
#              y=`Location Name`))+
#   geom_timeline(size=5, alpha=0.8)


#----------TIMELINE LABEL GEOM----------
#' Create timeline label stat function
#'
#' This creates the \code{stat_timeline_label()} function.
#' It uses the ggproto object StatTimelineLabel.
#' The \code{stat_timeline_label} function takes data of a certain
#' format, processes it, and returns it in the correct format for the
#' timeline label geom to plot.
#'
#' @param mapping Defaults to NULL
#' @param data Defaults to NULL, so the data is inherited from the plot
#'   data as specified in the \code{ggplot2::ggplot()} call
#' @param geom Defaults to "timelineLabel"
#' @param position Defaults to "identity"
#' @param show.legend Defaults to TRUE
#' @param inherit.aes Defaults to TRUE
#' @param na.rm Defaults to TRUE
#' @param ... Arguments to be passed to methods
#'
#' @return a layer which renders the data to make it suitable for the
#'   timeline label geom to plot
#'
#' @section Aesthetics:
#' \code{stat_timeline_label()} understands the following aesthetics:
#' \itemize{
#'   \item{x}{REQUIRED. A column of dates. Column type must be Date}
#'   \item{xmin}{REQUIRED. Minimum/starting date. Must be a Date object}
#'   \item{xmax}{REQUIRED. Maxmimum/ending date. Must be a Date object}
#'   \item{txt}{REQUIRED. Any type of column to be displayed as text}
#'   \item{max_by}{REQUIRED. A numeric/Date column to select maximum
#'     observations by}
#'   \item{n_max}{optional, if not supplied will be auto calculated.
#'     A number specifying how many values should be selected from the
#'     max_by column (arranged in descending order) }
#'   }
#'
#' @examples
#'   \dontrun{ggplot2::ggplot(data=
#'                                 eq_raw_cleaner(tail(eq,100)),
#'                            ggplot2::aes(x=Date,
#'                                         xmin=as.Date("2022-01-14"),
#'                                         xmax=as.Date("2022-05-26"),
#'                                         max_by=Mag,
#'                                         txt=Country,
#'                                         n_max=3)) +
#'                            stat_timeline_label()}
#'
#' @importFrom ggplot2 layer
#'
#' @export
stat_timeline_label<-function(mapping=NULL,data=NULL,geom="timelineLabel",
                              position="identity",show.legend=TRUE,
                              inherit.aes=TRUE, na.rm=TRUE,...){
  ggplot2::layer(
    stat=StatTimelineLabel,
    geom=geom,
    data=data,
    mapping=mapping,
    position=position,
    show.legend=show.legend,
    inherit.aes=inherit.aes,
    params=list(na.rm=na.rm,...))
}


#' Create timeline label geom function
#'
#' This creates the \code{geom_timeline_label()} function.
#' It uses the ggproto object GeomTimelineLabel
#' This function allows the timeline and points (as specified in the
#' creation of the object GeomTimelineLabel) to be plotted as a ggplot
#' layer.
#'
#' @param mapping Defaults to NULL
#' @param data Defaults to NULL, so the data is inherited from the plot
#'   data as specified in the \code{ggplot2::ggplot()} call
#' @param stat Defaults to "timelineLabel", specifying that the
#'   statistical transformation to be used on the data for this layer is
#'   the timelineLabel stat
#' @param position Defaults to "identity"
#' @param na.rm Defaults to TRUE
#' @param show.legend Defaults to TRUE
#' @param inherit.aes Defaults to TRUE
#' @param ... Arguments to be passed to methods
#'
#' @return a layer which plots the data as per the GeomTimelineLabel
#'   object definition that can be added to a ggplot object
#'
#' @section Aesthetics:
#' \code{geom_timeline_label()} understands the following aesthetics:
#' \itemize{
#'   \item{x}{REQUIRED. A column of dates. Column type must be Date}
#'   \item{xmin}{REQUIRED. Minimum/starting date. Must be a Date object}
#'   \item{xmax}{REQUIRED. Maxmimum/ending date. Must be a Date object}
#'   \item{txt}{REQUIRED. Any type of column to be displayed as text}
#'   \item{max_by}{REQUIRED. A numeric/Date column to select maximum
#'     observations by}
#'   \item{n_max}{optional, if not supplied will be auto calculated.
#'     A number specifying how many values should be selected from the
#'     max_by column (arranged in descending order) }
#'   \item{colour}{optional, defaults to black. Column to color points
#'     by}
#'   \item{fill}{optional, defaults to black. Column to fill points by}
#'   \item{size}{optional, defaults to 5pt. Column to size points by}
#'   \item{alpha}{optional, defaults to 1 (no transparency)}
#'   }
#'
#' @examples
#'   \dontrun{ggplot2::ggplot(data=
#'                                 eq_raw_cleaner(tail(eq,100)),
#'                            ggplot2::aes(x=Date,
#'                                         xmin=as.Date("2022-01-14"),
#'                                         xmax=as.Date("2022-05-26"),
#'                                         max_by=Mag,
#'                                         txt=Country,
#'                                         n_max=3)) +
#'                            geom_timeline_label()}
#'
#' @importFrom ggplot2 layer
#'
#' @export
geom_timeline_label<-function(mapping=NULL,data=NULL, stat="timelineLabel",
                              position="identity", na.rm=TRUE,
                              show.legend=TRUE,inherit.aes=TRUE,...){
  ggplot2::layer(
    data=data,
    mapping=mapping,
    stat=stat,
    geom=GeomTimelineLabel,
    position=position,
    show.legend=show.legend,
    inherit.aes=inherit.aes,
    params=list(na.rm=na.rm,...))
}

#' Labeled Timeline Stat Object
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatTimelineLabel=ggplot2::ggproto("StatTimelineLabel", ggplot2::Stat,
                                   required_aes=c("x","xmin","xmax","txt","max_by"),
                                   compute_panel=function(data, scales){
                                     out<-clipper_plus(data)
                                     return(out)
                                   }, optional_aes=c("n_max"))

#' Labeled Timeline Geom Object
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomTimelineLabel=ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                                   required_aes=c("x","xmin","xmax","txt","max_by"),
                                   draw_panel=function(data, panel_params, coord){
                                     coords<-coord$transform(data, panel_params)
                                     timeline_label_draw(coords)
                                   }, optional_aes=c("n_max","color",
                                                     "fill","alpha","size"),
                                   default_aes=ggplot2::aes(size=5, alpha=1,
                                                   colour=1, fill=1))

#----------HELPER FUNCTIONS--------
#' A data clipper function for \code{stat_timeline}
#'
#' This function "clips" the data that has been passed to the timeline
#' stat. All observations that occur before the specified xmin or
#' minimum date are filtered out, as are all observations that occur
#' after the specified xmax or maximum date. The "clipped" data frame is
#' then returned and used by the timeline geom for plotting.
#'
#' @param data A data.frame to be plotted in ggplot2 which is passed to
#'   \code{geom_timeline()} and subsequently \code{stat_timeline()}.
#'
#' @return A data.frame filtered to contain only the observations lying
#'   within the \code{xmin} and \code{xmax} bounds specified.
#'
#' @examples
#'   \dontrun{clipper(data.frame(x=c(4,7,12),
#'                               xmin=c(6,6,6),
#'                               xmax=c(15,15,15)))}
#'
clipper<-function(data){
  idx<-which(data$x>=data$xmin & data$x<=data$xmax)
  data<-data[idx,]
  return(data)
}

#' Draw function for \code{geom_timeline}
#'
#' This function takes the data.frame returned by \code{stat_timeline}
#' which has already been transformed for plotting.
#' It then uses the columns of the data.frame to create three types of
#' grob: a line that lies across the x-axis (the timeline), points that
#' represent individual earthquake observations, and segments that go
#' through each point to clarify its position on the y-axis (this is
#' helpful when a continuous y-aesthetic has been specified).
#' The points are colored, filled, and sized according to the colour,
#' fill, alpha, and size aesthetic specifications. If any/all of these
#' aesthetics are not specified by the user, default aesthetics are
#' provided in the \code{geom_timeline} code.
#' The function then creates a gTree out of these grobs which it returns
#' to be plotted.
#'
#' @param coords A data.frame passed from \code{stat_timeline} to
#'   \code{geom_timeline} and transformed for plotting.
#'
#' @return a gTree object containing the grobs to be drawn
#'
#' @examples
#'   \dontrun{timeline_draw(data.frame(x=c(7,12),
#'                                     xmin=c(6,6),
#'                                     xmax=c(15,15),
#'                                     y=c(3,6),
#'                                     size=c(5,5),
#'                                     colour=c(1,1),
#'                                     fill=c(1,1),
#'                                     alpha=c(1,1))}
#'
#' @importFrom grid linesGrob
#' @importFrom grid pointsGrob
#' @importFrom grid gpar
#' @importFrom grid segmentsGrob
#' @importFrom grid gTree
#' @importFrom grid gList
#' @importFrom scales alpha
#' @importFrom ggplot2 .pt
#' @importFrom grid unit
timeline_draw<-function(coords){
  timeline<-grid::linesGrob(
    default.units="npc",
    x=coords$x,
    y=0,
    gp=grid::gpar(lty=1,lwd=1,
                  col="black")
  )
  points<-grid::pointsGrob(
    x=grid::unit(coords$x, units="npc"),

    y=grid::unit(coords$y,
                 units="npc"),

    pch=19,
    gp=grid::gpar(col=
                    scales::alpha(coords$colour,
                                  coords$alpha),
                  fill=
                    scales::alpha(coords$fill,
                                  coords$alpha),
                  fontsize =
                    coords$size*ggplot2::.pt)
  )
  pointline<-grid::segmentsGrob(
    default.units="npc",
    x0=coords$xmin,
    y0=coords$y,
    x1=coords$xmax,
    y1=coords$y,
    gp=grid::gpar(lty=1,lwd=3, col="gray",
                  alpha=0.8)
  )
  earthquake<-grid::gTree(
    children=grid::gList(timeline,
                         pointline,
                         points))
  return(earthquake)
}

#' Function to select n_max earthquakes automatically
#'
#' If a value for n_max earthquakes is not supplied by the user, this
#' function calculates a reasonable n_max value and creates a new column
#' containing this new n_max value.
#' This n_max value is equal to half the number of rows in the data
#' inputted. If this is not an integer, it is rounded down.
#' A message will be displayed indicating what value n_max was set to.
#' An error is thrown if n_max is calculated to be less than 1 as this
#' will be impossible to plot.
#'
#' @param data A data.frame of data which has been passed from
#'   \code{geom_timeline_label} to \code{stat_timeline_label}.
#'
#' @return A data.frame of data with a new n_max column
#'
#' @examples
#'   \dontrun{auto_maxer(tail(eq,20))}
#'   \dontrun{auto_maxer(tail(eq,19))}
#'
#' @importFrom dplyr mutate
auto_maxer<-function(data){
  data<-dplyr::mutate(data, n_max=rep(floor(0.5*nrow(data)),
                                      nrow(data)))
  message("n_max has been automatically set to ", unique(data$n_max))
  if(unique(data$n_max)<1){
    stop("Please enter larger x range or specify n_max")
  }
  return(data)
}


#' A data clipper function adapted for \code{stat_timeline_label}
#'
#' This function "clips" the data that has been passed to the timeline
#' label stat. All observations that occur before the specified xmin or
#' minimum date are filtered out, as are all observations that occur
#' after the specified xmax or maximum date.
#' The data is then filtered so that the n_max largest observations will
#' be flagged with the logical value TRUE. This indicates to the geom
#' that these observations should be plotted with a label.
#'
#' @param data A data.frame of data which has been passed from
#'   \code{geom_timeline_label} to \code{stat_timeline_label}.
#'
#' @return A data.frame filtered to contain only the observations lying
#'   within the \code{xmin} and \code{xmax} bounds specified AND
#'   filtered to contain only n_max earthquakes to plot.
#'
#' @examples
#'   \dontrun{clipper_plus(data.frame(x=c(4,7,12),
#'                                    xmin=c(6,6,6),
#'                                    xmax=c(15,15,15),
#'                                    txt=c("red","yellow","blue"),
#'                                    max_by=c(1,2,3)))}
#'
clipper_plus<-function(data){
  out<-clipper(data)
  if(length(out$n_max)==0){
    out<-auto_maxer(out)
  }
  max_vals<-sort(out$max_by, decreasing=TRUE,na.last=TRUE)[1:unique(out$n_max)]
  max_yn=rep(FALSE, nrow(out))
  for(i in 1:nrow(out)){
    if(out$max_by[i] %in% max_vals == TRUE){
      max_yn[i]<-TRUE
    } else {
      max_yn[i]<-FALSE
    }
  }
  out$max_yn=max_yn
  return(out)
}

#' Draw function for \code{geom_timeline_label}
#'
#' This function takes the data.frame returned by
#' \code{stat_timeline_label} which has already been transformed for
#' plotting.
#' It then uses the columns of the data.frame to create up to five grob
#' types: a segment that lies across the x-axis (timeline), points that
#' represent individual earthquake observations, a segment that goes
#' through the points, text displaying the information in whichever
#' column the user has supplied to the txt argument, and a segment
#' that joins the text to the point to which it corresponds.
#' Text grobs and corresponding segments are only produced for a few of
#' the largest values in the column supplied to the max_by aesthetic.
#' The exact number of text grobs to be produced is determined by the
#' optional aesthetic n_max.
#' The points are colored, filled, and sized according to the colour,
#' fill, alpha, and size aesthetic specifications. If any/all of these
#' aesthetics are not specified by the user, default aesthetics are
#' provided in the \code{geom_timeline} code.
#' The function then creates a gTree out of these grobs which it returns
#' to be plotted.
#'
#' @param coords A data.frame passed from \code{stat_timeline} to
#'   \code{geom_timeline} and transformed for plotting.
#'
#' @return a gTree object containing the grobs to be drawn
#'
#' @examples
#'   \dontrun{timeline_label_draw(data.frame(x=c(7,12),
#'                                 xmin=c(6,6),
#'                                 xmax=c(15,15),
#'                                 max_by=c(1,2),
#'                                 n_max=c(1,1),
#'                                 max_yn=c(FALSE,TRUE),
#'                                 txt=c("a","b"),
#'                                 size=c(5,5),
#'                                 colour=c(1,1),
#'                                 fill=c(1,1),
#'                                 alpha=c(1,1)))}
#'
#' @importFrom grid unit
#' @importFrom grid textGrob
#' @importFrom grid segmentsGrob
#' @importFrom grid pointsGrob
#' @importFrom grid gTree
#' @importFrom grid gList
#' @importFrom grid gpar
#' @importFrom scales alpha
#' @importFrom ggplot2 .pt
#'
timeline_label_draw<-function(coords){
  earthquake_max<-grid::gList()
  for(i in 1:nrow(coords)){
    points<-grid::pointsGrob(
      x=grid::unit(coords$x, units="npc"),

      y=grid::unit(rep(0.3,length(coords$x)),
                   units="npc"),

      pch=19,
      gp=grid::gpar(col=
                      scales::alpha(coords$colour,
                                    coords$alpha),
                    fill=
                      scales::alpha(coords$fill,
                                    coords$alpha),
                    fontsize =
                      coords$size*ggplot2::.pt)
    )
    pointline<-grid::segmentsGrob(
      default.units="npc",
      x0=coords$xmin,
      y0=0.3,
      x1=coords$xmax,
      y1=0.3,
      gp=grid::gpar(lty=1,lwd=2, col="gray",
                    alpha=0.8)
    )
    timeline<-grid::segmentsGrob(
      default.units="npc",
      x0=coords$xmin,
      y0=0,
      x1=coords$xmax,
      y1=0,
      gp=grid::gpar(lty=1,lwd=3, col="black")
    )
    if(coords$max_yn[i]==TRUE){
      maxs<-grid::gList()
      maxline<-grid::gList()
      maxs[[i]]<-grid::textGrob(
        label=coords$txt[i],
        x=grid::unit(coords$x[i],
                     "npc"),
        y=grid::unit(rep(0.6,unique(coords$n_max)),
                     "npc"),
        rot=20
      )
      maxline[[i]]<-grid::segmentsGrob(
        default.units="npc",
        x0=coords$x[i],
        y0=0.55,
        x1=coords$x[i],
        y1=0.3
      )
      earthquake_max=grid::gList(grid::gList(points,
                                             pointline,
                                             timeline,
                                             maxs[[i]],
                                             maxline[[i]]),
                                 earthquake_max)
    } else {
      earthquake_non_max=grid::gList(points,
                                     pointline,
                                     timeline)
    }
  }
  earthquake<-grid::gTree(
    children=grid::gList(earthquake_max,earthquake_non_max))
  return(earthquake)
}
