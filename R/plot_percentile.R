plot_percentile <- function(x, coloring){

  #prepare data
  dataset <- x$tendril$data
  perm.data <- x$perm.data
  percentile = x$tendril.pi

  #retrieve data, permutations and percentile and bind in one dataframe
  data <- dataset[dataset$Terms == unique(perm.data$Terms),]
  actualdata <- createDataset(data$x, data$y, "Actual", "Actual", data$StartDay, perm.data)
  actualdata$color <- data[[coloring]]
  plotdata <- createDataset(perm.data$x, perm.data$y, perm.data$label, perm.data$type, perm.data$StartDay, perm.data)
  plotdata$color <- NA
  plotdata<- rbind(actualdata, plotdata)
  percdata <- createDataset(percentile$x, percentile$y, percentile$label, percentile$type, percentile$StartDay, percentile)
  percdata$color <- NA
  plotdata<- rbind(plotdata, percdata)

  if (coloring %in% LOG_COLORING_OPTIONS) {
    plotdata$color<-pmax(log10(plotdata$color), -3)
  }
  palette <- tendril_palette()

  #plot title
  Title <- paste(unique(perm.data$Terms), ", from day: ", unique(plotdata$perm.from.day), sep = "")

  #reference labeles
  data2.labels <- data.frame(
    xpos = c(Inf, -Inf),
    ypos = c(Inf, Inf),
    label = x$tendril$Treatments,
    hjust = c(1,0),
    vjust = c(1,1)
  )

    y<-NULL
    label<-NULL
    type<-NULL
    aes <- ggplot2::aes
    element_blank <- ggplot2::element_blank

    p<- ggplot2::ggplot(data=plotdata[plotdata$type=="Permutation", ], aes(x=x, y=y, group=label), aspect="iso")
    if (coloring %in% LOG_COLORING_OPTIONS) {
      p <- p +
        ggplot2::scale_colour_gradientn("log(p-val)",
                                        colours=palette$grpalette,
                                        values = palette$values,
                                        limits = palette$limits
       )
    } else if (coloring != "Terms") {
      p <- p +
        ggplot2::scale_colour_gradient(coloring)
    }
    p <- p +
      ggplot2::geom_path(color="grey80") +
      ggplot2::geom_path(data=plotdata[plotdata$type=="Percentile", ], aes(x=x, y=y, group=label), color="grey40", size=1)
    if (coloring != "Terms") {
      p <- p +
        ggplot2::geom_path(data=plotdata[plotdata$type=="Actual", ], aes(x=x, y=y, group=label, color = color))+
        ggplot2::geom_point(data=plotdata[plotdata$type=="Actual", ], aes(x=x, y=y, group=label, color = color), size=2)
    } else {
      p <- p +
        ggplot2::geom_path(data=plotdata[plotdata$type=="Actual", ], aes(x=x, y=y, group=label), color = "blue")+
        ggplot2::geom_point(data=plotdata[plotdata$type=="Actual", ], aes(x=x, y=y, group=label), color = "blue", size=2)
    }
    p <- p +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      ggplot2::theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      ggplot2::labs(color = "Type") +
      ggplot2::coord_fixed(ratio=1) +
      ggplot2::ggtitle(Title) +
      ggplot2::annotate(geom="text", x=data2.labels$xpos, y=data2.labels$ypos,
              label = data2.labels$label,
              hjust=data2.labels$hjust, vjust=data2.labels$vjust,
              colour = "grey40", size = 5, fontface = 2)

    return(p)
}
