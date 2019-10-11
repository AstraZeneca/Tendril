plotpercentile <- function(x){

  #prepare data
  dataset <- x$tendril$data
  perm.data <- x$perm.data
  percentile = x$tendril.pi

  #retrieve data, permutations and percentile and bind in one dataframe
  data <- dataset[dataset$Terms == unique(perm.data$Terms),]
  actualdata <- createDataset(data$x, data$y, "Actual", "Actual", data$StartDay, perm.data)
  actualdata$p.adj <- data$p.adj
  normdf <- createDataset(0, 0, "Actual", "Actual", 0, perm.data)
  normdf$p.adj <- 0
  actualdata<- rbind(normdf, actualdata)
  plotdata <- createDataset(perm.data$x, perm.data$y, perm.data$label, perm.data$type, perm.data$StartDay, perm.data)
  plotdata$p.adj <- perm.data$p.adj
  plotdata<- rbind(actualdata, plotdata)
  percdata <- createDataset(percentile$x, percentile$y, percentile$label, percentile$type, percentile$StartDay, perm.data)
  percdata$p.adj <- percentile$p.adj
  plotdata<- rbind(plotdata, percdata)

  plotdata$cc.10<-pmax(log10(plotdata$p.adj), -3)
  palette <- tendril_palette()
  
  #plot title
  Title <- paste(unique(perm.data$Terms), ", from day: ", unique(plotdata$perm.from.day), sep = "")

  #line colours
#  colours <- c("#F8766D", "grey50", "grey80")  # c("#F8766D", "darkgoldenrod1", "#00BFC4")

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

    p<- ggplot2::ggplot(data=plotdata[plotdata$type=="Permutation", ], aes(x=x, y=y, group=label), aspect="iso") +
       ggplot2::scale_colour_gradientn("log(p-val)",
                                       colours=palette$grpalette,
                                       values = palette$values,
                                       limits = palette$limits
       ) +
       ggplot2::geom_path(color="grey80") +
       ggplot2::geom_path(data=plotdata[plotdata$type=="Percentile", ], aes(x=x, y=y, group=label), color="grey40", size=1.25)+
       ggplot2::geom_path(data=plotdata[plotdata$type=="Actual", ], aes(x=x, y=y, group=label, color = cc.10))+
       ggplot2::geom_point(data=plotdata[plotdata$type=="Actual", ], aes(x=x, y=y, group=label, color = cc.10), size=2) +
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
