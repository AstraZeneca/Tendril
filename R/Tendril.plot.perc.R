plotpercentile <- function(x){

  #prepare data
  dataset <- x$data
  perm.data <- x$perm.data
  percentile = x$tendril.pi

  #retrieve data, permutations and percentile and bind in one dataframe
  data <- dataset[dataset$Terms == unique(perm.data$Terms),]
  actualdata <- createDataset(data$x, data$y, "Actual", "Actual", data$StartDay, perm.data)
  normdf <- createDataset(0, 0, "Actual", "Actual", 0, perm.data)
  actualdata<- rbind(normdf, actualdata)
  plotdata <- createDataset(perm.data$x, perm.data$y, perm.data$label, perm.data$type, perm.data$StartDay, perm.data)
  plotdata<- rbind(actualdata, plotdata)
  percdata <- createDataset(percentile$x, percentile$y, percentile$label, percentile$type, percentile$StartDay, perm.data)
  plotdata<- rbind(plotdata, percdata)

  #plot title
  Title <- paste(unique(perm.data$Terms), ", from day: ", unique(plotdata$perm.from.day), sep = "")

  #line colours
  colours <- c("#F8766D", "darkgoldenrod1", "#00BFC4")

  #reference labeles
  data2.labels <- data.frame(
    xpos = c(Inf, -Inf),
    ypos = c(Inf, Inf),
    label = x$Treatments,
    hjust = c(1,0),
    vjust = c(1,1)
  )

    y<-NULL
    label<-NULL
    type<-NULL
    p<-ggplot(data=plotdata[plotdata$type=="Permutation", ], aes(x=x, y=y, group=label, color = type), aspect="iso") +
      geom_path() +
      geom_path(data=plotdata[plotdata$type=="Percentile", ], aes(x=x, y=y, group=label, color = type))+
      geom_path(data=plotdata[plotdata$type=="Actual", ], aes(x=x, y=y, group=label, color = type))+
      geom_point(data=plotdata[plotdata$type=="Actual", ], aes(x=x, y=y, group=label, color = type), size=2) +
      scale_color_manual(values = colours) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      labs(color = "Type") +
      coord_fixed(ratio=1) +
      ggtitle(Title) +
      annotate(geom="text", x=data2.labels$xpos, y=data2.labels$ypos,
               label = data2.labels$label,
               hjust=data2.labels$hjust, vjust=data2.labels$vjust,
               colour = "white", size = 6, fontface = 2)

    return(p)
}
