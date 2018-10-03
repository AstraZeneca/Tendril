plotbasic <- function(x, coloring = "Terms") {

  plotdata=data.frame(x= x$data$x,
                      y= x$data$y,
                      Terms= x$data$Terms
  )
  cc= x$data[[coloring]]

  data2.labels <- data.frame(
    xpos = c(Inf, -Inf),
    ypos = c(Inf, Inf),
    label = x$Treatments,
    hjust = c(1,0),
    vjust = c(1,1)
  )

  y<-NULL
  Terms <- NULL

  if(coloring %in% c("p", "p.adj", "fish")) {
    if(is.null(x$SubjList)){
      stop(paste0("There is no SubjList supplied in the Tendril object and therefore the ", coloring, " value is not present."))
    }
    cc.10 <- log10(cc)
    cc.10[cc.10<(-3)] <- -3
    tendrilpal <- colorRampPalette( c( "grey15", "red", "darkorange", "gold", "cornflowerblue"  ) )( 5 )
    vals <- rescale(c(-3, -1.3, -1, -0.3, 0))
    limits_colorbar <- c(-3, 0)
    p <- ggplot(data=x$data, aes(x=x, y=y, group=Terms, color=cc.10), aspect="iso") +
      scale_colour_gradientn(colours=tendrilpal, values = vals, limits = limits_colorbar) +
      coord_fixed(ratio=1) +
      labs(color = paste("10log", coloring)) +
      geom_path(size=1) +
      geom_point(size=3) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.key.height=unit(2,"cm")) +
      annotate(geom="text", x=data2.labels$xpos, y=data2.labels$ypos,
               label = data2.labels$label,
               hjust=data2.labels$hjust, vjust=data2.labels$vjust,
               colour = "white", size = 6, fontface = 2)
  } else {
    p <- ggplot(data=x$data, aes(x=x, y=y, group=Terms, color=cc), aspect="iso") +
      coord_fixed(ratio=1) +
      labs(color = coloring) +
      geom_path(size=1) +
      geom_point(size=3) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      annotate(geom="text", x=data2.labels$xpos, y=data2.labels$ypos,
               label = data2.labels$label,
               hjust=data2.labels$hjust, vjust=data2.labels$vjust,
               colour = "white", size = 6, fontface = 2)
  }

  return(p)
}




