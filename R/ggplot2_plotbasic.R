## Tendril plot, adjusted with darker border  ##
ggplot2_plotbasic <- function(x, coloring = "Terms", term = NULL) {
  plotdata <- data.frame(x = x$data$x,
                         y = x$data$y,
                         Terms = x$data$Terms,
                         cc = x$data[[coloring]]
  )

  x.min <- min(plotdata$x)
  x.max <- max(plotdata$x)
  y.min <- min(plotdata$y)
  y.max <- max(plotdata$y)

  if (length(term) > 0) {
    plotdata <- plotdata[plotdata$Terms %in% term,]
  }

  if (!is.null(term)) {
    plotdata$Terms<-factor(plotdata$Terms,levels=term)
  }

  data2.labels <- data.frame(
    xpos = c(Inf, -Inf),
    ypos = c(Inf, Inf),
    label = x$Treatments,
    hjust = c(1,0),
    vjust = c(1,1)
  )

  y<-NULL
  Terms <- NULL

  aes <- ggplot2::aes
  element_blank <- ggplot2::element_blank
  unit <- ggplot2::unit

  if(coloring %in% LOG_COLORING_OPTIONS) {
    plotdata$cc.10 <- pmax(log10(plotdata$cc),-3)
    palette <- tendril_palette()
    p <- ggplot2::ggplot(data=plotdata, aes(x=x, y=y, group=Terms, color=cc.10)) +
      ggplot2::coord_fixed(ratio=1) +
      ggplot2::xlim(x.min, x.max) +
      ggplot2::ylim(y.min, y.max) +
      ggplot2::labs(color = paste("10log", coloring)) +
      ggplot2::geom_path(size=1) +
      ggplot2::geom_point(aes(x=x, y=y, fill=cc.10, colour=cc.10, size=TermsCount), shape=21, size=1.5, stroke=0.5) +
      ggplot2::scale_fill_gradientn(
        "log(p-val)",
        colours=palette$grpalette,
        values = palette$values,
        limits = palette$limits
        ) +
      ggplot2::scale_colour_gradientn(
        colours=darken(palette$grpalette),
        values = palette$values,
        limits = palette$limits
        ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.key.height=unit(2,"cm")) +
      ggplot2::theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      ggplot2::annotate(geom="text", x=data2.labels$xpos, y=data2.labels$ypos,
               label = data2.labels$label,
               hjust=data2.labels$hjust, vjust=data2.labels$vjust,
               colour = "grey40", size = 5, fontface = 2) +
      ggplot2::guides(color=FALSE)
    if (length(term)==1) {
      p <- p +
        ggplot2::ggtitle(term)
    }
  } else {
    if (length(term)==1) {
      p <- ggplot2::ggplot(data=plotdata, aes(x=x, y=y)) +
        ggplot2::coord_fixed(ratio=1) +
        ggplot2::xlim(x.min, x.max) +
        ggplot2::ylim(y.min, y.max) +
        ggplot2::geom_path(size=1) +
        ggplot2::geom_point(size=2) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.title.x = element_blank(),
                       axis.text.x = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.title.y = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.y = element_blank()) +
        ggplot2::theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        ggplot2::annotate(geom="text", x=data2.labels$xpos, y=data2.labels$ypos,
                          label = data2.labels$label,
                          hjust=data2.labels$hjust, vjust=data2.labels$vjust,
                          colour = "grey40", size = 5, fontface = 2) +
        ggplot2::ggtitle(term)
    } else {
      p <- ggplot2::ggplot(data=plotdata, aes(x=x, y=y, group=Terms, color=cc)) +
        ggplot2::coord_fixed(ratio=1) +
        ggplot2::xlim(x.min, x.max) +
        ggplot2::ylim(y.min, y.max) +
        ggplot2::labs(color = coloring) +
        ggplot2::geom_path(size=1) +
        ggplot2::geom_point(size=2) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.title.x = element_blank(),
                       axis.text.x = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.title.y = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.y = element_blank()) +
        ggplot2::theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        ggplot2::annotate(geom="text", x=data2.labels$xpos, y=data2.labels$ypos,
                          label = data2.labels$label,
                          hjust=data2.labels$hjust, vjust=data2.labels$vjust,
                          colour = "grey40", size = 5, fontface = 2)

    }
  }

  return(p)
}
