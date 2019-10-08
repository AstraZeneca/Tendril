## Tendril plot, adjusted with darker border  ##
ggplot2_plotbasic <- function(x, coloring = "Terms") {
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

  aes <- ggplot2::aes
  element_blank <- ggplot2::element_blank
  unit <- ggplot2::unit

  if(coloring %in% c("p", "p.adj", "fish")) {
    cc.10 <- log10(cc)
    cc.10[cc.10<(-3)] <- -3
    palette <- tendril_palette()
    p <- ggplot2::ggplot(data=x$data, aes(x=x, y=y, group=Terms, color=cc.10), aspect="iso") +
      ggplot2::coord_fixed(ratio=1) +
      ggplot2::labs(color = paste("10log", coloring)) +
      ggplot2::geom_path(size=1) +
      ggplot2::geom_point(aes(x=x, y=y, fill=cc.10, colour=cc.10, size=TermsCount), shape=21, stroke=0.1) +
      ggplot2::scale_fill_gradientn(
        colours=palette$grpalette,
        values = palette$values,
        limits = palette$limits
        ) +
      ggplot2::scale_colour_gradientn(
        colours=darken(palette$grpalette),
        values = palette$values,
        limits = palette$limits
        ) +
      ggplot2::theme(
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.key.height=unit(2,"cm")) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      ggplot2::annotate(geom="text", x=data2.labels$xpos, y=data2.labels$ypos,
               label = data2.labels$label,
               hjust=data2.labels$hjust, vjust=data2.labels$vjust,
               colour = "grey40", size = 5, fontface = 2)
  } else {
    p <- ggplot2::ggplot(data=x$data, aes(x=x, y=y, group=Terms, color=cc), aspect="iso") +
      ggplot2::coord_fixed(ratio=1) +
      ggplot2::labs(color = coloring) +
      ggplot2::geom_path(size=1) +
      ggplot2::geom_point(size=3) +
      ggplot2::theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      ggplot2::annotate(geom="text", x=data2.labels$xpos, y=data2.labels$ypos,
               label = data2.labels$label,
               hjust=data2.labels$hjust, vjust=data2.labels$vjust,
               colour = "grey40", size = 5, fontface = 2)
  }

  return(p)
}