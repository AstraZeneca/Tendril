## Plot single tendrils ##
plot_term <- function(tendril.data, term) {
  data <- tendril.data$data[tendril.data$data$Terms %in% term,]
  cc <- data$p.adj
  coloring <- "p.adj"

  data2.labels <- data.frame(
    xpos = c(Inf, -Inf),
    ypos = c(Inf, Inf),
    label = tendril.data$Treatments,
    hjust = c(1,0),
    vjust = c(1,1)
  )

  y<-NULL
  Terms <- NULL

  cc.10 <- log10(cc)
  cc.10[cc.10<(-3)] <- -3
  palette <- tendril_palette()
  element_blank <- ggplot2::element_blank
  unit <- ggplot2::unit
  aes <- ggplot2::aes

  p <- ggplot2::ggplot(
    data=data,
    aes(x=x, y=y, group=Terms, color=cc.10), aspect="iso") +
    ggplot2::scale_fill_gradientn(
      colours = palette$grpalette,
      values = palette$values,
      limits = palette$limits
      ) +
    ggplot2::scale_colour_gradientn(
      colours=darken(palette$grpalette),
      values = palette$values,
      limits = palette$limits
      ) +
    ggplot2::coord_fixed(ratio=1) +
    ggplot2::labs(color = paste("10log", coloring)) +
    ggplot2::ggtitle(paste(term,  collapse = "; ")) +
    ggplot2::geom_path(size=0.5) +
    ggplot2::geom_point(aes(x=x, y=y, fill=cc.10, colour=cc.10, size=TermsCount), shape=21, stroke=0.1) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.key.height=unit(2,"cm"),
          panel.border = element_blank()) +
    ggplot2::theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  return(p)
}
