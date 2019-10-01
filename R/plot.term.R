## Plot single tendrils ##
plot.term <- function(tendril.data, term) {
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
  tendrilpal <- colorRampPalette( c( "grey15", "red", "darkorange", "gold", "cornflowerblue"  ) )( 5 )
  vals <- scales::rescale(c(-3, -1.3, -1, -0.3, 0))
  p <- ggplot(data=data, aes(x=x, y=y, group=Terms, color=cc.10), aspect="iso") +
    scale_fill_gradientn(colours=tendrilpal, values = vals, limits = c(-3, 0)) +
    scale_colour_gradientn(colours=darken(tendrilpal), values = vals, limits = c(-3, 0)) +
    coord_fixed(ratio=1) +
    labs(color = paste("10log", coloring)) +
    ggtitle(paste(term,  collapse = "; ")) +
    geom_path(size=0.5) +
    geom_point(aes(x=x, y=y, fill=cc.10, colour=cc.10, size=TermsCount), shape=21, stroke=0.1) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.key.height=unit(2,"cm"),
          panel.border = element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  p
}
