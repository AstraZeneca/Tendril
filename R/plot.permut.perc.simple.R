## Customize permutation plot 2 ( Grey simulated - p.adj actual ) ##
plot.permut.perc.simple <- function(tendril_perm) {

  percentileData <- data.frame(
    x = tendril_perm$tendril.pi$x,
    y = tendril_perm$tendril.pi$y
  )
  percentileData$type = tendril_perm$tendril.pi$type
  percentileData$label = tendril_perm$tendril.pi$label

  actualData <- data.frame(
    x = tendril_perm$tendril$data[tendril_perm$tendril$data$Terms==tendril_perm$Permterm,]$x,
    y = tendril_perm$tendril$ata[tendril_perm$tendril$data$Terms==tendril_perm$Permterm,]$y,
    p.adj = tendril_perm$tendril$data[tendril_perm$tendril$data$Terms==tendril_perm$Permterm,]$p.adj
    )
  actualData$type="Actual data"
  actualData$label="Actual data"

  cc <- actualData$p.adj
  cc.10 <- log10(cc)
  cc.10[cc.10<(-3)] <- -3
  palette <- tendril_palette()

  element_blank <- ggplot2::element_blank
  aes <- ggplot2::aes

  ggplot2::ggplot(data=tendril_perm$perm.data,
    aes(x=x, y=y, group=label), aspect="iso") +
    ggplot2::scale_colour_gradientn(
      colours=palette$grpalette,
      values = palette$values,
      limits = palette$limits
      ) +
    ggplot2::geom_path(colour="grey80") +
    ggplot2::geom_path(data=percentileData, size=1, colour="grey50") +
    ggplot2::geom_path(data=actualData, aes(colour=cc.10), size=1) +
    ggplot2::geom_point(data=actualData, aes(colour=cc.10), size=2) +
    ggplot2::coord_fixed(ratio=1)  +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    #ggplot2::ggtitle(tendril_perm$Permterm)
}
