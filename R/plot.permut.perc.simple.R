## Customize permutation plot 2 ( Grey simulated - p.adj actual ) ##
plot.permut.perc.simple <- function(Tendril.perm) {
  Tendril.perm$perm.data[Tendril.perm$perm.data$ang<0,]$ang <- Tendril.perm$perm.data[Tendril.perm$perm.data$ang<0,]$ang + 2*pi
  Tendril.perm$tendril.pi <- TendrilPi(Tendril.perm)

  percentileData <- data.frame(x=Tendril.perm$tendril.pi$x, y=Tendril.perm$tendril.pi$y)
  percentileData$type=Tendril.perm$tendril.pi$type
  percentileData$label=Tendril.perm$tendril.pi$label
  actualData <- data.frame(x=Tendril.perm$data[Tendril.perm$data$Terms==Tendril.perm$Permterm,]$x,
                           y=Tendril.perm$data[Tendril.perm$data$Terms==Tendril.perm$Permterm,]$y,
                           p.adj=Tendril.perm$data[Tendril.perm$data$Terms==Tendril.perm$Permterm,]$p.adj)
  actualData$type="Actual data"
  actualData$label="Actual data"

  cc <- actualData$p.adj
  cc.10 <- log10(cc)
  cc.10[cc.10<(-3)] <- -3
  tendrilpal <- colorRampPalette( c( "grey15", "red", "darkorange", "gold", "cornflowerblue"  ) )( 5 )
  vals <- scales::rescale(c(-3, -1.3, -1, -0.3, 0))

  ggplot(data=Tendril.perm$perm.data, aes(x=x, y=y, group=label), aspect="iso") +
    scale_colour_gradientn(colours=tendrilpal, values = vals, limits = c(-3, 0)) +
    geom_path(colour="grey80") +
    geom_path(data=percentileData, size=1, colour="grey50") +
    geom_path(data=actualData, aes(colour=cc.10), size=1) +
    geom_point(data=actualData, aes(colour=cc.10), size=2) +
    coord_fixed(ratio=1)  +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ggtitle(Tendril.perm$Permterm)
}
