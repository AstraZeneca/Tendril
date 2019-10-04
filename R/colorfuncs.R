## Function to create darker/lighter border of marker points  ##
darken <- function(color, factor=1.4){
  col <- grDevices::col2rgb(color)
  col <- col/factor
  col <- grDevices::rgb(t(col), maxColorValue=255)
  col
}

lighten <- function(color, factor=1.4){
  col <- grDevices::col2rgb(color)
  col <- col*factor
  col <- grDevices::rgb(t(as.matrix(apply(col, 1, function(x) {if (x > 255) 255 else x}))), maxColorValue=255)
  # col <- rgb(t(col), maxColorValue=255)
  col
}


tendril_palette <- function() {
  # Returns a list containing information about the palette we use
  grpalette <- grDevices::colorRampPalette( c( "grey15", "red", "darkorange", "gold", "cornflowerblue"  ) )( 5 )
  values <- scales::rescale(c(-3, -1.3, -1, -0.3, 0))
  limits <- c(-3, 0)
  ret <- list(
    grpalette = grpalette,
    values = values,
    limits = limits
  )
  return(ret)
}

