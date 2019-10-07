plot.TendrilPerm <- function(tendril_perm, percentile = FALSE, ...) {
  if (!percentile){
    p <- plotpermutation(tendril_perm)
  }
  else {
    p <- plotpercentile(tendril_perm)
  }
  return(p)
}
