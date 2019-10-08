#' Plot TendrilPerm
#' @description
#' Function to plot TendrilPerm results
#' @param x An object of class TendrilPerm, as made by TendrilPerm()
#' @param percentile Specifies if the plot must show percentile values. Default
#' is FALSE.
#' @examples
#' # generate data using Tendril()
#' data <- Tendril(mydata = TendrilData,
#' rotations = Rotations,
#' AEfreqTreshold = 9,
#' Tag = "Comment",
#' Treatments = c("placebo", "active"),
#' Unique.Subject.Identifier = "subjid",
#' Terms = "ae",
#' Treat = "treatment",
#' StartDay = "day",
#' SubjList = SubjList,
#' SubjList.subject = "subjid",
#' SubjList.treatment = "treatment"
#' )
#' tendril_perm <- TendrilPerm(
#'   tendril = data,
#'   PermTerm="AE40",
#'   n.perm = 200,
#'   perm.from.day = 1)
#'
#' #Do plot
#' res <- plot(tendril_perm)
#' res <- plot(tendril_perm, percentile = TRUE)
#'
#' #To re-do the plot after the first call:
#' print(res)
#' @export
plot.TendrilPerm <- function(x, ...) {
  params <- as.list(substitute(list(...)))

  if (is.null(params$percentile)) {
    params$percentile = FALSE
  }

  if (params$percentile){
    p <- plotpercentile(x)
  } else {
    p <- plotpermutation(x)
  }
  return(p)
}
