#' Plot TendrilPerm
#' @description
#' Function to plot TendrilPerm results
#' @param x An object of class TendrilPerm, as made by TendrilPerm()
#' @param coloring Name of column used for coloring tendrils.
#' Available coloring choices are:
#'   "Terms" - One tendril one color;
#'   "p" - Prop.test;
#'   "p.adj" - P-values adjusted using the False discovery rate method within each tendril;
#'   "fish" - Fisher.test;
#'   "rdiff" - Risk difference;
#'   "RR" - Risk Ratio;
#'   "OR" - Odds Ratio;
#'   "FDR.tot" - P-values adjusted using the False discovery rate method for all tendrils; and
#'   "TermsCount" - Total number of events for that specific type of event
#' @param percentile Specifies if the plot must show percentile values.
#' Default is FALSE.
#' @param ... unused
#' @examples
#' # generate data using Tendril()
#' data <- Tendril(mydata = TendrilData,
#' rotations = Rotations,
#' AEfreqThreshold = 9,
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
plot.TendrilPerm <- function(x, coloring="p.adj", percentile=FALSE, ...) {
  if (percentile){
    p <- plot_percentile(x, coloring)
  } else {
    p <- plot_permutation(x, coloring)
  }
  return(p)
}
