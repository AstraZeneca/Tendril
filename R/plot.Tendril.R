#' Plot Tendril
#' @description
#' Function to plot Tendril results
#' @param x An object of class tendril, as made by Tendril()
#' @param term The term to extract.
#' @param coloring Name of column used for coloring tendrils (only basic plots).
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
#' @param interactive Specifies if the plot must be interactive or not.
#' If interactive == TRUE, plotly will be used to render the plot. Otherwise,
#' (default) the plot will be rendered as a static image using ggplot2.
#' @param ... unused
#' @details
#' If saving the results of the function to a variable, this will be of class tendril
#' and will contain the data passed to the plot function and the plot itself
#'
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
#'
#' #Do plot
#' res <- plot(data, coloring = "Terms")
#' res <- plot(data, coloring = "p.adj")
#'
#' #To re-do the plot after the first call:
#' print(res)
#' @export

plot.Tendril <- function(x, term=NULL, coloring="Terms", interactive=FALSE, ...) {

  if (!is.null(term) && !(all(term %in% levels(x$data$Terms)))) {
    stop(paste("Specified term",
               term,
               "is not in the list of available terms", x$Terms, '\n'))
  }

  if (!interactive) {
    p <- ggplot2_plotbasic(x, coloring=coloring, term=term)
  } else {
    p <- plotly_plotbasic(x, coloring=coloring, term=term)
  }

  return(p)
}
