#' Plot Tendril
#' @description
#' Function to plot Tendril results
#' @param x An object of class tendril, as made by Tendril()
#' @param type A character vector of length 1 describing the type of plot to be produced.
#' Available types are: "basic", "permutations" and "percentile"
#' @param coloring Name of column used for coloring tendrils (only basic plots).
#' Available coloring choices are:
#' "Terms" - One tendril one color;
#' "p" - Prop.test;
#' "p.adj" - P-values adjusted using the False discovery rate method within each tendril;
#' "fish" - Fisher.test;
#' "rdiff" - Risk difference;
#' "RR" - Risk Ratio;
#' "OR" - Odds Ratio;
#' "FDR.tot" - P-values adjusted using the False discovery rate method for all tendrils; and
#' "TermsCount" - Total number of events for that specific type of event
#' @param ... Unused arguments
#' @details
#' If saving the results of the function to a variable, this will be of class tendril
#' and will contain the data passed to the plot function and the plot itself
#'
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
#'
#' #Do plot
#' res <- plot(data, type = "basic", coloring = "Terms")
#' res <- plot(data, type = "basic", coloring = "p.adj")
#'
#' #To re-do the plot after the first call:
#' print(res)
#' @export

plot.tendril <- function(x, type = c("basic","permutations","percentile")[1], coloring = "Terms", ...){

  if (type == "basic"){
    p<-plotbasic(x, coloring=coloring, ...)
  }
  else if (type == "permutations"){
    p<-plotpermutation(x)
  }
  else if (type == "percentile"){
    p<-plotpercentile(x)
  }
  else {
    stop("Invalid type. Must be one of the following: basic, permutations or percentile")
  }

  return(p)
}
