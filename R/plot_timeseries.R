#' Plot time series
#' @description
#' Plot time series of net events on a second treatment arm
#' @param tendril An object of class Tendril, as made by Tendril()
#' @param term A character vector describing the value or values of Term to
#' select; defaults to \code{NULL} which corresponds to all values
#' @examples
#' # generate data using Tendril()
#' data <- Tendril(
#'   mydata = TendrilData,
#'   rotations = Rotations,
#'   AEfreqThreshold = 9,
#'   Tag = "Comment",
#'   Treatments = c("placebo", "active"),
#'   Unique.Subject.Identifier = "subjid",
#'   Terms = "ae",
#'   Treat = "treatment",
#'   StartDay = "day",
#'   SubjList = SubjList,
#'   SubjList.subject = "subjid",
#'   SubjList.treatment = "treatment"
#' )
#'
#' # do plot
#' plot_timeseries(data, term="AE33")
#' plot_timeseries(data, term=c("AE33","AE40"))
#' plot_timeseries(data, term=NULL)
#' @export

plot_timeseries <- function(tendril, term=NULL) {
  . <- plyr::.
  summarize <- plyr::summarize

  if (length(term)==0) {
    time.serie <- plyr::ddply(tendril$data,
                              .(Terms, Treat, StartDay),
                              summarize, n = length(Unique.Subject.Identifier))
  } else {
    time.serie <- plyr::ddply(tendril$data[tendril$data$Terms %in% term,],
                              .(Terms, Treat, StartDay),
                              summarize, n = length(Unique.Subject.Identifier))
  }
  if (nrow(time.serie) == 0) {
    stop(paste("No matching data for selected term value", term))
  }
  time.s.2 <- reshape2::dcast(time.serie, Terms + StartDay ~ Treat, fun.aggregate = mean, value.var = "n")

  cum.na <- function(x) {
    x[which(is.na(x))] <- 0
    return(cumsum(x))
  }

  time.grp <- dplyr::group_by(time.s.2, Terms)
  time.grp <- dplyr::mutate_at(time.grp, 3:4, cum.na)
  time.s.2 <- dplyr::ungroup(time.grp)
  time.s.2 <- as.data.frame(time.s.2)

  time.s.2$Net <- time.s.2[,3]-time.s.2[,4]
  if (!is.null(term)) {
    time.s.2$Terms<-factor(time.s.2$Terms,levels=term)
  }

  if (length(term)==1) {
    plot <- ggplot2::ggplot(data = time.s.2,
                            ggplot2::aes(x=StartDay, y=Net)) +
      ggplot2::ggtitle(term)
  } else {
    plot <- ggplot2::ggplot(data = time.s.2,
                            ggplot2::aes(x=StartDay, y=Net, col=Terms))
  }
  plot <- plot +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme_bw() +
    ggplot2::xlab("Day since randomization") +
    ggplot2::ylab(paste("Net events - ",
                        names(time.s.2)[3],
                        " over ",
                        names(time.s.2)[4],
                        sep = ""))
  return(plot)
}
