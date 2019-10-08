## Plot time series of net events on active arm ##
ae_timeseries <- function(tendril.data, term) {
  . <- plyr::.
  summarize <- plyr::summarize

  time.serie <- plyr::ddply(tendril.data$data[tendril.data$data$Terms %in% term,], .(Terms, Treat, StartDay), summarize, n=length(Unique.Subject.Identifier))
  time.s.2 <- reshape2::dcast(time.serie, Terms + StartDay ~ Treat, fun.aggregate = mean, value.var = "n")

  cum.na <- function(x) {
    x[which(is.na(x))] <- 0
    return(cumsum(x))
  }

  time.grp <- dplyr::group_by(time.s.2, Terms)
  time.grp <- dplyr::mutate_at(time.grp, 2:3, cum.na)
  time.s.2 <- dplyr::ungroup(time.grp)
  time.s.2 <- as.data.frame(time.s.2)
  
  time.s.2$Net <- time.s.2[,3]-time.s.2[,4]
  time.s.2$Terms<-factor(time.s.2$Terms,levels=term)
  
  plot <- ggplot2::ggplot(
    data = time.s.2,
    ggplot2::aes(x=StartDay, y=Net, col=Terms)) +
#    ggplot2::ggtitle(term) +
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
