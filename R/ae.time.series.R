## Plot time series of net events on active arm ##
ae.time.series <- function(tendril.data, term) {
  time.serie <- ddply(tendril.data$data[tendril.data$data$Terms == term,], .(StartDay, Treat), summarize, n=length(Unique.Subject.Identifier))
  time.s.2 <- dcast(time.serie, StartDay ~ Treat, fun.aggregate = mean)

  cum.na <- function(x) {
    x[which(is.na(x))] <- 0
    return(cumsum(x))
  }

  time.s.2$Placebo.cumsum <- cum.na(time.s.2$Placebo)
  time.s.2$Active.cumsum <- cum.na(time.s.2$`Ticagrelor 90mg bd`)
  time.s.2$Net <- time.s.2$Active.cumsum-time.s.2$Placebo.cumsum
  ggplot(data = time.s.2, aes(x=StartDay, y=Net, col=)) + ggtitle(term) + geom_point() + geom_line() + xlab("Day since randomization") + ylab("Net events on active") # + ylim(c(-5, 80))
}
