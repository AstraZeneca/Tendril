## Plot time series of net events on active arm ##
ae_timeseries <- function(tendril, term) {
  . <- plyr::.
  summarize <- plyr::summarize

  time.serie <- plyr::ddply(tendril$data[tendril$data$Terms == term,], .(StartDay, Treat), summarize, n=length(Unique.Subject.Identifier))
  if (nrow(time.serie) == 0) {
    stop(paste("No matching data for selected term value", term))
  }
  time.s.2 <- reshape2::dcast(time.serie, StartDay ~ Treat, fun.aggregate = mean)

  cum.na <- function(x) {
    x[which(is.na(x))] <- 0
    return(cumsum(x))
  }

  treatments <- tendril$Treatments
  treatments_cumsum <- paste0(treatments, ".cumsum")
  time.s.2[[ treatments_cumsum[[1]] ]] <- cum.na(time.s.2[[treatments[[1]]]])
  time.s.2[[ treatments_cumsum[[2]] ]] <- cum.na(time.s.2[[treatments[[2]]]])
  time.s.2$net <- (
    time.s.2[[ treatments_cumsum[[2]] ]] -
    time.s.2[[ treatments_cumsum[[1]] ]])

  plot <- ggplot2::ggplot(
    data = time.s.2,
    ggplot2::aes(x=StartDay, y=net, col=)) +
    ggplot2::ggtitle(term) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::xlab("Day since randomization") +
    ggplot2::ylab("Net events on active") # + ylim(c(-5, 80))
  return(plot)
}
