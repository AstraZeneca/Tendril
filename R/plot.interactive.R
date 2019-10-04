plot.interactive <- function(tendril, opacity=0.5) {
  `%>%` <- magrittr::`%>%`
  cc= tendril$data[["p.adj"]]

  cc.10 <- log10(cc)
  cc.10[cc.10<(-3)] <- -3
  palette <- tendril_palette()

  p <- tendril$data %>%
    dplyr::group_by(Terms) %>%
    plotly::plot_ly(x=~x, y=~y, width = 700, height = 700,
            mode = "lines+markers", type = "scatter",
            marker = list(size=~TermsCount/100, opacity=opacity), color = ~cc.10,
            colors = palette$grpalette,
            line = list(color = "lightgrey"),
            text = ~paste("Term: ", Terms, '<br>Start day:', StartDay, '<br>p.adjusted:', round(p.adj, 4)),
            hoverinfo = "text") %>%
    plotly::layout(xaxis = list(nticks = 10, showticklabels = FALSE, title = ""),
           yaxis = list(scaleanchor = "x", showticklabels = FALSE, title = "")
    )
  return(p)
}
