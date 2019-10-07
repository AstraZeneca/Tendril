plotly_plotbasic <- function(tendril, coloring, opacity=0.5) {
  `%>%` <- magrittr::`%>%`

  cc= tendril$data[[coloring]]

  if(coloring %in% c("p", "p.adj", "fish")) {
    cc <- log10(cc)
    cc[cc<(-3)] <- -3
  }
  palette <- tendril_palette()

  p <- tendril$data %>%
    dplyr::group_by(Terms) %>%
    plotly::plot_ly(x=~x, y=~y, width = 700, height = 700,
            mode = "lines+markers", type = "scatter",
            marker = list(size=~TermsCount/100, opacity=opacity), color = ~cc,
            colors = palette$grpalette,
            line = list(color = "lightgrey"),
            text = ~paste("Term: ", Terms, '<br>Start day:', StartDay, '<br>p.adjusted:', round(p.adj, 4)),
            hoverinfo = "text") %>%
    plotly::add_annotations(
      x = 0,
      y = 1,
      xref = "paper",
      yref = "paper",
      text = tendril$Treatments[2],
      xanchor = "left",
      showarrow = F
    ) %>%
    plotly::add_annotations(
      x = 1,
      y = 1,
      xref = "paper",
      yref = "paper",
      text = tendril$Treatments[1],
      xanchor = "right",
      showarrow = F
    ) %>%
    plotly::layout(xaxis = list(nticks = 10, showticklabels = FALSE, title = ""),
           yaxis = list(scaleanchor = "x", showticklabels = FALSE, title = "")
    )

  return(p)
}
