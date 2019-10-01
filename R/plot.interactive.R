plot.interactive <- function(tendril, opacity=0.5) {
  cc= tendril$data[["p.adj"]]

  cc.10 <- log10(cc)
  cc.10[cc.10<(-3)] <- -3
  tendrilpal <- colorRampPalette( c( "grey15", "red", "darkorange", "gold", "cornflowerblue"  ) )( 5 )
  vals <- scales::rescale(c(-3, -1.3, -1, -0.3, 0))

  p <- tendril$data %>%
    group_by(Terms) %>%
    plot_ly(x=~x, y=~y, width = 700, height = 700,
            mode = "lines+markers", type = "scatter",
            marker = list(size=~TermsCount/100, opacity=opacity), color = ~cc.10,
            colors = tendrilpal,
            line = list(color = "lightgrey"),
            text = ~paste("Term: ", Terms, '<br>Start day:', StartDay, '<br>p.adjusted:', round(p.adj, 4)),
            hoverinfo = "text") %>%
    layout(xaxis = list(nticks = 10, showticklabels = FALSE, title = ""),
           yaxis = list(scaleanchor = "x", showticklabels = FALSE, title = "")
    )
  return(p)
}
