plotly_plotbasic <- function(tendril, coloring, opacity=0.5) {
  `%>%` <- magrittr::`%>%`

  plotdata <- tendril$data
  plotdata<-dplyr::arrange(plotdata, Terms, StartDay)
  
  palette <- tendril_palette()
  max_termscount <- max(plotdata$TermsCount, na.rm = TRUE)
  
  if(coloring %in% c("p", "p.adj", "fish")) {
    plotdata$cc.10 <- pmax(log10(plotdata[[coloring]]), -3)

    p <- plotdata %>% 
      dplyr::group_by(Terms) %>% 
      plotly::plot_ly(x=~x, y=~y,type="scatter",mode="lines",
              line = list(color = "lightgrey"),
              showlegend=FALSE) %>%
      dplyr::ungroup() %>%
      plotly::add_markers(x=~x, y=~y, 
                  mode = "markers", type = "scatter", 
                  marker = list(size=~(TermsCount/max_termscount)*10,
                                opacity=opacity,
                                color = ~cc.10, 
                                colorscale="RdBu",
                                colorbar=list(title="log(p-val)")), 
                  text = ~paste("Term: ", Terms, '<br>Start day:', StartDay, '<br>p.adjusted:', round(p.adj, 4)), 
                  hoverinfo = "text",
                  inherit=FALSE) %>%
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
  } else {
    p <- plotdata %>% 
      dplyr::group_by(Terms) %>% 
      plotly::plot_ly(x=~x, y=~y,type="scatter",mode="lines+markers",
                      marker = list(size=~(TermsCount/max_termscount)*10,
                                    opacity=opacity),
                      color = ~Terms,
                      colors="Dark2",
                      text = ~paste("Term: ", Terms, '<br>Start day:', StartDay, '<br>p.adjusted:', round(p.adj, 4)), 
                      hoverinfo = "text",
                      showlegend=TRUE) %>%
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
  }

  return(p)
}
