plotly_plotbasic <- function(tendril, coloring, term) {
  `%>%` <- magrittr::`%>%`

  plotdata <- tendril$data
  plotdata<-dplyr::arrange(plotdata, Terms, StartDay)

  palette <- tendril_palette()
  max_termscount <- max(plotdata$TermsCount, na.rm = TRUE)

  x.min <- min(plotdata$x)
  x.max <- max(plotdata$x)
  y.min <- min(plotdata$y)
  y.max <- max(plotdata$y)

  if (length(term) > 0) {
    plotdata <- plotdata[plotdata$Terms %in% term,]
  }

  if (!is.null(term)) {
    plotdata$Terms<-factor(plotdata$Terms,levels=term)
  }

  if(coloring %in% LOG_COLORING_OPTIONS) {
    plotdata$cc.10 <- pmax(log10(plotdata[[coloring]]), -3)
    palette <- tendril_palette()

    colorscale = list()
    for (i in seq_len(length(palette$values))) {
      colorscale[[i]] <- c(palette$values[[i]], palette$grpalette[[i]])
    }

    p <- plotdata %>%
      dplyr::group_by(Terms) %>%
      plotly::plot_ly(x=~x, y=~y,type="scatter",mode="lines",
              line = list(color = "lightgrey"),
              showlegend=FALSE) %>%
      dplyr::ungroup() %>%
      plotly::add_markers(x=~x, y=~y,
                  mode = "markers", type = "scatter",
                  marker = list(size=~(TermsCount/max_termscount)*10,
                                color = ~cc.10,
                                colorscale=colorscale,
                                cmin=palette$limits[[1]],
                                cmax=palette$limits[[2]],
                                line=list(
                                  width = 0
                                ),

                                colorbar=list(title="log(p-val)")),
                  text = ~paste("Term: ", Terms,
                                '<br>Start day:', StartDay,
                                '<br>Frequency:', TermsCount,
                                '<br>p.adjusted:', round(p.adj, 4),
                                sep=""),
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
      plotly::layout(xaxis = list(nticks = 10,
                                  range = c(x.min, x.max),
                                  showticklabels = FALSE,
                                  title = ""),
                     yaxis = list(scaleanchor = "x",
                                  range=c(y.min, y.max),
                                  showticklabels = FALSE,
                                  title = "")
      )
  } else {
    p <- plotdata %>%
      dplyr::group_by(Terms) %>%
      plotly::plot_ly(x=~x, y=~y,type="scatter",mode="lines+markers",
                      marker = list(size=~(TermsCount/max_termscount)*10),
                      color = ~Terms,
                      colors="Dark2",
                      text = ~paste("Term: ", Terms,
                                    '<br>Start day:', StartDay,
                                    '<br>Frequency:', TermsCount,
                                    '<br>p.adjusted:', round(p.adj, 4),
                                    sep=""),
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
      plotly::layout(xaxis = list(nticks = 10,
                                  range = c(x.min, x.max),
                                  showticklabels = FALSE,
                                  title = ""),
                     yaxis = list(scaleanchor = "x",
                                  range=c(y.min, y.max),
                                  showticklabels = FALSE,
                                  title = "")
      )
  }

  return(p)
}
