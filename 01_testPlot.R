library(plotly)

#Set Session Variables
Sys.setenv("plotly_username"="ashukumar27")
Sys.setenv("plotly_api_key"="0fegl84zgv")

p <- plot_ly(midwest, x = percollege, color = state, type = "box")
plotly_POST(p, filename = "r-docs/midwest-boxplots", world_readable=TRUE)


set.seed(100)
d <- diamonds[sample(nrow(diamonds), 1000), ]
plot_ly(d, x = carat, y = price, text = paste("Clarity: ", clarity),
        mode = "markers", color = carat, size = carat)


##GGPLOTLY

p <- ggplot(data = d, aes(x = carat, y = price)) +
  geom_point(aes(text = paste("Clarity:", clarity)), size = 4) +
  geom_smooth(aes(colour = cut, fill = cut)) + facet_wrap(~ cut)

(gg <- ggplotly(p))

plot_ly(z = volcano, type = "surface")

#Examples
str(p <- plot_ly(economics, x = date, y = uempmed))
p %>%
  add_trace(y = fitted(loess(uempmed ~ as.numeric(date)))) %>%
  layout(title = "Median duration of unemployment (in weeks)",
         showlegend = FALSE) %>%
  dplyr::filter(uempmed == max(uempmed)) %>%
  layout(annotations = list(x = date, y = uempmed, text = "Peak", showarrow = T))
