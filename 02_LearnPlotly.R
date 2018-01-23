library(plotly)

#Set Session Variables
Sys.setenv("plotly_username"="ashukumar27")
Sys.setenv("plotly_api_key"="0fegl84zgv")


plot_ly(data = iris, x = Sepal.Length, y = Petal.Length, mode = "markers")


#With colors for different classes
plot_ly(data = iris, x = Sepal.Length, y = Petal.Length, mode = "markers", color=Species)


#With colors for different classes & ColorBrewer palette Name
plot_ly(data = iris, x = Sepal.Length, y = Petal.Length, mode = "markers", color=Species,
        colors="Set1")

#Custom Color Scale
# pass RGB or hex color codes directly to colors for finer control
pal <- RColorBrewer::brewer.pal(nlevels(iris$Species), "Set1")
plot_ly(data = iris, x = Sepal.Length, y = Petal.Length, color = Species,
        colors = pal, mode = "markers")

#Adding Color & Size Mapping
d <- diamonds[sample(nrow(diamonds), 1000), ]
# note how size is automatically scaled and added as hover text
plot_ly(d, x = carat, y = price, size = carat, mode = "markers",color=cut, opacity=clarity)


####################################

#Basic Time Series Plot with loess smooth

p <- plot_ly(economics, x = date, y = uempmed, name = "unemployment")
p %>% add_trace(y = fitted(loess(uempmed ~ as.numeric(date))))

#Density Plot
dens <- with(diamonds, tapply(price, INDEX = cut, density))
df <- data.frame(
  x = unlist(lapply(dens, "[[", "x")),
  y = unlist(lapply(dens, "[[", "y")),
  cut = rep(names(dens), each = length(dens[[1]]$x))
)
plot_ly(df, x = x, y = y, color = cut)


#Line Interpolation Options
x <- 1:5
y <- c(1, 3, 2, 3, 1)
plot_ly(x = x, y = y, name = "linear", line = list(shape = "linear")) %>%
  add_trace(y = y + 5, name = "spline", line = list(shape = "spline")) %>%
  add_trace(y = y + 10, name = "vhv", line = list(shape = "vhv")) %>%
  add_trace(y = y + 15, name = "hvh", line = list(shape = "hvh")) %>%
  add_trace(y = y + 20, name = "vh", line = list(shape = "vh")) %>%
  add_trace(y = y + 25, name = "hv", line = list(shape = "hv"))


########################################

#Bar Charts
p <- plot_ly(
  x = c("giraffes", "orangutans", "monkeys"),
  y = c(20, 14, 23),
  name = "SF Zoo",
  type = "bar",
  filename="r-docs/simple-bar"
)
p

#Two variables
p2 <- add_trace(
  p,
  x = c("giraffes", "orangutans", "monkeys"),
  y = c(12, 18, 29),
  name = "LA Zoo",
  filename="r-docs/simple-bars"
)
p2

#Stacked

layout(p2, barmode = "stack")

#Mapping a color variable
ggplot2::diamonds %>% count(cut, clarity) %>%
  plot_ly(x = cut, y = n, type = "bar", color = clarity)



#Dot Plots
s <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/school_earnings.csv")
s <- s[order(s$Men), ]
library(plotly)
p <- plot_ly(s, x = Women, y = School, name = "Women",
             mode = "markers", marker = list(color = "pink")) %>%
  add_trace(x = Men, name = "Men", marker = list(color = "blue")) %>%
  layout(
    title = "Gender earnings disparity",
    xaxis = list(title = "Annual Salary (in thousands)"),
    margin = list(l = 65)
  )
p


#Gauge Chart
base_plot <- plot_ly(
  type = "pie",
  values = c(40, 10, 10, 10, 10, 10, 10),
  labels = c("-", "0", "20", "40", "60", "80", "100"),
  rotation = 108,
  direction = "clockwise",
  hole = 0.4,
  textinfo = "label",
  textposition = "outside",
  hoverinfo = "none",
  domain = list(x = c(0, 0.48), y = c(0, 1)),
  marker = list(colors = c('rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)')),
  showlegend = FALSE
)

base_plot <- add_trace(
  base_plot,
  type = "pie",
  values = c(50, 10, 10, 10, 10, 10),
  labels = c("Error Log Level Meter", "Debug", "Info", "Warn", "Error", "Fatal"),
  rotation = 90,
  direction = "clockwise",
  hole = 0.3,
  textinfo = "label",
  textposition = "inside",
  hoverinfo = "none",
  domain = list(x = c(0, 0.48), y = c(0, 1)),
  marker = list(colors = c('rgb(255, 255, 255)', 'rgb(232,226,202)', 'rgb(226,210,172)', 'rgb(223,189,139)', 'rgb(223,162,103)', 'rgb(226,126,64)')),
  showlegend= FALSE
)

base_plot <- layout(
  base_plot,
  shapes = list(
    list(
      type = 'path',
      path = 'M 0.235 0.5 L 0.24 0.62 L 0.245 0.5 Z',
      xref = 'paper',
      yref = 'paper',
      fillcolor = 'rgba(44, 160, 101, 0.5)'
    )
  ),
  annotations = list(
    list(
      xref = 'paper',
      yref = 'paper',
      x = 0.23,
      y = 0.45,
      showarrow = FALSE,
      text = '50'
    )
  )
)


base_plot
