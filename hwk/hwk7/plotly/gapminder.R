rm(list = ls())
options(stringsAsFactors = FALSE)
library(plotly)
library(gapminder)

# example from notes
plot_ly(gapminder, 
        x = ~gdpPercap, 
        y = ~lifeExp, 
        z = ~pop,
        size = ~pop, 
        color = ~continent, 
        colors = c("green", "purple", "blue", "orange", "red", "yellow"),
        frame = ~year, 
        text = ~paste("Country:", country, 
                      "<br>Pop.:", pop,
                      "<br>Life Exp.:", lifeExp,
                      "<br>GDP:", gdpPercap)) %>% 
  add_markers(marker = list(symbol = "circle", sizemode = "diameter")) %>% 
  layout(title = "Population Versus Life Expectancy Versus GDP Over Time",
         scene = list(xaxis = list(title = "GDP",
                           type = "log"),
                      yaxis = list(title = "Life Expectancy",
                           type = "log"),
                      zaxis = list(title = "Population",
                           type = "log")))

