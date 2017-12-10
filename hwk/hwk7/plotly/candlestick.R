library(quantmod)
library(plotly)
library(lubridate)
rm(list = ls())

getSymbols("AAPL", src = "yahoo")
# make dataframe with date column
raw <- data.frame(Date = index(AAPL), coredata(AAPL))
# filter to 2017
raw <- raw %>%
    mutate(year = year(Date)) %>% 
    filter(year == 2017) %>% 
    select(-year)

# first tutorial example
plot_ly(raw, x = ~Date, xend = ~Date, color = ~AAPL.Close > AAPL.Open, 
        colors = c("red", "forestgreen"), hoverinfo = "none") %>%
    add_segments(y = ~AAPL.Low, yend = ~AAPL.High, size = I(1)) %>%
    add_segments(y = ~AAPL.Open, yend = ~AAPL.Close, size = I(3)) %>%
    layout(showlegend = FALSE, yaxis = list(title = "Price"))
# second tutorial example
plot_ly(raw, x = ~Date, type="candlestick", open = ~AAPL.Open, close = ~AAPL.Close, high = ~AAPL.High, low = ~AAPL.Low) %>%
    layout(showlegend = FALSE)

# from plotly website
msft <- getSymbols("MSFT", auto.assign = F)
dat <- as.data.frame(msft)
dat$date <- index(msft)
dat <- subset(dat, date >= "2016-01-01")

names(dat) <- sub("^MSFT\\.", "", names(dat))

p <- plot_ly(dat, x = ~date, xend = ~date, color = ~Close > Open,
             colors = c("red", "forestgreen"), hoverinfo = "none") %>%
    add_segments(y = ~Low, yend = ~High, size = I(1)) %>%
    add_segments(y = ~Open, yend = ~Close, size = I(3)) %>%
    layout(showlegend = FALSE, yaxis = list(title = "Price")) %>%
    rangeslider()
p

# another plotly example
getSymbols("AAPL",src='yahoo')
df <- data.frame(Date=index(AAPL),coredata(AAPL))

# create Bollinger Bands
bbands <- BBands(AAPL[,c("AAPL.High","AAPL.Low","AAPL.Close")])

# join and subset data
df <- subset(cbind(df, data.frame(bbands[,1:3])), Date >= "2017-01-01")

# colors column for increasing and decreasing
for (i in 1:length(df[,1])) {
    if (df$AAPL.Close[i] >= df$AAPL.Open[i]) {
        df$direction[i] = 'Increasing'
    } else {
        df$direction[i] = 'Decreasing'
    }
}

i <- list(line = list(color = 'forestgreen'))
d <- list(line = list(color = 'red'))

# plot candlestick chart
p <- df %>%
    plot_ly(x = ~Date, type="candlestick",
            open = ~AAPL.Open, close = ~AAPL.Close,
            high = ~AAPL.High, low = ~AAPL.Low, name = "AAPL",
            increasing = i, decreasing = d) %>%
    add_lines(y = ~up , name = "B Bands",
              line = list(color = '#ccc', width = 0.5),
              legendgroup = "Bollinger Bands",
              hoverinfo = "none") %>%
    add_lines(y = ~dn, name = "B Bands",
              line = list(color = '#ccc', width = 0.5),
              legendgroup = "Bollinger Bands",
              showlegend = FALSE, hoverinfo = "none") %>%
    add_lines(y = ~mavg, name = "Mv Avg",
              line = list(color = '#E377C2', width = 0.5),
              hoverinfo = "none") %>%
    layout(yaxis = list(title = "Price"))

# plot volume bar chart
pp <- df %>%
    plot_ly(x=~Date, y=~AAPL.Volume, type='bar', name = "AAPL Volume",
            color = ~direction, colors = c('forestgreen','red')) %>%
    layout(yaxis = list(title = "Volume"))

# create rangeselector buttons
rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'center', yref = 'paper',
           font = list(size = 9),
           buttons = list(
               list(count=1,
                    label='RESET',
                    step='all'),
               list(count=1,
                    label='1 YR',
                    step='year',
                    stepmode='backward'),
               list(count=3,
                    label='3 MO',
                    step='month',
                    stepmode='backward'),
               list(count=1,
                    label='1 MO',
                    step='month',
                    stepmode='backward')
           ))

# subplot with shared x axis
p <- subplot(p, pp, heights = c(0.7,0.2), nrows=2,
             shareX = TRUE, titleY = TRUE) %>%
    layout(title = paste("Apple: 2017-01-01-",Sys.Date()),
           xaxis = list(rangeselector = rs),
           legend = list(orientation = 'h', x = 0.5, y = 1,
                         xanchor = 'center', yref = 'paper',
                         font = list(size = 10),
                         bgcolor = 'transparent'))
p

# my turn ----

getSymbols("AAPL",src = "yahoo")
df <- data.frame(Date = index(AAPL), coredata(AAPL))
df <- subset(df, Date >= "2017-01-01")


# mark iphone x release date
a <- list(text = "Oct. 27\niPhoneX\nReleased",
          x = '2017-10-27',
          y = 1.05,
          xref = 'x',
          yref = 'paper',
          xanchor = 'left',
          showarrow = FALSE
)

# use shapes to create a line
l <- list(type = line,
          x0 = '2017-10-27',
          x1 = '2017-10-27',
          y0 = 0.5,
          y1 = 1,
          xref = 'x',
          yref = 'paper',
          line = list(color = 'black',
                      width = 0.5)
)

p <- plot_ly(df, x = ~Date, type="candlestick", open = ~AAPL.Open, 
        close = ~AAPL.Close, high = ~AAPL.High, low = ~AAPL.Low,
        color = ~AAPL.Close > AAPL.Open,
        colors = c("forestgreen", "red")) %>%
    layout(showlegend = FALSE, annotations = a, shapes = l, 
           yaxis = list(title = "Price"),
           axis = list(rangeslider = list(visible = FALSE)))
pp <- df %>%
    plot_ly(x=~Date, y=~AAPL.Volume, type='bar', name = "AAPL Volume",
            color = ~AAPL.Close > AAPL.Open, colors = c('forestgreen','red')) %>%
    layout(yaxis = list(title = "Volume"))
all <- subplot(p, pp, heights = c(0.7,0.2), nrows=2,
             shareX = TRUE, titleY = TRUE) %>% 
    layout(title = paste("Apple: 2017-01-01 -",Sys.Date()),
           showlegend = FALSE, xaxis = list(rangeslider = list(visible = F)))
all

# latest solution using plotly candlestick guide
rm(list = ls())
library(quantmod)
library(plotly)

stock <- getSymbols("AAPL", auto.assign = F)
dts <- index(stock)
df <- data.frame(stock, row.names = NULL)
df$dates <- dts
names(df) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted", "dates")

# Subset to after Jan 2017
df <- subset(df, dates > "2017-01-01")

# colors column for increasing and decreasing
for (i in 1:nrow(df)) {
    if (df$Close[i] >= df$Open[i]) {
        df$direction[i] = 'Increasing'
    } else {
        df$direction[i] = 'Decreasing'
    }
}

# Add annotation
a <- list(text = ~paste("Oct. 27", "<br>iPhone X", "<br>available"),
          x = '2017-10-27',
          y = df[which(df$dates == '2017-10-27'), 2],
          xref = 'x',
          yref = 'y',
          xanchor = 'center',
          yanchor = 'bottom',
          showarrow = F)

# # Range selector
# rangeselectorlist = list(
#     x = 0, y = 0.9,
#     bgcolor = "#0099cc",
#     font = list(color = "white"),
#     
#     buttons = list(
#         list(count = 1, label = "reset", step = "all"),
#         list(count = 6, label = "6 mo", step = "month", stepmode = "backward"),
#         list(count = 3, label = "3 mo", step = "month", stepmode = "backward"),
#         list(count = 1, label = "1 mo", step = "month", stepmode = "backward")
#     )
# )

p <- df %>% 
    plot_ly(type = "candlestick",
            x = ~dates,
            open = ~Open,
            high = ~High,
            low = ~Low,
            increasing = list(line = list(color = "#FF0000")),
            decreasing = list(line = list(color = "#008000")),
            close = ~Close # , text = ~paste(dates, "Open: ", Open, '<br>Close:', Close, '<br>High:', High, '<br>Low:', Low, , '<br>Volume:', Volumn)
    ) %>%
    layout(yaxis = list(title = "Price"))

pp <- df %>%
    plot_ly(x = ~dates, y = ~Volume, type = 'bar', name = "Volume",
            color = ~direction, colors = c('#008000', '#FF0000')) %>%
    layout(yaxis = list(title = "Volume"))

subplot(p, pp, heights = c(0.7, 0.2), nrows = 2, shareX = T, titleY = T) %>%
    layout(title = "2017 Apple Stock Price",
           xaxis = list(title = "", domain = c(0, 0.95),
                        rangeslider = list(visible = F)),
           showlegend = F,
           annotations = a
    )

# latest ----
rm(list = ls())
library(quantmod)
library(plotly)

df <- getSymbols(Symbols = "AAPL", src = "yahoo")
df <- data.frame(Date = index(AAPL), coredata(AAPL))
df <- subset(df, Date >= "2017-01-01")
names(df) <- sub("^AAPL\\.", "", names(df))

# labels for increasing/decreasing
for (i in 1:nrow(df)) {
    if (df$Close[i] >= df$Open[i]) {
        df$direction[i] = "Increasing"
    } else {
        df$direction[i] = "Decreasing"
    }
}

# mark iphone x release date
a <- list(text = "Oct. 27\niPhone X\nReleased",
          x = '2017-10-27',
          y = 1.05,
          xref = "x",
          yref = "paper",
          xanchor = "left",
          showarrow = FALSE)

# use shapes to create a line
l <- list(type = line,
          x0 = "2017-10-27",
          x1 = "2017-10-27",
          y0 = 0.5,
          y1 = 1,
          xref = "x",
          yref = "paper",
          line = list(color = "black", width = 0.5))

# top plot
p <- plot_ly(df,
             type = "candlestick",
             x = ~Date,
             open = ~Open,
             close = ~Close,
             high = ~High,
             low = ~Low,
             increasing = list(line = list(color = "red")),
             decreasing = list(line = list(color = "forestgreen"))) %>% 
    layout(showlegend = FALSE,
           annotations = a,
           shapes = l,
           axis = list(rangeslider = list(visible = FALSE)),
           yaxis = list(title = "Price"))

# bottom plot
pp <- plot_ly(df,
              x = ~Date,
              y = ~Volume,
              type = "bar",
              name = "Volume",
              color = ~direction,
              colors = c("forestgreen", "red")) %>% 
    layout(yaxis = list(title = "Volume"))

# combine into subplot
subplot(p,
        pp,
        heights = c(0.7, 0.2),
        nrows = 2,
        shareX = TRUE,
        titleY = TRUE) %>% 
    layout(title = paste0("Apple: 2017-01-01 - ", Sys.Date()),
           showlegend = FALSE,
           xaxis = list(rangeslider = list(visible = FALSE)),
           showlegend = FALSE)
