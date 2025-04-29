# Install if needed
# install.packages("quantmod")

library(quantmod)

# Get historical data for Apple (AAPL)
getSymbols("AAPL", src = "yahoo", from = "2025-04-01", to = Sys.Date())

# Basic line chart of closing prices
chartSeries(AAPL, 
            theme = chartTheme("white"), 
            TA = NULL, 
            name = "Apple Inc. (AAPL)")

# Add moving averages (SMA)
addSMA(n = 20, col = "blue")
addSMA(n = 50, col = "red")

## Plotly ----------------------

library(quantmod)
library(plotly)
library(dplyr)

# Load data
getSymbols("AAPL", src = "yahoo", from = "2025-01-01", to = Sys.Date())

# Convert to data frame
df <- data.frame(Date = index(AAPL), coredata(AAPL))
colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

# Plot with plotly
p <- plot_ly(data = df, x = ~Date, type = "candlestick",
             open = ~Open, high = ~High, low = ~Low, close = ~Close) %>%
  layout(title = "Apple Inc. (AAPL) Candlestick Chart",
         xaxis = list(rangeslider = list(visible = FALSE)),
         yaxis = list(title = "Price (USD)"))

p