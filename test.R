# Install if needed
#install.packages("quantmod")

library(quantmod)

# Get historical data for Apple (AAPL)

url <- "https://raw.githubusercontent.com/datasets/s-and-p-500-companies/master/data/constituents.csv"
sp500_info <- read.csv(url)
sp500_info <- data.frame(
  Symbol = sp500_info$Symbol,
  Name = sp500_info$Security,
  Sector = sp500_info$GICS.Sector
)
getSymbols(sp500_info$Symbol, src = "yahoo", from = "2025-05-15", to = "2025-05-16", auto.assign = TRUE, return.class = "data.frame") # Sys.Date()

# Basic line chart of closing prices
chartSeries(AAPL, 
            theme = chartTheme("black"), 
            TA = NULL, 
            name = "Apple Inc. (AAPL)")

# Add moving averages (SMA)
addSMA(n = 12, col = "blue")
addSMA(n = 50, col = "red")

## Plotly ----------------------

library(quantmod)
library(plotly)
library(dplyr)

# Load data
df2 <- getSymbols("AAPL", src = "yahoo", from = "2025-03-01", to = Sys.Date(), auto.assign = FALSE)

# Convert to data frame
df2 <- data.frame(Date = index(df2), coredata(df2))
colnames(df2) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

# Plot with plotly
p <- plot_ly(data = df2, x = ~Date, type = "candlestick",
             open = ~Open, high = ~High, low = ~Low, close = ~Close) %>%
  layout(title = "Apple Inc. (AAPL) Candlestick Chart",
         xaxis = list(rangeslider = list(visible = FALSE)),
         yaxis = list(title = "Price (USD)"))

p





#get stock from day
library(quantmod)
library(dplyr)
library(stringr)

library(quantmod)
library(dplyr)
library(stringr)

get_data_1day <- function(symbols, date, source = "yahoo") {
  results <- list()
  
  for (symbol in symbols) {
    # Safely attempt to get the data
    data <- tryCatch({
      getSymbols(symbol, src = source, from = date - 4, to = date + 1, auto.assign = FALSE)
    }, error = function(e) {
      message(paste("Error fetching:", symbol, "-", e$message))
      return(NULL)
    })
    
    # Skip if data could not be fetched
    if (is.null(data)) next
    
    valid_dates <- index(data)
    latest_date <- max(valid_dates[valid_dates <= date])
    row_data <- data[latest_date]
    df_row <- data.frame(date = latest_date, Symbol = symbol, coredata(row_data))
    
    # Clean column names
    colnames(df_row) <- c("date", "Symbol", str_replace(colnames(df_row)[-(1:2)], paste0("^", symbol, "\\."), ""))
    
    results[[length(results) + 1]] <- df_row
  }
  
  return(bind_rows(results))
}



symbols <- c("AAPL", "MSFT", "GOOGL")
query_date <- as.Date("2025-03-05")

df <- get_data_1day(symbols, query_date)
print(df)






#many dates
library(quantmod)
library(dplyr)
library(stringr)
library(lubridate)

get_different_dates <- function(symbol = "AAPL", reference_date = Sys.Date(), source = "yahoo") {
  # Define the list of target dates
  target_dates <- c(
    `today` = reference_date,
    `1_week_ago` = reference_date - weeks(1),
    `1_month_ago` = reference_date %m-% months(1),
    `3_months_ago` = reference_date %m-% months(3),
    `6_months_ago` = reference_date %m-% months(6),
    `start_of_year` = ymd(paste0(year(reference_date), "-01-01")),
    `1_year_ago` = reference_date %m-% years(1)
    
  )
  results <- list()
  for (label in names(target_dates)) {
    print(label)
    date <- target_dates[[label]]
    # Query a 2-day window around the date
    data <- tryCatch({
      getSymbols(symbol, src = source, from = date - days(2), to = date + days(2), auto.assign = FALSE)
    }, error = function(e) return(NULL))
    if (is.null(data)) next
    valid_dates <- index(data)
    # Find the latest available date <= target
    closest_date <- max(valid_dates[valid_dates <= date])
    row_data <- data[closest_date]
    df_row <- data.frame(date = closest_date, reference = label, symbol = symbol, coredata(row_data))
    colnames(df_row) <- c("date", "reference", "symbol", str_replace(colnames(df_row)[-(1:3)], paste0("^", symbol, "\\."), ""))
    results[[length(results) + 1]] <- df_row
  }
  df <- bind_rows(results)
  differences <- c()
  for (i in 2:length(target_dates)) {
    num <- (df$Adjusted[1] - df$Adjusted[i]) / df$Adjusted[i] * 100
    differences <- c(differences, paste(as.character(round(num, 2)), "%"))
  }
  return(differences)
}

aa <- get_different_dates("MMM")
aa
  

df_merged <- merge(df1, df2, by = "ID", all.x = TRUE)

difs <- get_different_dates("GOOGL")
print(difs)


# tu można przekonwertować symbol do nazwy
library(tidyquant)
sp500 <- tq_index("DOW")
head(sp500)
tq_index_options("DOW")



get_companies_sp500_info <- function(){
  url <- "https://raw.githubusercontent.com/datasets/s-and-p-500-companies/master/data/constituents.csv"
  sp500_info <- read.csv(url)
  sp500_info <- data.frame(
    Symbol = sp500_info$Symbol,
    Name = sp500_info$Security,
    Sector = sp500_info$GICS.Sector
  )
  #sp500_info = sp500_info[1:1,]
  
  x1= get_data_1day(sp500_info$Symbol, Sys.Date(), source = "yahoo") %>%
    left_join(sp500_info, by = "Symbol") %>%
    select(date, Symbol, Name, Sector, everything())
  x2 = get_data_1day(sp500_info$Symbol, Sys.Date() - weeks(1), source = "yahoo")
  
  x = x1 %>%
    left_join(x2, by = c("Symbol")) %>%
    mutate(
      diff = Adjusted.x - Adjusted.y,
      diff_perc = (diff / Adjusted.y) * 100
    )# %>%
    #select(date, Symbol, Name, Sector, everything())
  x = data.frame(
    Symbol = x$Symbol,
    Name = x$Name,
    Sector = x$Sector,
    diff = x$diff,
    volume = x$Volume.x,
    diff_perc = round(x$diff_perc, 2))
  return(x)
}

x = get_companies_sp500_info()
x

library(ggplot2)
library(treemapify)

# Example: df with columns: Symbol, Name, Sector, volume, diff_perc

ggplot(x, aes(
  area = volume,              # Size of each rectangle
  fill = diff_perc,           # Color by percent change
  label = Symbol,             # Main label
  subgroup = Sector           # Grouping by sector
)) +
  geom_treemap() +
  geom_treemap_text(
    colour = "white",
    place = "centre",
    grow = TRUE
  ) +
  geom_treemap_subgroup_border(color = "white") +
  geom_treemap_subgroup_text(
    place = "centre",
    colour = "white",
    alpha = 0.7,
    grow = TRUE,
    fontface = "italic"
  ) +
  scale_fill_gradient2(
    low = "red", mid = "black", high = "green",
    midpoint = 0,
    name = "% Change"
  ) +
  theme(legend.position = "bottom")


url <- "https://raw.githubusercontent.com/datasets/s-and-p-500-companies/master/data/constituents.csv"
sp500_info <- read.csv(url)





#gt library

library(gt)
library(dplyr)

# Sample data
df <- tibble::tibble(
  Sector = c("Health Care", "Health Care", "Technology", "Technology"),
  Company = c("Pfizer", "Moderna", "Apple", "Microsoft"),
  Price = c(36.2, 140.5, 189.3, 312.1),
  Change = c(0.5, -2.1, 1.2, -0.8),
  Volume = c(15000000, 12000000, 25000000, 20000000)
)

# Create the GT table with sector as row group
df %>%
  gt(groupname_col = "Sector") %>%
  tab_header(title = "Companies by Sector") %>%
  fmt_number(columns = c(Price, Change), decimals = 2) %>%
  fmt_number(columns = Volume, decimals = 0) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )










library(quantmod)
library(dplyr)
library(purrr)

get_data_1week_new <- function(symbols, date = Sys.Date(), source = "yahoo") {
  results <- list()
  
  for (symbol in symbols) {
    df <- tryCatch({
      getSymbols(symbol, src = source, from = date - 7, to = date + 1, auto.assign = FALSE)
    }, error = function(e) {
      message(paste("Error fetching:", symbol, "-", e$message))
      return(NULL)
    })
    
    if (is.null(df) || nrow(df) == 0) next
    
    # Clean column names
    colnames(df) <- gsub(paste0("^", symbol, "\\."), "", colnames(df))
    
    # Check if needed columns are present
    required_cols <- c("Open", "Close", "Adjusted", "Volume")
    if (!all(required_cols %in% colnames(df))) next
    
    # Use last row only if it exists
    last_row <- tail(df, 1)
    if (nrow(last_row) == 0) next
    
    results[[length(results) + 1]] <- data.frame(
      Symbol = symbol,
      Open = as.numeric(last_row$Open),
      Close = as.numeric(last_row$Close),
      Adjusted_week = I(list(as.numeric(df$Adjusted))),
      Volume_avg = mean(df$Volume, na.rm = TRUE)
    )
  }
  
  return(bind_rows(results))
}




get_companies_sp500_info <- function(){
  url <- "https://raw.githubusercontent.com/datasets/s-and-p-500-companies/master/data/constituents.csv"
  sp500_info <- read.csv(url)
  sp500_info <- data.frame(
    Symbol = sp500_info$Symbol,
    Name = sp500_info$Security,
    Sector = sp500_info$GICS.Sector
  )
  #sp500_info = sp500_info[1:10,]
  
  df= get_data_1week_new(sp500_info$Symbol, Sys.Date(), source = "yahoo") %>%
    left_join(sp500_info, by = "Symbol") %>%
    mutate(
      diff = Close - Open,
      diff_perc = (diff / Open) * 100
    )
  
  return(df)
}
sp500 = get_companies_sp500_info()
save(sp500, file = "C:/Users/onece/OneDrive/Pulpit/sem4/data_vis/lab/assignment4/Financial_Dashboard/sp500.RData")
#sp500$Adjusted_week[2]
