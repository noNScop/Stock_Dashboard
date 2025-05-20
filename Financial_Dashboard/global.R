library(shiny)
library(ggplot2)
library(stringr)
library(shinyWidgets)
library(plotly)
library(bslib)
library(treemapify)
library(lubridate)
library(dplyr)
library(quantmod)
library(purrr)
library(reactable)
library(sparkline)
library(googledrive)
library(future)
library(promises)
library(tidyr)
library(flexdashboard)

library(highcharter)
library(DT)






get_data_1day <- function(symbols, date, source = "yahoo", max_lookback = 5) {
  results <- list()
  
  for (symbol in symbols) {
    found <- FALSE
    for (i in 0:max_lookback) {
      try_date <- date - i
      
      data <- tryCatch({
        getSymbols(symbol, src = source, from = try_date, to = try_date + 1, auto.assign = FALSE)
      }, error = function(e) {
        message(paste("Error fetching:", symbol, "-", e$message))
        return(NULL)
      })
      
      if (!is.null(data) && nrow(data) > 0) {
        row_data <- data[1, ]
        df_row <- data.frame(date = try_date, Symbol = symbol, coredata(row_data))
        
        # Clean column names
        colnames(df_row) <- c("date", "symbol", "Open", "High", "Low", "Close", "Volume", "Adjusted")        
        results[[length(results) + 1]] <- df_row
        found <- TRUE
        break
      }
    }
    
    if (!found) {
      message(paste("No data found for", symbol, "within", max_lookback, "days before", date))
    }
  }
  print("get_data_1day obtained")
  return(bind_rows(results))
}





# returns data for company for many dates
get_different_dates <- function(symbol = "AAPL", reference_date = Sys.Date(), source = "yahoo") {
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
    date <- target_dates[[label]]
    # Query a 2-day window around the date
    data <- tryCatch({
      getSymbols(symbol, src = source, from = date - days(6), to = date + days(1), auto.assign = FALSE)
    }, error = function(e) return(NULL))
    if (is.null(data)) next
    valid_dates <- index(data)
    # Find the latest available date <= target
    closest_date <- max(valid_dates[valid_dates <= date])
    row_data <- data[closest_date]
    df_row <- data.frame(date = closest_date, reference = label, symbol = symbol, coredata(row_data))
    colnames(df_row) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    results[[length(results) + 1]] <- df_row
  }
  df <- bind_rows(results)
  differences <- c()
  for (i in 2:length(target_dates)) {
    num <- (df$Adjusted[1] - df$Adjusted[i]) / df$Adjusted[i] * 100
    differences <- c(differences, paste(as.character(round(num, 2)), "%"))
  }
  print("data different dates obtained - this should not happen as it is outdated function")
  return(differences)
}



# loading sp500 data (tymczasowe rozwiązanie)#TODO karol

# Authenticate first if needed
drive_auth(path="GC_API_key.json")

# Get metadata (optional step)
file <- drive_get("financial_dashboard_cache/sp500.RData")

# Download the file
drive_download(file, path = "sp500.RData", overwrite = TRUE)

# Load it
load("sp500.RData")

sp500 <- sp500 %>%
  mutate(across(c(Open, Close, diff, diff_perc), round, 2))
df3 <- getSymbols("AAPL", src = "yahoo", from = "2025-03-01", to = Sys.Date(), auto.assign = FALSE)
print("df3 data obtaied")
cur_symbols <-  c("PLNEUR=X", "PLNUSD=X", "PLNGBP=X", "PLNCHF=X", "PLNCZK=X", "PLNHUF=X", "PLNJPY=X", "PLNSEK=X")

#my_data_test <- get_data_1day(cur_symbols, Sys.Date())






