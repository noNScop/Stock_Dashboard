library(httr)
library(jsonlite)

# Trading Economics docs
# https://docs.tradingeconomics.com/economic_calendar/snapshot/

key <- "eab4ead4aac4450:zuv03qaczfzj2xe"

start_date <- "2025-05-01"
end_date <- "2025-05-09"

url <- paste0("https://api.tradingeconomics.com/calendar/country/All/", start_date, 
              "/", end_date, "?c=", key)

response <- GET(url)

# The API return 10 first events, so in this example there are more events up to 2025-05-06,
# but they are not included so more API calls are needed.
# It seems that free API doesn't support future events beyond the current day
events <- fromJSON(content(response, as = "text"))

# I think these are the interesting columns that should appear in the dashboard table,
# There is also a column with source URLs, so some interactivity could be added 
# (but we probably don't need it)
a <- events[, c("Date", "Country", "Event", "Actual", "Previous", "TEForecast")]

# Free accounts have access to the following countries: Mexico, New Zealand, Sweden, Thailand.
# For more, contact us at support@tradingeconomics.com.
unique(events$Country)

# Time is in UTC time zone
a$Date
