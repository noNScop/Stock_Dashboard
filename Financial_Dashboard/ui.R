navbarPage(
  "Financial Dashboard",
  theme = bs_theme(bootswatch = "minty"),
  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
  
  tags$style(HTML("
    body, .container-fluid, .navbar, .well {
      background-color: #e0e0e0 !important;
    }
  ")),
  
  # ---------------- Stock Prices Tab ---------------- #
  tabPanel("Stock Prices",
           
           div(
             style = "display: flex; align-items: center;",
             
             h1("Stock Prices", style = "margin-right: 15px;"),
             
             tags$img(
               src = "financial-profit.png",
               height = "35px"
             )
           ),
           card(
             reactableOutput("table"),
             
           ),  # ✅ Table unchanged and on top
           
           sidebarLayout(
             position = "right",
             sidebarPanel(
               width = 5,
               
               # ✅ All six panels and gauge inside sidebar
               card(h4(textOutput("text5")), uiOutput("Plot5")),
               card(
                 style = "height: 460px;",
                 h4(textOutput("textGauge")),
                 div(style = "overflow: hidden; height: 220px",
                     div(style = "transform: scale(2); transform-origin: top ;",
                         gaugeOutput("investmentGauge")
                     )
                 ),
                 sliderInput("Gauge_swich", "Choose range of moving averages", min = 10, max = 150, value = c(50, 150))
               ),
               #card(h5("Status:"), verbatimTextOutput("status_text")),
               #card(h3("Economic News Headlines"), DTOutput("news_table", width = "100%"))
             ),
             mainPanel(
               width = 7,
               card(
                 style = "height: 825px;",
                 h3(textOutput("title_candle")),
                 materialSwitch(inputId = "show_averages", label = "show averages", value = TRUE),
                 plotlyOutput("candlePlot3"),  # ✅ Candle plot in main panel
                 
                 pickerInput(
                   inputId = "plot3_choice_range", 
                   label = "Select range:", 
                   choices = c("1M", "6M", "1Y", "3Y", "10Y"), 
                   selected = "1M",
                   options = pickerOptions(size = 20, selectedTextFormat = "count > 3"), 
                   multiple = FALSE
                 ),
                 pickerInput(
                   inputId = "plot3_type", 
                   label = "Select type:", 
                   choices = c("candle plot", "line plot"), 
                   selected = "candle plot",
                   options = pickerOptions(size = 20, selectedTextFormat = "count > 3"), 
                   multiple = FALSE
                 )
               ),
             )
           )
  ),
  
  # ---------------- Market Overview Tab ---------------- #
  tabPanel("Market Overview",
           # 
           
           div(
             style = "display: flex; align-items: center;",
             
             h1("Market Overview", style = "margin-right: 15px;"),
             
             tags$img(
               src = "financial-profit.png",
               height = "35px"
             )
           ),
           
           tags$head(
             tags$style(HTML("
        #ticker {
          white-space: nowrap;
          overflow: hidden;
          box-sizing: border-box;
          background: #888;
          color: #fff;
          padding: 10px;
          font-family: monospace;
          font-weight: bold;
          position: relative;
        }

        #ticker .scrolling {
          display: inline-block;
          white-space: nowrap;
          padding-left: 1%;
          animation: ticker 30s linear infinite;
        }

        @keyframes ticker {
          0% { transform: translateX(0); }
          100% { transform: translateX(-50%); }
        }

        .ticker-item {
          margin-right: 30px;
        }

        .green {
          color: #184912;
        }

        .red {
          color: #8e3236;
        }
      "))
           ),
           div(id = "ticker", div(class = "scrolling", htmlOutput("ticker_text"))),
           
           sidebarLayout(
             position = "right",
             sidebarPanel(
               width = 5,
               
               card(
                 style = "height: 670px; overflow-y: hidden; overflow-x: hidden;",
                 h3("Economic News Headlines"),
                 div(
                   style = "height: 530px; overflow-y: hidden; overflow-x: hidden;",  
                   DTOutput("news_table")
                 )
               )
             ),
             mainPanel(
               width = 7,
               
               card(
                 style = "height: 685px;",
                 h3("Top Volume Trade Companies"),
                 highchartOutput("treemap", height = "600px")
               )
             )
           )
           
  ),
  
  # ---------------- About Tab ---------------- #
  
  tabPanel("About",
           h3("Dashboard Overview"),
           p("This dashboard provides a comprehensive overview of key financial insights for companies in the S&P 500 index."),
           p("Explore the following features:"),
           tags$ul(
             tags$li(
               strong("📊 Price Charts"), tags$br(),
               "Candlestick charts show open, high, low, and close prices — ideal for technical analysis.", tags$br(),
               "Line charts display adjusted closing prices for smoother trends."
             ),
             
             tags$li(
               strong("📦 Stat Boxes"), tags$br(),
               "Six colored panels summarize recent performance over various time frames, helping assess momentum."
             ),
             
             tags$li(
               strong("📈 Investment Gauge"), tags$br(),
               "Shows the difference between two Simple Moving Averages. Positive (green) suggests upward momentum; negative (red) signals a downtrend."
             ),
             
             tags$li(
               strong("🌳 Treemap"), tags$br(),
               "Displays top-traded companies by volume, organized by sector. Color indicates recent price change."
             ),
             
             tags$li(
               strong("📰 News"), tags$br(),
               "Relevant economic headlines provide context for market movements."
             ),
             
             tags$li(
               strong("🧾 Ticker"), tags$br(),
               "A live scrolling bar shows real-time adjusted prices and percentage changes for selected tickers. Green = gains, red = losses."
             )
           ),
           h3("Info"),
           
           p("Created by: Oliwier Necelman and Karol Sroka"),
           p("Data: Yahoo Finance — S&P 500 companies"),
           p("GitHub: ", a("https://github.com/noNScop/Stock_Dashboard/", href = "https://github.com/noNScop/Stock_Dashboard/")),
           p("Icons: ", a("Finance icons by Smashicons", href = "https://www.flaticon.com/free-icons/finance")),
           
           h3("Watch a Quick Overview"),
           tags$iframe(
             width = "560", height = "315",
             src = "https://www.youtube.com/embed/z-INcQkiKwo",
             title = "YouTube video player",
             frameborder = "0",
             allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
             allowfullscreen = NA
           )
  ),
  
  
  
  
  # ---------------- Help Tab ---------------- #
  tabPanel("Help",
  h3("How to interact with key features:"),
  
  tags$ul(
    tags$li(
      strong("📊 Stock Chart (Main Panel)"), tags$br(),
      "Time Range Selector adjusts the visible date window for the stock's performance (e.g., 1M, 6M, etc.).", tags$br(),
      "Chart Type lets you switch between candlestick and line views.", tags$br(),
      "\"Show averages\" Toggle enables overlay of short- and long-term moving averages, based on the slider below.", tags$br(),
      "Slider Input allows you to define the SMA periods used for the investment gauge and chart overlays."
    ),
    
    tags$li(
      strong("📦 Six Stat Boxes (Sidebar)"), tags$br(),
      "Show percentage changes over common time intervals (1W, 1M, 3M, etc.) and update when you select a different stock from the table."
    ),
    
    tags$li(
      strong("📈 Investment Gauge"), tags$br(),
      "Automatically reflects the current difference between two SMAs. The gauge updates dynamically based on the selected stock and SMA range."
    ),
    h4("Moving Averages & Investing"),
      tags$li(
      "Moving averages smooth out price data to identify trends by averaging prices over a set period." , tags$br(),
       "A short-term moving average crossing above a long-term moving average can signal a buying opportunity." , tags$br(),
       "Conversely, a short-term average crossing below a long-term average may indicate a sell signal.")
    
  )
)

  
)
