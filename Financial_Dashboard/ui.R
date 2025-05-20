navbarPage(
  "My Dashboard",
  theme = bs_theme(bootswatch = "minty"),
  
  tags$style(HTML("
    body, .container-fluid, .navbar, .well {
      background-color: #e0e0e0 !important;
    }
  ")),
  
  # ---------------- Dashboard Tab ---------------- #
  tabPanel("Dashboard",
           div(
             style = "display: flex; align-items: center;",
             
             h1("Financial Data", style = "margin-right: 15px;"),
             
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
                 style = "height: 430px;",
                 h4("Should you invest?"),
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
                 style = "height: 750px;",
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
             
             h1("Financial Data", style = "margin-right: 15px;"),
             
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
          color: #207d20;
        }

        .red {
          color: #E04040;
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
                   style = "height: 570px; overflow-y: hidden; overflow-x: hidden;",  
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
           h2("About This App"),
           p("This dashboard visualizes key financial insights for companies in the S&P 500 index."),
           tags$ul(
             tags$li("📊 Candlestick and line charts show price trends."),
             tags$li("📦 Stat boxes summarize recent performance over multiple timeframes."),
             tags$li("📈 An investment gauge tracks SMA crossovers."),
             tags$li("🌳 A treemap displays companies by sector and volume."),
             tags$li("📰 News headlines provide market context."),
             tags$li("🧾 A live ticker shows real-time price changes.")
           ),
           p("Created by: Oliwier Necelman and Karol Sroka"),
           p("Data: Yahoo Finance — S&P 500 companies"),
           p("GitHub: ", a("https://github.com/noNScop/Stock_Dashboard/", href = "https://github.com/noNScop/Stock_Dashboard/")),
           p("Icons: ", a("Finance icons by Smashicons", href = "https://www.flaticon.com/free-icons/finance")),
           
           h3("Watch a Quick Overview"),
           tags$iframe(
             width = "560", height = "315",
             src = "https://www.youtube.com/watch?v=OPlslc1ZG0Q",
             title = "YouTube video player",
             frameborder = "0",
             allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
             allowfullscreen = NA
           )
           
  ),
  
  
  # ---------------- Help Tab ---------------- #
  tabPanel("Help",
           h2("How to Use the App"),
           tags$ul(
             tags$li("📊 Use the chart controls to select time range and type (candlestick or line)."),
             tags$li("📈 Enable 'Show averages' to overlay short- and long-term SMAs."),
             tags$li("🎚️ Use the SMA sliders to adjust periods for chart overlays and the investment gauge."),
             tags$li("📦 Click on a company in the table to update the charts and stats."),
             tags$li("🌳 Interact with the treemap to explore top-volume companies by sector."),
             tags$li("📰 Review the latest news headlines for context."),
             tags$li("🧾 Watch the live ticker for real-time price updates.")
           )
  )
  
)
