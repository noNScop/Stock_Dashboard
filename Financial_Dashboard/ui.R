navbarPage(
  
  "My Dashboard",
  theme = bs_theme(bootswatch = "minty"),
  
  tags$style(HTML("
    body, .container-fluid, .navbar, .well {
      background-color: #e0e0e0 !important;
    }
  ")),
 
  tabPanel("Dashboard",
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
      background: #aaa;
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
      color: #00ff00;
    }

    .red {
      color: #ff4c4c;
    }
  "))
           ),
           
           
           div(id = "ticker", div(class = "scrolling", htmlOutput("ticker_text"))),
           
    card(
      reactableOutput("table"),
      
    ),
  
  
  
    sidebarLayout(
      position = "right",
      sidebarPanel(
        width = 5,
        # ✅ Status Message Card
        card(
          h5("Status:"),
          verbatimTextOutput("status_text")
        ),
        
        card(
          h4(textOutput("text5")),
          uiOutput("Plot5"),
         #textInput("text5", label = NULL, value = "AAPL")
          ),
        card(
          plotlyOutput("candlePlot3"),
          
          pickerInput(
            inputId = "plot3_choice_range", 
            label = "Select range:", 
            choices = c("1M", "6M", "1Y", "3Y", "10Y"), 
            selected = "1M",
            options = pickerOptions(
              #actionsBox = TRUE, 
              size = 20,
              selectedTextFormat = "count > 3"
            ), 
            multiple = FALSE
          ),
          pickerInput(
            inputId = "plot3_type", 
            label = "Select type:", 
            choices = c("candle plot","line plot"), 
            selected = "candle plot",
            options = pickerOptions(
              #actionsBox = TRUE, 
              size = 20,
              selectedTextFormat = "count > 3"
            ), 
            multiple = FALSE
          )
        ),
        card(
          p("should you invest(moving averages)"),
          div(style = "overflow: hidden; height: 220px",
              div(style = "transform: scale(2); transform-origin: top ;",
                  gaugeOutput("investmentGauge")
              )
          ),
          sliderInput("Gauge_swich",
                      "Choose range of moving averages",
                      min = 10,
                      max = 150,
                      value = c(50, 150))
        )
      ),
      mainPanel(
        width = 7,
        
        card(
            h3("top volume trade companies"),
            pickerInput(
              inputId = "treemap2", 
              label = "Select:", 
              choices = unique(sp500$Sector), 
              selected = unique(sp500$Sector), 
              options = pickerOptions(
                actionsBox = TRUE, 
                size = 20,
                selectedTextFormat = "count > 3"
              ), 
              multiple = TRUE
            ),
            highchartOutput("treemap")
        ),

        card(
          #plotOutput("treemap")
          
          ),
        card(
          textOutput("text55")
        )
      )
    )
  ),
  
  
  
  
  
  
  
  
  tabPanel("About",
    h2("About This App"),
    p("This dashboard helps users explore financial data in easy and interactive way with many plots"),
    p("Created by: Oliwier Necelman and Karol Sroka"),
    p("Dataset sources:
      Yahoo Finance-
      S&P 500 companies- "),
    p("GitHub repo: https://github.com/noNScop/Stock_Dashboard/"),
    p("Live demo: https://yourshinyapp.io"),
    p("https://www.flaticon.com/free-icons/finance  - Finance icons created by Smashicons ")
  ),
  
  
  tabPanel("Help",
    h2("How to Use the App"),
    tags$ul(
      tags$li("Use the slider to adjust the number of bins in the histogram."),
      tags$li("Interact with visualizations using mouse hover or selection."),
      tags$li("Filter data using dropdowns, checkboxes, or other controls."),
      tags$li("Select a row in a table to update other visualizations.")
    )
  )
)



