# DOWNLOAD DATA LOGIC

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

# ----------------------------------

# UPDATE CACHE LOGIC
needs_update <- function(last_modified, threshold_hours = 1) {
  difftime(Sys.time(), last_modified, units = "hours") > threshold_hours
}

update_cache <- ExtendedTask$new(function(status, sp500_data, sp500) {
  future_promise({
    drive_auth(path="GC_API_key.json")
    file_meta <- drive_get("financial_dashboard_cache/sp500.RData")
    last_modified <- as_datetime(file_meta$drive_resource[[1]]$modifiedTime)
    list(file_meta = file_meta, last_modified = last_modified)
    sp500 <- sp500
    if(needs_update(last_modified)) {
      sp500 <- get_companies_sp500_info()
      save(sp500, file = "sp500.RData")  # save first
      drive_update(file_meta, media = "sp500.RData")
    }
    sp500
  }) %...>% 
    (function(sp500) {
      sp500_data(sp500)     # assign reactiveVal
      status("Data ready!")
    }) %...!% 
    (function(e) {
      status(paste("Error:", e$message))
    })
})

future::plan(multisession)

# ----------------------------------

# VIZUALISATIONS


# Define server logic required to draw a histogram
function(input, output, session) {
  # sp500 downloaded and defined in global.R
  sp500_data <- reactiveVal(sp500)
  status <- reactiveVal("Waiting for update check...")
  output$status_text <- renderText({
    status()
  })
  update_cache$invoke(status, sp500_data, sp500)

  ## text and plot for 6 rectangles with stats for comapny
    output$text5 <- renderText({
      sp500 <- sp500_data()
      paste("Stats for ", sp500$Symbol[input$table__reactable__selected])
    })
    output$Plot5 <- renderUI({
      df <- data.frame(
        label = c("1W", "1M", "3M", "6M", "YTD", "1Y"),
        value = get_different_dates(sp500$Symbol[input$table__reactable__selected]),
        stringsAsFactors = FALSE
      )
      
      box_ui <- lapply(1:nrow(df), function(i) {
        value <- df$value[i]
        color <- if (grepl("-", value)) "#9B2020" else "#206d20"
        bg <- if (grepl("-", value)) "#431010" else "#102a10"
        
        div(
          style = paste0(
            "background-color:", bg, "; color:", color, 
            "; padding:10px; margin:4px; border-radius:8px; ",
            "text-align:center; font-weight:bold; min-height:90px;",
            "display:flex; flex-direction:column; justify-content:center;"
          ),
          div(style = "font-size: 18px; line-height: 1.2;", value),
          div(style = "font-size: 13px; margin-top: 4px;", df$label[i])
        )
      })
      
      tagList(
        div(style = "margin-bottom: -8px;",  # Pull rows closer together
            fluidRow(
              column(4, style = "padding:4px;", box_ui[[1]]),
              column(4, style = "padding:4px;", box_ui[[2]]),
              column(4, style = "padding:4px;", box_ui[[3]])
            ),
            fluidRow(
              column(4, style = "padding:4px;", box_ui[[4]]),
              column(4, style = "padding:4px;", box_ui[[5]]),
              column(4, style = "padding:4px;", box_ui[[6]])
            )
        )
      )
    })
    
    
    
    
    
    
    
    
    # creating treeplot of sp500 comapnies
    output$treemap <- renderPlot({
      library(ggplot2)
      library(treemapify)
      sp500 <- sp500_data()
      
      if (length(input$treemap2) == 0) {
        return(NULL)
      }
      
      df <- sp500[sp500$Sector %in% input$treemap2, ]
     
      
      df$label <- paste0(df$Symbol, "\n", sprintf("%.2f%%", df$diff_perc))
      
      ggplot(df, aes(
        area = Volume_avg,
        fill = diff_perc,
        subgroup = Sector
      )) +
        geom_treemap() +
        geom_treemap_text(
          aes(label = Symbol),
          colour = "black",
          place = "centre",
          grow = TRUE,
          reflow = FALSE,
          size = 12
        ) +
        #geom_treemap_text(
        #  aes(label = paste0("\n\n", sprintf("%.2f%%", diff_perc))),
        #  colour = "white",
        #  place = "centre",
        #  grow = TRUE,
        #  reflow = TRUE,
        #  size = 8,
        #  min.size = 3
        #) +
        geom_treemap_subgroup_border(color = "black") +
        geom_treemap_subgroup_text(
          place = "topright",
          colour = "white",
          #alpha = 0.6,
          grow = FALSE,
          fontface = "italic",
          size = 12
        ) +
        scale_fill_gradient2(
          low = "#E04040",mid = "#666", high = "#40F040",
          midpoint = 0,
          name = "% Change"
        ) + 
       theme(legend.position = "bottom")
      
    })
    
    
    
    
    
    
    
    #reactive value for changing ranges in candle plot
    df3_reactive <- reactiveVal(NULL)
    observeEvent({
      event_data("plotly_relayout", source = "candle")
      input$plot3_choice_range
      input$table__reactable__selected
    },{
      sp500 <- sp500_data()
      relayout <- event_data("plotly_relayout", source = "candle")
      end_date <- min(as.Date(relayout[["xaxis.range[1]"]]), Sys.Date())
      
      start_date <- switch(
        input$plot3_choice_range,
        "1M" = end_date %m-% months(1),
        "6M" = end_date %m-% months(6),
        "1Y" = end_date %m-% years(1),
        "3Y" = end_date %m-% years(3),
        "10Y" = end_date %m-% years(10),
      )
      print(start_date, end_date)
      
      #end_date <- as.Date(relayout[["xaxis.range[1]"]])
      df <- getSymbols(sp500$Symbol[input$table__reactable__selected], src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
      df3_reactive(df)
    })
    
    # candle plot with plotly
    output$candlePlot3 <- renderPlotly({
      sp500 <- sp500_data()
      
      # If nothing is selected, return nothing
      if (is.null(input$table__reactable__selected)) {
        return(NULL)
      }
      
      df3 <- tryCatch({
        req(df3_reactive())  
      }, error = function(e) {
        # If reactive fails, fall back to fetching new data
        tryCatch({
          getSymbols(sp500$Symbol[input$table__reactable__selected], src = "yahoo", from = Sys.Date() - 30, to = Sys.Date(), auto.assign = FALSE)
        }, error = function(e2) {
          showNotification("Failed to load stock data.", type = "error")
          return(NULL)
        })
      })
      
      df3_plot <- data.frame(Date = index(df3), coredata(df3))
      colnames(df3_plot) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
      
      if(input$plot3_type == "candle plot"){
        p <- plot_ly(
          data = df3_plot,
          x = ~Date,
          type = "candlestick",
          open = ~Open,
          high = ~High,
          low = ~Low,
          close = ~Close,
          source = "candle"
        ) %>%
          layout(
            dragmode = "pan",
            title = paste("Candlestick Chart:", sp500$Symbol[input$table__reactable__selected]),
            xaxis = list(rangeslider = list(visible = FALSE)),
            yaxis = list(title = "Price (USD)"#,
                         #range = c(0, max(df3_plot$High, na.rm = TRUE))
                         )
          )
      }else{
        p <- plot_ly(
          data = df3_plot,
          x = ~Date,
          y = ~Adjusted,
          type = 'scatter',
          mode = 'lines',
          line = list(color = "#55b"),
          name = "Adjusted Close",
          source = "candle"
        ) %>%
          layout(
            dragmode = "pan",
            title = paste("Line Chart:", sp500$Symbol[input$table__reactable__selected]),
            xaxis = list(rangeslider = list(visible = FALSE)),
            yaxis = list(title = "Adjusted Close (USD)")
          )
      }
      event_register(p, "plotly_relayout")
    })
    
    
    
    
    
   # table with sp500 comapnies 
    
    observe({
      updateReactable("table", selected = 1)
    })
    
    output$table <- renderReactable({
      
      sp500 <- sp500_data()
      sp500 <- sp500 %>%
        select(
          Sector, Symbol, Name, everything()
        )
      reactable(
        sp500,
        defaultSorted = list(Volume_avg = "desc"),
        defaultPageSize = 20,
        style = list(maxHeight = "400px", overflowY = "auto"),
        groupBy = "Sector",
        searchable = TRUE,
        highlight = TRUE,
        bordered = TRUE,
        onClick = "select",
        selection = "single",
        columns = list(
          diff = colDef(
            style = function(value) {
              color <- if (value > 0) "green" else "red"
              list(color = color, fontWeight = "bold")
            }
          ),
          diff_perc = colDef(
            name = "Change (%)",
            cell = function(value) {
              color <- if (value > 0) "green" else "red"
              htmltools::div(style = paste("color:", color, "; font-weight: bold"), paste0(round(value, 2), "%"))
            }
          ),
          
          Adjusted_week = colDef(
            name = "Price Trend",
            cell = function(values) {
              # Render inline sparkline
              sparkline(values, type = "line", width = 100, height = 30)
            },
            html = TRUE
          )
        )
      )
    })
    
    
    
    
    
    #moving bar 
    data <- reactiveVal(get_data_1day(cur_symbols, Sys.Date()))
    
    observe({
      invalidateLater(30000, session)
      updated <- get_data_1day(cur_symbols, Sys.Date())
      
      data(updated)
    })
    
    output$ticker_text <- renderUI({
      curr_df <- data()
      curr_df <- tidyr::separate(curr_df, symbol, into = c("symbol"), sep = "=", extra = "drop")
      curr_df$symbol <- paste0(substr(curr_df$symbol, 1, 3), "-", substr(curr_df$symbol, 4, nchar(curr_df$symbol)))
      
      curr_df$Adjusted <- as.numeric(curr_df$Adjusted)
      curr_df$diff_perc <- round((curr_df$Close - curr_df$Open) / curr_df$Open * 100, 2)
      
      # Generate HTML spans with appropriate color
      parts <- mapply(function(sym, adj, change) {
        color_class <- if (change >= 0) "green" else "red"
        sprintf('<span class="ticker-item %s">%s %.3f (%.2f%%)</span>', color_class, sym, adj, change)
      }, curr_df$symbol, curr_df$Adjusted, curr_df$diff_perc, SIMPLIFY = TRUE)
      
      # Duplicate ticker content to make it loop smoothly
      HTML(paste(c(parts, parts), collapse = " | "))
    })
    
    
    
    
    
    
    
    
    
}