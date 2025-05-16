





# Define server logic required to draw a histogram
function(input, output, session) {

    
    
  ## text and plot for 6 rectangles with stats for comapny
    output$text5 <- renderText({
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
    
    

}
