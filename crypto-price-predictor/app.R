# Load required libraries
library(shiny)
library(bslib)
library(httr)
library(jsonlite)
library(prophet)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)

# UI
ui <- page_sidebar(
  title = "Crypto Price Predictor",
  theme = bs_theme(
    bg = "#1e1e2f",        # dark background
    fg = "white",          # light text
    primary = "#00bcd4",   # accent color
    base_font = font_google("Roboto")
  ),
  sidebar = sidebar(
    selectInput("coin", "Select a coin:", choices = c("bitcoin", "ethereum", "solana")),
    actionButton("go", "Predict Next 7 Days")
  ),
  navset_tab(
    tabPanel("📈 Forecast",
             h3("Next 7-Day Forecast"),
             plotlyOutput("forecastPlot"),
             br(),
             DTOutput("forecastTable")
    ),
    tabPanel("📚 History Visualization",
             h3("Historical Price Patterns (Last 9 Months)"),
             plotlyOutput("histogramPlot"),
             br(),
             plotlyOutput("heatmapPlot")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Get historical data
  get_full_history_data <- reactive({
    req(input$coin)
    url <- paste0("https://api.coingecko.com/api/v3/coins/", input$coin, "/market_chart?vs_currency=usd&days=270")
    res <- GET(url)
    data <- content(res, as = "parsed")
    
    if (is.null(data$prices)) return(NULL)
    
    df <- data.frame(
      time = as.POSIXct(sapply(data$prices, function(x) as.numeric(x[[1]]) / 1000), origin = "1970-01-01"),
      price = as.numeric(sapply(data$prices, function(x) x[[2]]))
    )
    
    df <- df[!is.na(df$price), ]
    df$date <- as.Date(df$time)
    return(df)
  })
  
  observeEvent(input$go, {
    df <- get_full_history_data()
    req(nrow(df) > 0)
    
    df_prophet <- data.frame(ds = df$time, y = df$price)
    model <- prophet(df_prophet)
    future <- make_future_dataframe(model, periods = 7)
    forecast <- predict(model, future)
    
    output$forecastPlot <- renderPlotly({
      p <- ggplot() +
        geom_line(data = forecast, aes(x = ds, y = yhat), color = "#00bcd4") +
        geom_ribbon(data = forecast, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), fill = "#00bcd4", alpha = 0.2) +
        labs(title = paste("Next 7-Day Prediction for", input$coin),
             x = "Date", y = "Predicted Price (USD)") +
        theme_minimal()
      ggplotly(p)
    })
    
    output$forecastTable <- renderDT({
      df_table <- forecast %>%
        select(ds, yhat, yhat_lower, yhat_upper) %>%
        tail(7) %>%
        mutate(
          Date = as.Date(ds),
          `Predicted Price` = round(yhat, 2),
          `Lower Bound` = round(yhat_lower, 2),
          `Upper Bound` = round(yhat_upper, 2)
        ) %>%
        select(Date, `Predicted Price`, `Lower Bound`, `Upper Bound`)
      
      datatable(
        df_table,
        rownames = FALSE,
        options = list(dom = 't', pageLength = 7),
        class = 'cell-border stripe hover compact dark-theme'
      ) %>%
        formatStyle(
          columns = names(df_table),
          backgroundColor = '#0d1117',
          color = 'white',
          fontWeight = 'bold'
        )
    })
  }) # <- THIS closes the observeEvent!
  
  # Histogram
  output$histogramPlot <- renderPlotly({
    df <- get_full_history_data()
    req(nrow(df) > 0)
    p <- ggplot(df, aes(x = price)) +
      geom_histogram(fill = "#00bcd4", bins = 30, color = "white") +
      labs(title = paste("Price Distribution -", input$coin),
           x = "Price (USD)", y = "Frequency") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Heatmap
  output$heatmapPlot <- renderPlotly({
    df <- get_full_history_data()
    req(nrow(df) > 0)
    df$month <- format(df$date, "%b")
    df$weekday <- weekdays(df$date)
    df_summary <- df %>%
      group_by(month, weekday) %>%
      summarise(avg_price = mean(price), .groups = "drop")
    
    df_summary$weekday <- factor(df_summary$weekday,
                                 levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    
    p <- ggplot(df_summary, aes(x = month, y = weekday, fill = avg_price)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "#ffe0b2", high = "#e65100") +
      labs(title = paste("Average Price Heatmap -", input$coin),
           x = "Month", y = "Day of Week") +
      theme_minimal()
    
    ggplotly(p)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
