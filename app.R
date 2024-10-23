library(shiny)
library(ggplot2)

ui <- fluidPage(
  fluidRow(
    column(4,
           h4("Inputs"),
           fileInput("upload","Choose claims data file (.csv only)"),
           sliderInput("tail", "Tail", value = 1.1, min = 1, max = 2)
    ),
    column(8,
           tabsetPanel(
             tabPanel("Cumulative Paid Claims ($)",
                      h4("Cumulative Paid Claims"),
                      h5("Development Year"),
                      tableOutput("show"),
             ),
             tabPanel("Plot of Cumulative Paid Claims",
                      plotOutput("graph"))
           )
    )
  )
)

server <- function(input, output, server) {
  #Read the csv and save as dataframe 
  df <- reactive({ 
    req(input$upload)
    read.csv(input$upload$datapath)
  })
  
  #Extract needed values and perform needed calculations from dataframe
  data <- reactive({
    dataf <- data.frame()
    req(df())
    values <- as.numeric(gsub(",","",df()[, 3]))
    loss_years_unique_count <- length(unique(df()[, 1]))
    forecast_length <- max(df()[,2])
    for (i in 1:loss_years_unique_count){
      prev_claims_count <- (i - 1) * forecast_length - (i - 1) * i / 2
      cum_claims <- 0
      claims_list <- c()
      for (n in 1:forecast_length){
        if (n <= forecast_length - i){
          cum_claims <- cum_claims + values[n + prev_claims_count]
        } else if (n < forecast_length){
          cum_claims = sum(dataf[, n]) / sum(dataf[, n - 1]) * claims_list[length(claims_list)]
        } else {
          cum_claims = claims_list[length(claims_list)] * input$tail
        }
        claims_list <- c(claims_list, cum_claims)
      }
      dataf <- rbind(dataf, claims_list)
    }
    return(dataf)
  })
  
  #Table output for first tab -------------
  output$show  <- renderTable({
    df <- as.data.frame(t(data()))
    colnames(df) <- as.character(df[1,])
    df <- df[-1,]
    rownames(df) <- c(2017, 2018, 2019)
    df <- round(df)
  }, rownames = TRUE)
  
  #Plot output for second tab ------------
  output$graph <- renderPlot({
    df <- data()
    ggplot()+
      geom_line(data = df, aes(x = Years, y = Y2017, color = "Loss Year 2017")) +
      geom_line(data = df, aes(x = Years, y = Y2018, color = "Loss Year 2018")) +
      geom_line(data = df, aes(x = Years, y = Y2019, color = "Loss Year 2019")) +
      labs(title = "Cumulative claims paid",
           x = "Development Year",
           y = "Claims Paid") +
      scale_color_manual(name = "Development Year", 
                         values = c("Loss Year 2017" = "blue",
                                    "Loss Year 2018" = "red",
                                    "Loss Year 2019" = "green")) +
      scale_y_continuous(limits = c(0,NA)) +
      theme_minimal()
  })
}

shinyApp(ui, server)