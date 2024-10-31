library(shiny)
library(ggplot2)
library(tibble)
library(tidyr)

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
    req(df())
    df <- df()
    df[,3] <- as.numeric(gsub(",","",df[, 3]))
    cur_loss_year <- df[1,1]
    cur_loss_year_row <- 1
    dev_years <- max(df[,2]) #number of unique loss years = development years 
    cur_cum <- 0
    dataf <- data.frame(matrix(NA, nrow = length(unique(df[, 1])), ncol = dev_years))
    
    #Calculate the cumulative claims that are directly summed from the data
    for (row_num in 1:nrow(df)) {
      if (df[row_num, 1] != cur_loss_year){
        cur_loss_year     <- cur_loss_year     + 1
        cur_loss_year_row <- cur_loss_year_row + 1
        cur_cum <- 0 
      }
      cur_dev_year <- df[row_num, 2]
      cur_cum <- cur_cum + df[row_num, 3]
      dataf[cur_loss_year_row, cur_dev_year] = cur_cum 
    }
    
    #Calculate the cumulative claims from previous development years
    for (year_num in 2:dev_years) {
      for (n in seq(year_num - 2, 0)) {
        dataf[year_num, dev_years - n] <- (sum(dataf[1:year_num - 1, dev_years - n]) /
                                             sum(dataf[1:year_num - 1, dev_years - n - 1]) *
                                             dataf[year_num, dev_years - n - 1])
      }
    }
    
    #Calculate the cumulative claims from the tail value
    dataf[, dev_years + 1] <- dataf[, dev_years] * input$tail
    
    #set row names and column names
    rownames(dataf) <- as.character(unique(df[,1]))
    colnames(dataf) <- seq(1, ncol(dataf))
    return(dataf)
  })
  
  #Table output for first tab -------------
  output$show  <- renderTable({
    dataf <- as.data.frame(data())
    return(dataf)
  }, rownames = TRUE)
  
  #Plot output for second tab ------------
  output$graph <- renderPlot({
    df <- data()
    data_long <- df %>%
      rownames_to_column(var = "Loss Year") %>%
      pivot_longer(cols = -`Loss Year`,
                   names_to = "Development",
                   values_to = "Value")
    ggplot(data_long, aes(x = Development,
                          y = Value,
                          color = `Loss Year`,
                          group = `Loss Year`)) +
      geom_line() +
      geom_point() +
      labs(title = "Cumulative Claims by Loss Year",
           x = "Development Year",
           y = "Cumulative claims")
  })
}

shinyApp(ui, server)