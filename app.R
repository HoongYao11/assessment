 library(shiny)
 library(readxl)
 library(ggplot2)

ui <- fluidPage(
  fluidRow(
    column(4,
           h4("Inputs"),
           fileInput("upload","Choose claims data file"),
           sliderInput("tail", "Tail", value = 1.1, min = 1, max = 2)
           ),
    column(8,
      tabsetPanel(
        tabPanel("Cumulative Paid Claims ($)",
                 h4("Cumulative Paid Claims"),
                 h5("Development Year"),
                    tableOutput("show")
                 ),
        tabPanel("Plot of Cumulative Paid Claims",
                    plotOutput("graph"))
        )
      )
    )
  )

server <- function(input, output, server) {
  df <- reactive({ 
    req(input$upload)
    read_xlsx(input$upload$datapath, sheet = 2)
  })
  data <- reactive({
    req(df())
    values <- as.numeric(unlist(df()[2:7, 3]))
    cum172 = sum(values[1:2])
    cum182 = sum(values[4:5])
    cum192 = (cum172 + cum182) / (values[1] + values[4])* values[6]
    cum173 = cum172 + values[3]
    cum183 = cum173 / cum172 * cum182
    cum193 = cum173 / cum172 * cum192
    dataf <- data.frame(
      "Years" = c(1,2,3,4),
      "Y2017" = c(values[1], cum172, cum173, cum173 * input$tail),
      "Y2018" = c(values[4], cum182, cum183, cum183 * input$tail),
      "Y2019" = c(values[6], cum192, cum193, cum193 * input$tail)
    )
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