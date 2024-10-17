# library(shiny)
# install.packages("readxl")
# library(readxl)
# library(ggplot2)
ui <- fluidPage(
  fileInput("upload","Choose claims data file"),
  sliderInput("tail", "Tail", value = 1.1, min = 1, max = 2),
  tableOutput("show"),
  plotOutput("graph")
)

server <- function(input, output, server) {
  df <- reactive({ 
    req(input$upload)
    read_xlsx(input$upload$datapath, sheet = 2)
  })
  data <- reactive({
    req(df())
    num171 = as.numeric(df()[2,3])
    num172 = as.numeric(df()[3,3])
    num173 = as.numeric(df()[4,3])
    num181 = as.numeric(df()[5,3])
    num182 = as.numeric(df()[6,3])
    num191 = as.numeric(df()[7,3])
    cum172 = num171+num172
    cum182 = num181 + num182
    cum192 = (cum172+cum182)/(num171+num181)*num191
    cum173 = cum172 + num173
    cum183 = cum173 / cum172 * cum182
    cum193 = cum173 / cum172 * cum192
    cum174 = cum173 * input$tail
    cum184 = cum183 * input$tail
    cum194 = cum193 * input$tail
    dataf <- data.frame(
      "Years" = c(1,2,3,4),
      "Y2017" = c(num171, cum172, cum173, cum174),
      "Y2018" = c(num181, cum182, cum183, cum184),
      "Y2019" = c(num191, cum192, cum193, cum194)
    )
    return(dataf)
  })
  output$show  <- renderTable(data())
  output$graph <- renderPlot({
    df <- data()
    ggplot()+
      geom_line(data = df, aes(x = Years, y = Y2017, color = "Loss Year 2017")) +
      geom_line(data = df, aes(x = Years, y = Y2018, color = "Loss Year 2018")) +
      geom_line(data = df, aes(x = Years, y = Y2019, color = "Loss Year 2019")) +
      labs(title = "Cumulative claims paid",
           x = "Development Year",
           y = "Claims Paid") +
      scale_color_manual(name = "Development Year", values = c("Loss Year 2017" = "blue",
                                                               "Loss Year 2018" = "red",
                                                               "Loss Year 2019" = "green")) +
      theme_minimal()
  })
}

shinyApp(ui, server)
library(curl)
curl::curl_fetch_memory("http://www.google.com")
