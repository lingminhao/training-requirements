library(shiny)
library(plotly)
library(dplyr)
library(readr)

dataset <- as.data.frame(read_csv('gapminder_clean.csv', show_col_types = FALSE) 
                         %>% filter(continent != ""))

ui <- fluidPage(
  headerPanel('Gap minder Dataset Exploration'),
  sidebarPanel(
    sliderInput('year', 'Year', min = min(dataset$Year), max = max(dataset$Year), 
                step = 5, value = min(dataset$Year), sep = ""), 
    selectInput('xaxis','X axis', setdiff(colnames(dataset), c("...1", "Country Name", 
                                                               "Year", "pop", "continent"))),
    radioButtons('xlinearlog', label = NULL, choices = c('Linear', 'Log')), 
    selectInput('yaxis','Y axis', setdiff(colnames(dataset), c("...1", "Country Name", 
                                                               "Year", "pop", "continent"))),
    radioButtons('ylinearlog', label = NULL, choices = c('Linear', 'Log'))
  ),
  mainPanel(plotlyOutput('plot'))
)

server <- function(input, output) {
  newdata <- reactive({
    filtered_data <- dataset %>% 
      filter(Year == input$year) %>% 
      select(input$xaxis, input$yaxis, "Country Name", "pop", "continent")
    colnames(filtered_data) <- make.names(colnames(filtered_data))
    filtered_data
  })
  
  output$plot <- renderPlotly({4
    newdata <- newdata()
    fig <- plot_ly(
      x = newdata()[,make.names(input$xaxis)], 
      y = newdata()[,make.names(input$yaxis)],
      type = 'scatter', 
      mode = 'markers', 
    size = newdata()$pop,
    sizes = c(10,1000),
    color = newdata()$continent,
    colors =  c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951'),
    hoverinfo = 'text',
    text = ~paste(newdata()$Country.Name,
                  '<br>Continent:', newdata()$continent,
                  paste0('<br>',input$xaxis, ':'), newdata()[,make.names(input$xaxis)],
                  paste0('<br>',input$yaxis, ':'), newdata()[,make.names(input$yaxis)],
                  '<br>Pop:', newdata()$pop
    )
    ) %>% layout(xaxis = list(title = input$xaxis),
                 yaxis = list(title = input$yaxis), legend = 'continent')
    if (input$xlinearlog == 'Log'){
      fig <- fig %>% layout(xaxis = list(type = "log"))
    }
    if (input$ylinearlog == 'Log'){
      fig <- fig %>% layout(yaxis = list(type = 'log'))
    }
    fig
  }
  )
}

shinyApp(ui,server)



