rm(list = ls())

library(shiny)
library(highcharter)

options(highcharter.theme = hc_theme_smpl())

ui <- fluidPage(
  h2("Highcharter as Shiny Inputs"),
  fluidRow(
    column(12, 
      h3("Point Event"), 
      highchartOutput("hc_1"),
      actionButton(inputId = "update", label = "Update chart", width = "100%")
    )
  )
)

server = function(input, output) {
  
  output$hc_1 <- renderHighchart({

    hc <- highcharts_demo() %>% 
      hc_plotOptions(
        series = list(
          cursor = "pointer"
        )
      ) %>% 
      hc_add_event_point(event = "mouseOver") %>% 
      hc_add_event_point(event = "click")
    hc
    
  })

 observeEvent(input$update, {
    highchartProxy(shinyId = "hc_1") %>% 
      hc_set_title(title_opts = list(text = 'truc')) %>% 
      hc_add_plotline(options = list(id = 'plotline_range_min', color = 'red', value = 1, width = 1, zIndex = 2), axis = 'y')
 }, ignoreInit = TRUE)
 
}

shinyApp(ui = ui, server = server)
