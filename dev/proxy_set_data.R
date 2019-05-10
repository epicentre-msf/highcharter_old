rm(list = ls())

library(shiny)
library(highcharter)

options(highcharter.theme = hc_theme_smpl())

df <- dplyr::tibble(
  date = seq.Date(as.Date("2019-01-01"), as.Date("2019-12-01"), by = "months"),
  x = runif(12, 0, 50),
  y = runif(12, 0, 50),
  z = runif(12, 0, 50)
) %>% 
  tidyr::gather(group, value, -date)

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
    
    hc <- hchart(df, "line", hcaes(date, value, group = group)) %>% 
      hc_title(text = "Title") %>% 
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
    df_new <- dplyr::tibble(
      date = seq.Date(as.Date("2019-01-01"), as.Date("2019-12-01"), by = "months"),
      x = runif(12, 0, 50),
      y = runif(12, 0, 50),
      z = runif(12, 0, 50)
    ) %>% 
      tidyr::gather(group, value, -date)
    
    highchartProxy(shinyId = "hc_1") %>% 
      hc_set_title(title_opts = list(text = paste('Date Update', input$update))) %>% 
      #hc_add_plotline(options = list(id = 'plotline_range_min', color = 'red', value = 1, width = 1, zIndex = 2), axis = 'y') %>% 
      hc_set_data(type = "line", data = df_new, mapping = hcaes(date, value, group = group), redraw = TRUE)
  }, ignoreInit = TRUE)
  
}

shinyApp(ui = ui, server = server)
