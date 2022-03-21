
library(dplyr)
library(shiny)

midwest <- data.frame(midwest, stringsAsFactors = FALSE)

shinyServer(function(input, output) {
  output$scatter <- renderPlotly({
    filtered <- midwest %>% filter(
      poppovertyknown <= input$poppovertyknown
    )
    x <- filtered[[input$x_var]]
    y <- filtered[[input$y_var]]
    title <- paste0("Midwest Dataset: ", input$x_var, " v.s.", input$y_var)
    p <- ggplot() +
      geom_point(mapping = aes(x = x, y = y)) +
      labs(x = input$x_var, y = input$y_var, title = title)
    p
  })
  output$plot <- renderPlotly({
    selected_state <- midwest %>% filter(
      state == input$state
    )
    x <- selected_state[[input$xvar]]
    y <- selected_state[[input$yvar]]
    title_states <- paste0("Midwest Data: ", input$xvar, "VS.", input$yvar)
    pp <- ggplot() +
      geom_point(mapping = aes(x = x, y = y)) +
      labs(title = title_states, x = input$xvar, y = input$yvar)
    pp
  })
})