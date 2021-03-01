library(shiny)
library(dplyr)
library(ggplot2)

covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

ui <- fluidPage(
  selectInput(inputId = "state",
              label = "State:",
              choices = c(unique(covid19$state)),
              multiple = TRUE),
  submitButton(text = "Submit"),
  plotOutput(outputId = "plot")
)
server <- function(input, output) {
  output$plot <- renderPlot(
    covid19 %>% 
      filter(state %in% c(input$state),
             cases >= 20) %>% 
      group_by(state) %>% 
      mutate(days_0 = as.numeric(difftime(date, lag(date, 1)))) %>% 
      replace_na(list(days_0 = 0)) %>% 
      mutate(days_since_20 = cumsum(days_0)) %>%  
      select(-days_0) %>% 
      ggplot(aes(x = days_since_20, y = cases)) +
      geom_line(aes(color = state)) +
      scale_y_log10() + 
      labs(x = "Days Since 20 Cases",
           y = "# Cases",
           color = "State")
  )
}
  
shinyApp(ui = ui, server = server)