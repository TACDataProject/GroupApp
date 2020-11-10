# Download TAC Group

library(shiny)
library(shinycssloaders)
library(dplyr)
library(ggplot2)
library(tidyr)

# Load data
autolink <- readRDS("Data/Link_202010.rds")
gtd <- readRDS("Data/GTD0814.rds")
tac <- readRDS("Data/TAC_202010.rds")
sideb.list <- sort(unique(tac$sideb))

ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("sideb", "Choose a rebel group (UCDP SideB):", 
                  choices = sideb.list),
      downloadButton("downloadEvent", "Download Event Data"),
      downloadButton("downloadCount", "Download Count Data")
    ),
    
    mainPanel(
      plotOutput("plot1") %>% withSpinner()
    )
  )
)

server <- function(input, output) {
  
  eventInput <- reactive({
    left_join(subset(autolink,sideb==input$sideb), gtd)
  })
  
  countInput <- reactive({
    subset(tac,sideb==input$sideb)
  })
  
  output$plot1 <- renderPlot({
    countInput() %>% 
    gather(key, value, t_a, t_e, t_f) %>% 
    ggplot(aes(x = year, y = value, color = key, linetype = key)) +
      geom_line() +
      labs(title=paste("TAC Count Data for", input$sideb),x="Year", 
           y = "Incidents", color = "Match Level", linetype = "Match Level") +
      theme_classic()
  })
  
  output$downloadEvent <- downloadHandler(
    filename = "event.csv",
    content = function(file) {
      write.csv(eventInput(), file)
    }
  )
  
  output$downloadCount <- downloadHandler(
    filename = 'count.csv',
    content = function(file) {
      write.csv(countInput(), file)
    }
  )
}

shinyApp(ui, server)