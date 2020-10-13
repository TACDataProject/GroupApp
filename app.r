# Download TAC Group

library(shiny)
library(shinycssloaders)
library(dplyr)
library(ggplot2)
library(tidyr)

# # Set up data
# autolink <- haven::read_dta("Data/Link.dta")
# saveRDS(autolink, "Data/Link_202010.rds")
# 
# gtd <- haven::read_dta("Data/GTD0814.dta")
# saveRDS(gtd, "Data/GTD0814.rds") 
# 
# tac.group <- readRDS("Data/TAC_group_202010.rds")
# 
# ucdp <- haven::read_dta("Data/UCDP2014.dta")
# saveRDS(ucdp, "Data/UCDP2014.rds") 
# 
# sideb <- ucdp %>% 
#   select(sidebid, sideb) %>% 
#   mutate(sidebid = as.numeric(sidebid)) %>% 
#   filter(!is.na(sidebid)) %>% 
#   distinct() %>% 
#   arrange(sidebid)
# 
# tac <- left_join(tac.group, sideb) %>% 
#   select(sidebid, sideb, everything())
# saveRDS(tac, "Data/TAC_202010.rds")
# 
# autolink <- haven::read_dta("Data/Link.dta")
# saveRDS(autolink, "Data/Link_202010.rds")

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
  
# tableOutput("table")
# headerPanel("Download Terrorism in Armed Conflict (TAC) Data")

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