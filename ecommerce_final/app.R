#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

ui <-
    
    navbarPage(
    
        theme = shinytheme("flatly"),
        
        "Grocery Shopping Trends",
        
        tabPanel("About", 
                 titlePanel("About"),
                 h3("Project Background"),
                 p("Hello, this is where I talk about my project."),
                 h3("About Me"),
                 p("Hi there! This project was created by Raymond Hu (Harvard 
                 College '20). 
                 You can reach him at raymond.zhang.hu@gmail.com.")),
    
        tabPanel("Page 1",
                 titlePanel("Discussion Title"),
                 
                 sidebarLayout(
                     sidebarPanel(
                         h4("About"),
                         p("Paragraph"),
                         selectInput(inputId = "order_dow",
                                     label = "Day of the Week",
                                     choices = list("Sunday" = 0,
                                                    "Monday" = 1,
                                                    "Tuesday" = 2,
                                                    "Wednesday" = 3,
                                                    "Thursday" = 4,
                                                    "Friday" = 5, 
                                                    "Saturday" = 6)
                         )
                     ),
                     
                     mainPanel(
                         plotOutput("graph1")
                     )
                 )
                     
         ),
                 
        
        tabPanel("Model",
                 fluidPage(
                     titlePanel("Model Title"),
                     sidebarLayout(
                         sidebarPanel(
                             selectInput(
                                 "plot_type",
                                 "Plot Type",
                                 c("Option A" = "a", "Option B" = "b")
                             )),
                         mainPanel(plotOutput("insta_productnum_day_week")))
                 ))
        
        
    
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$graph1 <- renderPlot({
        
        instacart_dow_product <- read_rds("instacart_dow_product.rds")
        
        instacart_dow_product %>% 
            
        filter(order_dow == input$order_dow) %>%     
        ggplot(aes(x = order_hour_of_day, y = n, color = department)) +
        geom_line() +
        theme_classic()
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
