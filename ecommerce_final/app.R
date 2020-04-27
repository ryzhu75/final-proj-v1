#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(dplyr)
library(shiny)
library(shinythemes)

# Read in dataset
instacart_dow_product <- readRDS("instacart_dow_product.rds")

# list_departments <- 
#     instacart_dow_product %>% 
#     ungroup(order_dow, order_hour_of_day, department, department_id) %>% 
#     distinct(department) %>% 
#     lapply(as.character)

# ui

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
                        
                         sliderInput("order_hour_of_day",
                                     "Hour of Day:",
                                     min = 0, max = 23,
                                     value = 23, 
                                     animate = animationOptions(interval = 1300, loop = TRUE, playButton = "Play",
                                                                pauseButton = "Pause")
                                     
                                     
                         ),
                          
                         selectInput(inputId = "order_dow",
                                     label = "Day of the Week:",
                                     choices = list(
                                         "Sunday" = 0,
                                         "Monday" = 1,
                                         "Tuesday" = 2,
                                         "Wednesday" = 3,
                                         "Thursday" = 4,
                                         "Friday" = 5,
                                         "Saturday" = 6
                                     )


                         )
                         
                         
                         ,
                
                         radioButtons(inputId = "department",
                                      label = "Department",
                                      choices = list(
                                          "Alcohol" = "alcohol",
                                          "Babies" = "babies",
                                          "Bakery" = "bakery",
                                          "Beverages" = "beverages",
                                          "Breakfast" = "breakfast",
                                          "Bulk" = "bulk",
                                          "Canned goods" = "canned goods",
                                          "Dairy eggs" = "dairy eggs",
                                          "Deli" = "deli",
                                          "Dry goods, pasta" = "dry goods pasta",
                                          "Frozen" = "frozen",
                                          "Household" = "household",
                                          "International" = "international",
                                          "Meat, seafood" = "meat seafood",
                                          "Other" = "other",
                                          "Pantry" = "pantry",
                                          "Personal care" = "personal care",
                                          "Pets" = "pets",
                                          "Produce" = "produce",
                                          "Snacks" = "snacks"
                                          
                                      ))
                     ),
                     
                     mainPanel(
                         plotOutput("graph1"),
                         plotOutput("graph2")
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

        
        # Plot graph of hour of day vs. total purchases
        instacart_dow_product %>% 
        
        group_by(order_dow, order_hour_of_day) %>% 
        
        summarize(n = sum(n)) %>% 
            
        # Filtered by input (order of day)    
        filter(order_dow == input$order_dow) %>%  
            
        # Filtered by hour (input slider)
        filter(order_hour_of_day %in% (0:input$order_hour_of_day)) %>%     
        ggplot(aes(x = order_hour_of_day, y = n)) +
        geom_line() +
        theme_classic() +
        theme(legend.position = "bottom") +
        scale_color_discrete(name = "Department") +
        scale_y_continuous(name = "# Purchases", limits = c(0, 500000)) +
        scale_x_continuous(name = "Hour of Day", limits = c(0, 23)) 
        
    })
    
    output$graph2 <- renderPlot({
        
        
        # Plot graph of hour of day vs. total purchases
        instacart_dow_product %>% 
            
            #Filtered by input (order of day)    
            filter(order_dow == input$order_dow) %>%  
            filter(department == input$department) %>% 
            filter(order_hour_of_day %in% (0:input$order_hour_of_day)) %>% 
            ggplot(aes(x = order_hour_of_day, y = n, color = department)) +
            geom_line() +
            theme_classic() +
            theme(legend.position = "bottom") +
            scale_color_discrete(name = "Department") +
            scale_y_continuous(name = "# Purchases", limits = c(0, 200000)) +
            scale_x_continuous(name = "Hour of Day", limits = c(0, 23))
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
