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
library(gganimate)
library(gt)
#library(tidymodels)

# Read in dataset for graphs 1 and 2
instacart_dow_product <- readRDS("instacart_dow_product.rds")

# Read in dataset for graph 3
combine_trans_hour <- readRDS("combine_trans_hour.rds")

# Read in dataset for graph 4
combine_basket_size <- readRDS("combine_basket_size.rds")

# Read in data set for graph 5 and table 1
prior_size <- readRDS("prior_size.rds")

# Read in data set for graph 6
insta_by_department <- readRDS("insta_by_department.rds")


ui <-
    
    navbarPage(
    
        # Set theme
        
        theme = shinytheme("flatly"),
        
        # Set Title
        
        "Online Grocery Shopping Case Study: Instacart",
        
        # First content page: About
        tabPanel("About", 
                 titlePanel("About"),
                 h3("Project Background"),
                 p("In recent years, online grocer delivery services have been 
                 growing steadily, disrupting the traditional brick and mortar 
                 grocery store model. Proprietary services like Instacart, 
                 FreshDirect, and Shipt have even pushed supermarket chains to
                 develop their own food delivery services, such as Walmart 
                 Grocery and Safeway. Online grocery shopping offers unprecedented 
                 accessibility and ways to shop. This project provides an overview 
                 of one of the biggest online grocers, Instacart, and compares its 
                 usage data to that of an anonymized brick and mortar exclusive 
                   grocery store."), 
                 h3("Findings"),
                 p("Instacart usage activity is highest on the weekends and the most commonly
                   purchased products come from produce and dairy sections, behavior that is similar to what
                   I would expect from a traditional brick and mortar grocery. However, 
                   compared to a traditional brick and mortar grocer, purchasing behavior on Instacart is more smoothed 
                   throughout the day. The peak of traffic a traditional grocer experiences during rush hour
                   still exists, but starts several hours earlier and is not as concentrated. Further, Instacart basket 
                   sizes remain largely constant throughout the day whereas the traditional grocer's customer basket sizes
                   peak during the day and fall in the night and mornings. Finally, a regression of 
                   basket size on the time since a customer's last reorder shows that customers with
                   more items in their current basket are associated with a longer time since their last order.
                   Feel free to explore the interactive diagrams on this website!"),
                 h3("About Me"),
                 p("Hi there! This project was created by me, Raymond Hu, 
                 Harvard College Class of 2020. I study Economics with a secondary 
                 in East Asian Studies and love to study data with the help of R.
                 You can reach me at raymond.zhang.hu@gmail.com.")),
    
        # Second content page: Instacarrt Overview
        
        tabPanel("Instacart Overview",
                 titlePanel("Overview"),
                 
                 sidebarLayout(
                     sidebarPanel(
                         h4("About"),
                         p("This is an overview of Instacart's customer purchasing 
                         data, organized by day of week and by hour of day. You can 
                         filter by department below. Much to my surprise, 
                         Instacart's usage is more in line with what I would think 
                         traditional grocers would be, with a spike during post-work 
                         hours and on weekends."),
                        
                         # Slider feature to control hour of day on both graphs
                         
                         sliderInput("order_hour_of_day",
                                     "Hour of Day:",
                                     min = 0, max = 23,
                                     value = 23, 
                                     animate = animationOptions(interval = 1300, loop = TRUE, playButton = "Play",
                                                                pauseButton = "Pause")
                                     
                                     
                         ),
                          
                         # Drop down menu that allows users to select day of week
                         
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
                
                         # Radio buttons that allow the user to sort data by department
                         
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
                     
                     # Plot outputs
                     
                     mainPanel(
                         plotOutput("graph1"),
                         plotOutput("graph2"),
                         plotOutput("graph6")
                     )
                 )
                     
         ),
                 
        # Third Content Page: Online vs. Brick and Mortar Comparison
        
        tabPanel("Online vs. Brick and Mortar",
                 titlePanel("Online vs. Brick and Mortar"),
                 sidebarLayout(
                     sidebarPanel(
                         h4("About"),
                         p("This page offers comparison metrics between Instacart
                           and the brick and mortar grocer. Although similar, 
                           Instacart's usage tends to be more consistent throughout 
                           the day. This makes intuitive sense as it is more easily 
                           accessible than having to physically drive or walk to 
                           a store."),
                         selectInput(
                             inputId = "p2_input",
                             label = "Differences",
                             choices = c(
                                 "Distribution of Transactions" = "graph3",
                                 "Unique Items per Basket" = "graph4"
                             )
                         )
                         ),
                        
                 mainPanel(plotOutput("graph34")))
             ),
        
        # Fourth Content Page: Regression

        tabPanel("Regression",
                 titlePanel("Instacart Purchasing Analysis"),
                 sidebarLayout(
                     sidebarPanel(
                         h4("Regression Analysis"),
                         p("What is the relationship between the number of days 
                            since the last order and the number of items in the 
                            current order? My hypothesis is that there is a
                            positive relationship between the two, as someone 
                            ordering more now likely has gone a longer time and has consumed
                            more since their last resupply. The regression results 
                            are consistent with this hypothesis,
                            showing a positive relationship between the number of days since the last 
                            order and the number of unique items in the current 
                            order basket. According 
                           to the regression, a 1 item increase in the basket is 
                           associated with a 0.0695 increase in the number of days 
                           since a customer's last order")
                     ),

                     mainPanel(imageOutput("image1"),
                               imageOutput("table1"))
                 )
        ),
        
        # Data page: Outlines the dataset and where I got the data from
        
        tabPanel("Data", 
                 titlePanel("Data"),
                 h3("Instacart"),
                 p("In 2017, Instacart released a dataset with over 3 million
                   recorded orders, the largest to date of any online grocer. 
                   The link to the data set can be found here: 
                   https://www.instacart.com/datasets/grocery-shopping-2017."), 
                 h3("Brick and Mortar"),
                 p("The data for the brick and mortar grocery store was retrieved 
                 from dunnnhumby.com. This dataset containts hosehold level 
                 transactions over two years from a group of 2,500 households 
                 who are frequent shoppers at a grocery retrailer. The data can
                 be found here: https://www.dunnhumby.com/careers/engineering/sourcefiles?sourcefile=https%3A//www.dunnhumby.com/sites/default/files/sourcefiles/dunnhumby_The-Complete-Journey.zip"),
                 h3("Citations"),
                 p("“The Instacart Online Grocery Shopping Dataset 2017”, 
                 Accessed from https://www.instacart.com/datasets/grocery-shopping-2017 on 4/27/2020")
    
    )
    
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$graph1 <- renderPlot({

        
        # Plot graph of total purchases throughout the day
        instacart_dow_product %>% 
        
        group_by(order_dow, order_hour_of_day) %>% 
        
        summarize(n = sum(n)) %>% 
            
        # Filtered by input (order of day)    
        filter(order_dow == input$order_dow) %>%  
            
        # Filtered by hour (input slider)
        filter(order_hour_of_day %in% (0:input$order_hour_of_day)) %>%     
            
        ggplot(aes(x = order_hour_of_day, y = n)) +
        geom_line() +
        labs(title = "Total Purchases") +
        theme_classic() +
        theme(legend.position = "bottom") +
        scale_color_discrete(name = "Department") +
        scale_y_continuous(name = "# Purchases", limits = c(0, 750000)) +
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
            labs(title = "Total Purchases by Department") +
            theme_classic() +
            theme(legend.position = "bottom") +
            scale_color_discrete(name = "Department") +
            scale_y_continuous(name = "# Purchases"
                               # , limits = c(0, 200000)
                               ) +
            scale_x_continuous(name = "Hour of Day", limits = c(0, 23))
        
    })
    
    output$graph6 <- renderPlot({
        
        insta_by_department %>% 
        ggplot(aes(x = reorder(department, -percent), y = 100*percent)) +
        geom_col() +
        scale_y_continuous(name = "% of Total Purchases") +
        scale_x_discrete(name = "Department") +
        labs(title = "Purchase Distribution by Department") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, vjust = .9, hjust = .95))
           
    })
    
    output$graph34 <- renderPlot({
        
        # Plot different graphs depending on the input from the drop down side menu in "Online vs. Brick and Mortar"
        
        # Plot describes distribution of grocery store purchases throughout the day
        
        graph_3 <- combine_trans_hour %>% 
            ggplot(aes(x = trans_hour, y = percent, color = source)) +
            geom_line() +
            theme_classic() +
            labs(title = "Distribution of Grocery Store Transactions throughout the Day",
                 y = "% of Total Purchases",
                 x = "Hour of Day") +
            scale_y_continuous(labels = scales::percent) +
            scale_color_discrete(name = "", labels = c("Brick and Mortar", "Instacart")) +
            theme(legend.position = "bottom") 
        
        # Plot describes the # unique items per basket for each hour
        
        graph_4 <- combine_basket_size %>% 
            ggplot(aes(x = trans_hour, y = basket_size, color = source)) +
            geom_line() +
            labs(title = "Average Number of Unique Items per Basket") +
            scale_color_discrete(name = "", labels = c("Brick and Mortar", "Instacart")) +
            scale_x_continuous(name = "Hour of Day") +
            scale_y_continuous(name = "# Unique Items / Basket", limits = c(0, 12)) +
            theme_classic() +
            theme(legend.position = "bottom")
        
        
        # Displays the appropriate plot depending on the input from the drop menu
        
        ifelse(input$p2_input == "graph3",
               print(graph_3),
               print(graph_4))
        
    })
    
    
    # Image of gt table of regression results of prior_size (unique items x days since last order)
    
    output$table1 <- renderImage({
        
    prior_size_table <- normalizePath(file.path('prior_size_table.png/prior_size_table.png'))
    
    list(src = prior_size_table,
         width = 625,
         height = 150, 
         alt = "Error")
        
    }, deleteFile = FALSE)
    
    # Image of the scatterplot + line of best fit for prior_size table
    
    output$image1 <- renderImage({

        prior_size_image <- normalizePath(file.path('prior_size_image.png'))
        
        list(src = prior_size_image,
             width = 750,
             height = 375)
        
    }, deleteFile = FALSE)

}

# Run the application 
shinyApp(ui = ui, server = server)
