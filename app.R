#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(shinydashboard)

# Read in categories for the user's purchases
if (file.exists("categories.rds")) {
  category_list <- readRDS("categories.rds")
} else {
  category_list <- c("Restaurants", "Groceries")
}

# Initialize database if one doesn't exist
if (file.exists("shiny_database.rds")) {
  shiny_database <- readRDS("shiny_database.rds")
} else {
  shiny_database <- data.table(name = character(),
                               description = character(),
                               category = character(),
                               price = numeric())
}

# Define UI for the budget application
ui <- dashboardPage(
  
    dashboardHeader(),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("New Purchase", tabName = "new_purchase", icon = icon("receipt"))
      )
      
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard"),
        tabItem(tabName = "new_purchase",
                fluidRow(
                  column(6,
                         textInput("item_name", label = "Item Name", placeholder = "Ex. Detective Pikachu"),
                         
                         selectInput("new_or_existing", label = "Category", choices = c("Select Existing", "Create New")),
                         
                         conditionalPanel(
                           condition = "input.new_or_existing === 'Select Existing'",
                           selectInput("category", label = "Name", choices = category_list)
                         ),
                         
                         conditionalPanel(
                           condition = "input.new_or_existing === 'Create New'",
                           textInput("new_category", label = "Name")
                         )
                         
                  ),
                  column(6,
                         numericInput("price", label = "Price", value = 0),
                         
                         textInput("description", label = "Description", placeholder = "Ex. Movie ticket and popcorn"),
                         br(),
                         h5(""),
                         
                         actionButton("item_button", "Submit")
                  )
                )
        )
      )
    )
  )
# Define server logic required to draw a histogram
server <- function(input, output) {
  # Submit button for newly entered item
  observeEvent(input$item_button, {
    
    if (input$new_or_existing == "Select Existing") {
      # Gather all relavant data
      item_data <- list(input$item_name, input$description, input$category, input$price)
    } else {
      # Gather all relavant data
      item_data <- list(input$item_name, input$description, input$new_category, input$price)
      
      # Add new category
      category_list <- c(category_list, input$new_category)
      
      # Save categories
      saveRDS(category_list, "categories.rds")
    }
    
    # Add new row to database
    shiny_database <- rbind(shiny_database, item_data)
    
    # Save database
    saveRDS(shiny_database, "shiny_database.rds")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
