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
library(DT)
library(openxlsx)

source("shiny_budget_helpers.R")

# load data
shiny_database <- load_db()
date_system_choices <- get_date_system_choices()

# Define UI for the budget application
ui <- dashboardPage(
  
    dashboardHeader(),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("New Purchase", tabName = "new_purchase", icon = icon("receipt")),
        menuItem("Upload Data", tabName = "data_upload", icon = icon("file-upload"))
      )
      
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard",
                DT::dataTableOutput("expense_table"),
                plotOutput("bp_by_category")),
        tabItem(tabName = "new_purchase",
                fluidRow(
                  column(6,
                         textInput("item_name", label = "Item Name", placeholder = "Ex. Detective Pikachu"),
                         
                         selectInput("new_or_existing", label = "Category", choices = c("Select Existing", "Create New")),
                         
                         conditionalPanel(
                           condition = "input.new_or_existing === 'Select Existing'",
                           selectInput("category", label = "Name", selected = "Categories", choices = shiny_database$category)
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
        ),
        
        tabItem( tabName = "data_upload",
                 fluidRow(
                   column(6,
                          fileInput("file_metadata", 
                                    label = "Data File",
                                    accept = c(
                                      ".xlsx"
                                    )),
                          selectInput("date_format", label = "Date System", choices = date_system_choices, selected = "1900")
                          )
                 ),
                 tableOutput(
                   "file_contents"
                 )
        )
      )
    )
  )
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Submit button for newly entered item
  observeEvent(input$item_button, {
    
    # add new row to database
    update_db(new_item())
    
    # clear inputs for a new submission
    reset_inputs()
    
  })
  
  observeEvent(input$expense_table_cell_edit, {
    # update display
    displayed_db(editData(shiny_database, input$expense_table_cell_edit, "expense_table"))
    
    #update shiny_database
    info = input$expense_table_cell_edit
    i = info$row
    j = info$col
    v = info$value
    shiny_database[i,j] <<- eval(parse(text = paste0("as.", typeof(shiny_database[[j]]), "(v)")))
  })
  
  onSessionEnded(function() {
    saveRDS(shiny_database, "shiny_database.rds")
  })
  
  ############################### Functions ###################################
  
  # update database with new data either from file upload or new item submit
  update_db <- function(input_db) {
    # Add new row to database
    shiny_database <<- rbind(shiny_database, input_db)
    
    # Save database
    saveRDS(shiny_database, "shiny_database.rds")

    # Update the displayed database
    displayed_db(shiny_database)
  }
  
  # clear inputs on submit. Also add new categories if needed.
  reset_inputs <- function() {
    updateTextInput(session, inputId = "item_name", value = "")
    updateSelectInput(session, inputId = "new_or_existing", selected = "Select Existing")
    updateSelectInput(session, inputId = "category", choices = shiny_database$category)
    updateTextInput(session, inputId = "new_category", value = "")
    updateNumericInput(session, inputId = "price", value = 0)
    updateTextInput(session, inputId = "description", value = "")
  }
  
  ############################### Objects #####################################
  
  # This is the database that will get updated every time the user adds a new item
  displayed_db <- reactiveVal(shiny_database)
  
  # Gather data for newly entered item
  new_item <- reactive({
    date <- format(Sys.Date(), "%m/%d/%y")
    if (input$new_or_existing == "Select Existing") {
      # Gather all relevant data (get category from dropdown menu)
      item_data <- list(input$item_name, input$description, input$category, input$price, date)
    } else {
      # Gather all relevant data (get category from text input)
      item_data <- list(input$item_name, input$description, input$new_category, input$price, date)
    }
    item_data
  })
  
  # Contents of the uploaded file. Displays error message if file is invalid
  upload_file <- observe({
    # get file metadata (size, path to file, etc)
    file_metadata <- input$file_metadata

    # check that a file is selected 
    validate(
      need(file_metadata, "")
    )
    # check that file has the .xlsx extension
    validate(
      need(substr(file_metadata, nchar(file_metadata)-4, nchar(file_metadata)) == ".xlsx",
           "Invalid file extension. Please upload an Excel file (.xlsx extension).")
    )
    
    # read file
    new_data <- read.xlsx(file_metadata$datapath)
    
    # check that data has the correct columns
    expected_cols <- c("name", "description", "category", "price", "date")
    validate (
      need(colnames(new_data) == expected_cols, 
           "Looks like the file isn't in the proper format.\nPlease make you have these columns: name, description, category, price, date.")
    )
    # Convert from excel data format to R
    if (input$date_format == "1900") {
      new_data$date <- format(as.Date(new_data$date, origin = "1899-12-30"), "%m/%d/%y")
    } else {
      new_data$date <- format(as.Date(new_data$date, origin = "1904-01-01"), "%m/%d/%y")
    }
    
    update_db(as.data.table(new_data))
  })
  
  ############################### Outputs #####################################
  
  # Render the table which is displayed to the user
  output$expense_table <- DT::renderDataTable({
    DT::datatable(displayed_db(), options = list(lengthMenu = c(5, 10, 20, 50, 100), pageLength = 5), editable = 'cell')
  })
  
  # Render barplot
  output$bp_by_category <- renderPlot({
    barplot_by_category(displayed_db())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
