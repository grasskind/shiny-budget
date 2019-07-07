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
library(scales)
library(dashboardthemes)

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
      shinyDashboardThemes(
        theme = "blue_gradient"
      ),
      tabItems(
        tabItem(tabName = "dashboard",
                DT::dataTableOutput("expense_table"),
                fluidRow(
                  column(6,
                         plotOutput("bp_by_category")
                  ),
                  column(6,
                         tabBox(
                           selected = "Week",
                           width = "100%",
                           tabPanel("Week", plotOutput("past_week", height = "350px")),
                           tabPanel("Month", plotOutput("past_month", height = "350px")),
                           tabPanel("Custom Range",
                                    plotOutput("custom_range_spending", height = "350px"),
                                    dateInput("start_date", "Start Date", value = Sys.Date() - 6,
                                              format = "yyyy-mm-dd"),
                                    dateInput("end_date", "End Date", value = Sys.Date(),
                                              format = "yyyy-mm-dd"))
                         )
                  )
                )),
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
  # reactive database for rendering plots, and boolean to show table only when it's non-empty
  values <- reactiveValues(plot_database = shiny_database, show_table = FALSE, previous_file = "_NA_", add_file = 0)
  
  proxy <- "Set as proxy for item data when table is non-empty"
  
  # Set proxy just once, only if table was previously not shown because it was empty
  observe({
    if (!values$show_table && values$plot_database[,.N] > 0) {
      proxy <<- dataTableProxy("expense_table")
      values$show_table <- TRUE
    }
  })
  
  # Submit button for newly entered item
  observeEvent(input$item_button, {
    
    # add new row to displayed database if proxy has already been set
    # otherwise it'll be set when the first row is added to the reactive database
    if (values$show_table) {
      proxy %>% addRow(new_item())
    }
    
    # update databases
    new_row <- as.data.table(new_item())
    shiny_database <<- rbind(shiny_database, new_row, stringsAsFactors = F)
    values$plot_database <- rbind(values$plot_database, new_row, stringsAsFactors = F)
    
    # Save database
    saveRDS(shiny_database, "shiny_database.rds")
    
    # clear inputs for a new submission
    reset_inputs()
    
  })
  
  observeEvent(input$expense_table_cell_edit, {
    # Get info about what cell the user changed
    info <- input$expense_table_cell_edit
    i <- info$row
    j <- info$col
    v <- info$value
    # Convert input value to the proper type
    colname <- colnames(shiny_database)[j + 1]
    coltype <- eval(parse(text = paste0("class(","shiny_database$", colname,")")))
    value <- eval(parse(text = paste0("as.", coltype, "(v)")))
    
    # Change the databases
    shiny_database[i,c(j+1)] <<- value
    values$plot_database[i,c(j+1)] <- value
    # Save changes
    saveRDS(shiny_database, "shiny_database.rds")
  })
  
  
  ############################### Functions ###################################
  
  # update database with new data either from file upload or new item submit
  update_db <- function(input_db) {
    # Add new row to database
    shiny_database <<- rbind(shiny_database, input_db)
    
    # Save database
    saveRDS(shiny_database, "shiny_database.rds")
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
  
  # Gather data for newly entered item
  new_item <- reactive({
    date <- format(Sys.Date(), "%m/%d/%y")
    if (input$new_or_existing == "Select Existing") {
      # Gather all relevant data (get category from dropdown menu)
      item_data <- data.frame(input$item_name, input$description, input$category, input$price, date, stringsAsFactors = F)
    } else {
      # Gather all relevant data (get category from text input)
      item_data <- data.frame(input$item_name, input$description, input$new_category, input$price, date, stringsAsFactors = F)
    }
    colnames(item_data) <- c("name", "description", "category", "price", "date")
    item_data
  })
  
  # Contents of the uploaded file. Displays error message if file is invalid
  observe({
    
    # get file metadata (size, path to file, etc)
    file_metadata <- input$file_metadata
    
    validate(
      need(file_metadata != values$previous_file, "")
    )
    
    # set to prevent file from being uploaded in a never-ending loop
    values$previous_file <- file_metadata

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
    
    # Update databases
    new_rows <- as.data.table(new_data)
    shiny_database <<- rbind(shiny_database, new_rows, stringsAsFactors = F)
    values$plot_database <- rbind(values$plot_database, new_rows, stringsAsFactors = F)
    
    # Save database
    saveRDS(shiny_database, "shiny_database.rds")
    # Add rows to displayed table
    values$add_file <- values$add_file + 1
  })
  
  ############################### Outputs #####################################
  
  # Render the table which is displayed to the user
  output$expense_table <- DT::renderDataTable({
    rerender <- values$add_file
    if (values$show_table) {
      DT::datatable(shiny_database,
                    options = list(lengthMenu = c(5, 10, 20, 50, 100),
                                   pageLength = 5
                                   ),
                    rownames = F,
                    editable = 'cell')
    }
  }, server = FALSE)
  
  # Render barplot
  output$bp_by_category <- renderPlot({
    if (values$show_table) {
      barplot_by_category(values$plot_database)
    }
  })
  
  # Render spending over time plots (one for past week, month, or year, depending on the selected tab)
  output$past_week<- renderPlot({
    if (values$show_table) {
      week_spending(values$plot_database)
    }
  })
  
  output$past_month<- renderPlot({
    if (values$show_table) {
      month_spending(values$plot_database)
    }
  })
  
  output$custom_range_spending<- renderPlot({
    if (values$show_table) {
      custom_range_spending(values$plot_database, input$start_date, input$end_date)
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
