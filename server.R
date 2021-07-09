library(shiny)
library(data.table)
library(DT)
library(openxlsx)
library(scales)

source("shiny_budget_helpers.R")

# load data
shiny_database <- load_db()

shinyServer(
  function(input, output, session) {
    
    # reactive database for rendering plots, and boolean to show table only when it's non-empty
    values <- reactiveValues(plot_database = shiny_database, 
                             show_table = FALSE, 
                             add_file = 0,
                             default_budget = F)
    
    proxy <- "Set as proxy for item data when table is non-empty"
    
    # Set proxy just once, only if table was previously not shown because it was empty
    observe({
      if (!values$show_table && values$plot_database[,.N] > 0) {
        proxy <<- dataTableProxy("expense_table")
        values$show_table <- TRUE
      }
    })

################################## Data Addition/Changes ##########################################
    
    # Clear inputs on submit. Also add new categories if needed.
    reset_inputs <- function() {
      updateTextInput(session, inputId = "item_name", value = "")
      updateSelectInput(session, inputId = "new_or_existing", selected = "Select Existing")
      updateSelectInput(session, inputId = "category", choices = shiny_database$category)
      updateTextInput(session, inputId = "new_category", value = "")
      updateNumericInput(session, inputId = "price", value = 0)
      updateTextInput(session, inputId = "description", value = "")
    }
    
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
    
    # Submit button for newly entered item
    observeEvent(input$item_button, {
      
      # add new row to displayed database if proxy has already been set
      # otherwise it'll be set when the first row is added to the reactive database
      if (values$show_table) {
        proxy %>% addRow(new_item())
      }
      
      # update databases
      new_row <- as.data.table(new_item())
      shiny_database <<- rbindlist(list(shiny_database, new_row), fill = T)
      values$plot_database <- rbindlist(list(values$plot_database, new_row), fill = T)
      
      # Save database
      saveRDS(shiny_database, "shiny_database.rds")
      
      # clear inputs for a new submission
      reset_inputs()
      
    })
    
    # Stores newly uploaded data 
    new_data_table <- reactive({
      # Stop if no file has been selected
      req(input$file_1)
      
      file <- input$file_1
      
      ext <- substr(file$datapath, nchar(file$datapath)-4, nchar(file$datapath))
      
      validate(
        need(ext == ".xlsx",
             "Invalid file extension. Please upload an Excel file (.xlsx extension).")
        )
      validate(
        need(tryCatch({read.xlsx(file$datapath, sheet = as.integer(input$sheet))}, 
                      error = function(x) {return(F)})
             , "Error loading file, most likely an invalid sheet number")
      )
      
      # read in data 
      new_data <- read.xlsx(file$datapath, sheet = as.integer(input$sheet))
      
      # check that data has the correct columns
      expected_cols <- c("name", "description", "category", "price", "date")
      
      # check that data has proper columns
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
      
      new_rows <- as.data.table(new_data)
      
      # Check data is complete 
      validate(
        need(!any(is.na(new_rows$price)) && is.numeric(new_rows$price), "Price is missing or isn't a number in one or more rows.")
      )
      validate(
        need(!any(is.na(new_rows$category)), "Category is missing in one or more rows.")
      )
      validate(
        need(!any(is.na(new_rows$date)), "Date is missing in one or more rows.")
      )
      validate(
        need(!any(is.na(new_rows$name)), "Name is missing in one or more rows.")
      )
      
      # Remove whitepace around the ends of a category
      new_rows$category <- trimws(new_rows$category) 
      new_rows
    })
   
    # Updates both the front end and back-end databases whenever new data is uploaded  
    observeEvent(new_data_table(), {
      # Update databases
      new_rows <- new_data_table()
      shiny_database <<- rbindlist(list(shiny_database, new_rows), fill = T)
      values$plot_database <- rbindlist(list(values$plot_database, new_rows), fill = T)
      
      # Save database
      saveRDS(shiny_database, "shiny_database.rds")
      
      # Add rows to displayed table
      values$add_file <- values$add_file + 1
    })
    
    # Direct user changes to data table
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
    
    # Stores budget spreadsheet
    budget <- reactive({
      if(is.null(input$file_2)) {
        budget_df <- data.table(category = unique(values$plot_database$category),
                   budget = 0)
        values$default_budget <- T
        } else {
          values$default_budget <- F
          
          # Stop if no file has been selected
          req(input$file_2)
          
          file <- input$file_2
          
          ext <- substr(file$datapath, nchar(file$datapath)-4, nchar(file$datapath))
          
          validate(
            need(ext == ".xlsx",
                 "Invalid file extension. Please upload an Excel file (.xlsx extension).")
            )
          validate(
            need(tryCatch({read.xlsx(file$datapath)}, 
                          error = function(x) {return(F)})
                 , "Error loading file.")
          )
          
          # read in data 
          new_data <- read.xlsx(file$datapath)
          
          # check that data has proper columns
          validate (
            need(colnames(new_data) == c("category", "budget"), 
                 "Looks like the file isn't in the proper format.\nPlease make you have these columns: category, budget.")
          )
          
          new_rows <- as.data.table(new_data)
          
          # Check data is complete 
          validate(
            need(!any(is.na(new_rows$budget)) && is.numeric(new_rows$budget), "Budget is missing or isn't a number in one or more rows.")
          )
          validate(
            need(!any(is.na(new_rows$category)), "Category is missing in one or more rows.")
          )
          
          # Remove whitepace around the ends of a category
          new_rows$category <- trimws(new_rows$category) 
          new_rows  
      }
      
    })
 
    
#################################### Template Downloads ###########################################    
   
    # Download spending tracker template
    output$spending_tracker_template <- downloadHandler(
      filename <- function() {
        "spending_tracker_template.xlsx"
      },
      content <- function(file) {
        temp <- data.table(name = character(),
                   description = character(),
                   category = character(),
                   price = numeric(),
                   date = character())
        write.xlsx(temp, file)
      } 
    )
    
    # Download budget template
    output$budget_template <- downloadHandler(
      filename <- function() {
        "budget_template.xlsx"
      },
      content <- function(file) {
        temp <- data.table(category = unique(values$plot_database$category),
                   budget = 0)
        write.xlsx(temp, file)
      } 
    ) 
      
    
###################################################################################################
##                                                                                               ## 
####################################### Outputs ###################################################
##                                                                                               ## 
###################################################################################################
    
    
####################################### Update Inputs #############################################
    
    # Change choices based on existing categories
    observe({
     updateSelectInput(session, "category", choices = values$plot_database$category)
     updateSelectInput(session, "gp", choices = values$plot_database$category)
     updateSelectInput(session, "yr", choices = year(as.Date(values$plot_database$date, format = "%m/%d/%y")))
    })
    
####################################### Dashboard #################################################
    
    # Render the table which is displayed to the user
    output$expense_table <- DT::renderDataTable({
      
      # Re-render expense table when a new file is uploaded
      re_render <- values$add_file
      
      if (values$show_table) {
        DT::datatable(shiny_database,
                      options = list(lengthMenu = c(5, 10, 20, 50, 100),
                                     pageLength = 5
                      ),
                      rownames = F,
                      editable = 'cell')
      }
    }, server = FALSE)
    
    # Render average per category bar plot
    output$bp_by_category <- renderPlot({
      if (values$show_table) {
        barplot_by_category(values$plot_database, input$yr)
      }
    })
    
    # Render spending over time line plot
    output$monthly_spending<- renderPlot({
      if (values$show_table) {
        monthly_spending(values$plot_database, input$gp, input$yr)
      }
    })
    
    # Render total spending in a given month bar plot
    output$month_totals_plot <- renderPlot({
      if (values$show_table) {
      month_totals_bp(values$plot_database,input$mon, input$yr)
      }
    })

 
####################################### Budget ####################################################
    
    # Render the budget barplot
    output$remaining_budget_plot <- renderPlot({
      monthly_budget_remaining(values$plot_database, budget())
    })
    
####################################### Upload Data ###############################################
    
    # Render a datatable showing the newly uploaded data 
    output$upload_table <- renderTable({
      new_data_table()
    })
    
    # Render a datatable showing the uploaded budget 
    output$budget_upload_table <- renderTable({
      new_budget <- budget()
      if(!values$default_budget) {
        new_budget
      } else {
        return()
      }
        
    })
    
    session$onSessionEnded(function() {
      stopApp()
    })
  }
)