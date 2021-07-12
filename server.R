library(shiny)
library(data.table)
library(DT)
library(openxlsx)
library(scales)

source("shiny_budget_helpers.R")

# load data
input_data <- load_db()
shiny_database <- input_data[[1]]
shiny_budget <- input_data[[2]]
shiny_palette <- input_data[[3]]
shiny_color_choices <- input_data[[4]]

shinyServer(
  function(input, output, session) {
    
    # reactive database for rendering plots, and boolean to show table only when it's non-empty
    # plot_database: Stores session database 
    # show_table: Don't show table when there's no data
    # rerender_expense_table: Re-render expense table when rows are deleted or a new data file is uploaded 
    values <- reactiveValues(session_database = shiny_database, 
                             session_budget = shiny_budget,
                             session_color_choices = shiny_color_choices,
                             show_table = FALSE, 
                             rerender_expense_table = 0,
                             rerender_budget_table = 0)
    
    proxy <- "Set as proxy for item data when table is non-empty"
    budget_proxy <- dataTableProxy("current_budget_table")
    
    # Set proxy just once, only if table was previously not shown because it was empty
    observe({
      if (!values$show_table && values$session_database[,.N] > 0) {
        proxy <<- dataTableProxy("expense_table")
        values$show_table <- TRUE
      }
    })

################################## Helper Functions ###############################################

    # Clear inputs on submit. Also add new categories if needed.
    reset_inputs <- function() {
      updateTextInput(session, inputId = "item_name", value = "")
      updateSelectInput(session, inputId = "new_or_existing", selected = "Select From Budget")
      updateSelectInput(session, inputId = "category", choices = isolate(values$session_database$category))
      updateTextInput(session, inputId = "new_category", value = "")
      updateNumericInput(session, inputId = "price", value = 0)
      updateTextInput(session, inputId = "description", value = "")
    }
    
    # Add new category to budget. If the category already exists, does nothing
    add_category_to_budget <- function(cg) {
      if (! cg %in% values$session_budget$category) {
        new_row <- data.table(category = cg, budget = 0) 
        values$session_budget <- rbindlist(list(values$session_budget, new_row))
      }
    }
    
    # Add categories to budget that were spent on this month but not included in the budget
    add_missing_categories_to_budget <- function() {
      # Get current month/year
      mon.index <- as.numeric(data.table::month(Sys.Date()))
      year.index <- as.numeric(data.table::year(Sys.Date()))
      
      # Add month and year columns
      data <- values$session_database
      data$month <- data.table::month(as.Date(data$date, format = "%m/%d/%y"))
      data$yea <- data.table::year(as.Date(data$date, format = "%m/%d/%y"))
       
      # Add current month's categories
      cm_categories <- unique(data$category[data$month == mon.index & data$yea == year.index])
      sapply(cm_categories, add_category_to_budget)
    }
    
    # Add a new category to the color database
    add_category_color <- function(cg) {
      new_row <- data.table(category = cg, col = shiny_palette[1])
      shiny_palette <<- shiny_palette[-1]
      values$session_color_choices <- rbindlist(list(values$session_color_choices, new_row))
    }
    
    # Remove a category that is no longer needed from the color database
    remove_category_color <- function(cg) {
      shiny_palette <<- c(values$session_color_choices$col[values$session_color_choices$category == cg], shiny_palette)
      values$session_color_choices <- values$session_color_choices[values$session_color_choices$category != cg,]
    }
    
################################## Data Addition/Changes ##########################################
    
    # Gather data for newly entered item
    new_item <- reactive({
      date <- format(input$new_item_date, "%m/%d/%y")
      
      # Get month/year
      mon.index <- as.numeric(data.table::month(format(input$new_item_date, "%Y-%m-%d")))
      year.index <- as.numeric(data.table::year(format(input$new_item_date, "%Y-%m-%d")))
      
      if (input$new_or_existing == "Select Existing") {
        category <- input$category
      } else if (input$new_or_existing == "Select From Budget") {
        category <- input$budget_category
      } else {
        category <- input$new_category
      }
      
      # Gather all relevant data
      item_data <- data.frame(input$item_name,
                              input$description,
                              category,
                              input$price,
                              date,
                              mon.index,
                              year.index,
                              stringsAsFactors = F)
      colnames(item_data) <- c("name", "description", "category", "price", "date", "month", "year")
      item_data
    })
    
    # Submit button for newly entered item
    observeEvent(input$item_button, {
      
      # add new row to displayed database if proxy has already been set
      # otherwise it'll be set when the first row is added to the reactive database
      if (values$show_table) {
        proxy %>% addRow(new_item())
      }
      
      # update database
      new_row <- as.data.table(new_item())
      values$session_database <- rbindlist(list(values$session_database, new_row), fill = T)
      
      if (input$new_or_existing %in% c("Create New", "Select Existing")) {
        add_category_to_budget(new_row$category)
      } 
      
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
      
      validate (
        need(tryCatch({
          if (input$date_format == "1900") {
            tmp <- format(as.Date(new_data$date, origin = "1899-12-30"), "%m/%d/%y")
          } else {
            tmp <- format(as.Date(new_data$date, origin = "1904-01-01"), "%m/%d/%y")
          }
        },error = function(x) return(F)),
        "One or more dates were in the wrong format")
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
      
      new_rows$month <- data.table::month(format(as.Date(new_rows$date, format = "%m/%d/%y"), "%Y-%m-%d"))
      new_rows$year <- data.table::year(format(as.Date(new_rows$date, format = "%m/%d/%y"), "%Y-%m-%d"))
      
      # Remove whitepace around the ends of a category
      new_rows$category <- trimws(new_rows$category) 
      new_rows
    })
   
    # Updates database whenever new data is uploaded  
    observeEvent(new_data_table(), {
      
      # Update database
      new_rows <- new_data_table()
      values$session_database <- rbindlist(list(values$session_database, new_rows), fill = T)
      
      # Update budget
      add_missing_categories_to_budget()
      
      # Add rows to displayed table and budget
      values$rerender_expense_table <- values$rerender_expense_table + 1
      values$rerender_budget_table <- values$rerender_budget_table + 1
    })
    
    # Delete selected rows from data table
    observeEvent(input$delete_selected, {
      if (!is.null(input$expense_table_rows_selected)) {
        values$session_database <- values$session_database[-input$expense_table_rows_selected,]
        # Re-render displayed table
        values$rerender_expense_table <- values$rerender_expense_table + 1
      }
      
    })
    
    # Stores newly uploaded budget spreadsheet
    new_budget <- reactive({
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
      
      # Remove whitespace around the ends of a category
      new_rows$category <- trimws(new_rows$category) 
      new_rows  
    })
    
    # Updates budget whenever new data is uploaded
    observeEvent(new_budget(), {
      # Replace old budget with new one
      values$session_budget <- new_budget()
      
      # Add in any categories that were spent on this month but aren't included in the budget
      add_missing_categories_to_budget()
      
      # Re-render displayed budget
      values$rerender_budget_table <- values$rerender_budget_table + 1
    })
    
    # Delete selected rows from budget table 
    observeEvent(input$delete_selected_budget, {
      if (!is.null(input$current_budget_table_rows_selected)) {
        values$session_budget <- values$session_budget[-input$current_budget_table_rows_selected,]
        # Re-render displayed budget
        values$rerender_budget_table <- values$rerender_budget_table + 1
      }
      
    })
    
    # Gather data for newly entered category
    new_budget_category <- reactive({
      # Gather all relevant data
      item_data <- data.frame(input$user_add_budget_category,
                              input$user_add_budget_price,
                              stringsAsFactors = F)      
      colnames(item_data) <- c("category", "budget")
      item_data
    })
    
    # Add new category to budget
    observeEvent(input$user_add_budget_category_button, {
      # Add new row to the displayed budget table
      budget_proxy %>% addRow(new_budget_category())
      
      # Update database
      new_row <- as.data.table(new_budget_category())
      values$session_budget <- rbindlist(list(values$session_budget, new_row), fill = T)
      
      # Reset inputs
      updateTextInput(session, inputId = "user_add_budget_category", value = "")
      updateNumericInput(session, inputId = "user_add_budget_price", value = 0)
    })
    
    # Record user changes to budget
    observeEvent(input$current_budget_table_cell_edit, {
      # Get info about what cell the user changed
      info <- input$current_budget_table_cell_edit
      i <- info$row
      v <- info$value
      value <- as.numeric(v)
      
      # Change the database
      values$session_budget[i,"budget"] <- value
    })
    
    # Add/remove colors from color database whenever budget is updated 
    observeEvent(values$session_budget, {
      # Add categories that are in budget but missing from color database
      categories_to_add <- setdiff(values$session_budget$category, values$session_color_choices$category)
      
      # Remove categories that are in neither the budget nor the expense table
      categories_to_remove <- setdiff(values$session_color_choices$category, 
                                      c(values$session_budget$category, values$session_database$category))
      
      if (length(categories_to_add != 0)) {
        sapply(categories_to_add, add_category_color)
      }
      if (length(categories_to_remove) != 0) {
        sapply(categories_to_remove, remove_category_color)
      }
    })
    
    # Add/remove colors from color database whenever expense table is updated
    observeEvent(values$session_database, {
      # Add categories that are in expense table but missing from color database
      categories_to_add <- setdiff(values$session_database$category, values$session_color_choices$category)
      
      # Remove categories that are in neither the budget nor the expense table
      categories_to_remove <- setdiff(values$session_color_choices$category, 
                                      c(values$session_budget$category, values$session_database$category))
      
      if (length(categories_to_add != 0)) {
        sapply(categories_to_add, add_category_color)
      }
      if (length(categories_to_remove) != 0) {
        sapply(categories_to_remove, remove_category_color)
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
        temp <- data.table(category = unique(values$session_database$category),
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
     updateSelectInput(session, "category", choices = values$session_database$category)
     updateSelectInput(session, "budget_category", choices = values$session_budget$category)
     updateSelectInput(session, "gp", choices = values$session_database$category)
     updateSelectInput(session, "yr", choices = year(as.Date(values$session_database$date, format = "%m/%d/%y")))
    })
    
####################################### Dashboard #################################################
    
    # Render the table which is displayed to the user
    output$expense_table <- DT::renderDataTable({
      
      # Re-render expense table 
      re_render <- values$rerender_expense_table
      
      if (values$show_table) {
        DT::datatable(isolate(values$session_database),
                      options = list(lengthMenu = c(5, 10, 20, 50, 100),
                                     pageLength = 5
                                     #columnDefs = list(list(visible = F, targets = c(5,6)))
                      ),
                      rownames = F,
                      editable = F)
      }
    }, server = FALSE)
    
    # Render average per category bar plot
    output$bp_by_category <- renderPlot({
      if (values$show_table) {
        barplot_by_category(values$session_database, input$yr)
      }
    })
    
    # Render spending over time line plot
    output$monthly_spending<- renderPlot({
      if (values$show_table) {
        monthly_spending(values$session_database, input$gp, input$yr)
      }
    })
    
    # Render total spending in a given month bar plot
    output$month_totals_plot <- renderPlot({
      if (values$show_table) {
      month_totals_bp(values$session_database,input$mon, input$yr)
      }
    })

 
####################################### Budget ####################################################
    
    # Render the budget barplot
    output$remaining_budget_plot <- renderPlot({
      monthly_budget_remaining(values$session_database, values$session_budget)
    })
    
    # Table to Display Budget
    output$current_budget_table <- DT::renderDataTable({
      re_render <- values$rerender_budget_table
      DT::datatable(isolate(values$session_budget),
                    options = list(lengthMenu = c(5, 10, 20, 50, 100),
                                   pageLength = 10
                    ),
                    rownames = F,
                    editable = list(target = 'cell', disable = list(columns = 0))) 
    }, server = FALSE)
    
####################################### Upload Data ###############################################
    
    # Render a datatable showing the newly uploaded data 
    output$upload_table <- renderTable({
      new_data_table()
    })
    
    # Render a datatable showing the uploaded budget 
    output$budget_upload_table <- renderTable({
      new_budget()
    })
    
    session$onSessionEnded(function() {
      # Save data changes from this session
      shiny_database <- isolate(values$session_database)
      shiny_budget <- isolate(values$session_budget)
      shiny_color_choices <- isolate(values$session_color_choices)
      
      saveRDS(shiny_budget, "shiny_budget.rds")
      saveRDS(shiny_database, "shiny_database.rds")
      saveRDS(shiny_palette, "shiny_palette.rds")
      saveRDS(shiny_color_choices, "shiny_color_choices.rds")
      
      stopApp()
    })
  }
)