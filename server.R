library(shiny)
library(shinyhelper)
library(data.table)
library(DT)
library(openxlsx)

source("shiny_budget_helpers.R")

# loads all input data (spending database, budget, color database, and color palette)
input_data <- load_db()
shiny_database <- input_data[[1]]
shiny_budget <- input_data[[2]]
shiny_palette <- input_data[[3]]
shiny_color_choices <- input_data[[4]]

server <- function(input, output, session) {
    # Handler for help messages
    observe_helpers(help_dir = "help_messages")
  
    # session_database: Stores expenses table
    # session_budget: Categories and allotted budget dataframe
    # session_color_choices: Categories and corresponding colors
    # show_table: Don't show table when there's no data
    # rerender_expense_table: Re-render expense table when rows are deleted or a new data file is uploaded 
    #   Used to stop table from re-rendering when it's unnecessary 
    # rerender_budget_table: Analagous, used for budget table
    values <- reactiveValues(session_database = shiny_database, 
                             session_budget = shiny_budget,
                             session_color_choices = shiny_color_choices,
                             show_table = FALSE, 
                             rerender_expense_table = 0,
                             rerender_budget_table = 0)
    
    # Used to seamlessly update table (without re-rendering the whole thing) 
    # When possible
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

    # Clear inputs for new item on submit. Also adds new categories if needed.
    reset_inputs <- function() {
      updateTextInput(session, inputId = "item_name", value = "")
      updateSelectInput(session, inputId = "new_or_existing", selected = "Select From Budget")
      updateSelectInput(session, inputId = "category", choices = isolate(values$session_database$category))
      updateTextInput(session, inputId = "new_category", value = "")
      updateNumericInput(session, inputId = "price", value = 0)
      updateTextInput(session, inputId = "description", value = "")
      updateDateInput(session, inputId = "new_item_date", value = Sys.Date())
    }
    
    # Add new category to budget. If the category already exists, does nothing
    # Also causes budget table to re-render
    add_category_to_budget <- function(cg) {
      if (! cg %in% values$session_budget$category) {
        new_row <- data.table(category = cg, budget = 0) 
        values$session_budget <- rbindlist(list(values$session_budget, new_row))
        values$rerender_budget_table <- values$rerender_budget_table  + 1 
      }
    }
    
    # Add categories to budget that were spent on this month but not included in the budget
    add_missing_categories_to_budget <- function() {
      # Get current month/year
      mon.index <- as.numeric(data.table::month(Sys.Date()))
      year.index <- as.numeric(data.table::year(Sys.Date()))
      
      data <- values$session_database
       
      # Add current month's categories
      cm_categories <- unique(data$category[data$month == mon.index & data$year == year.index])
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
      # Add color back to the palette for future use
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
      
      # Get category of new item
      if (input$new_or_existing == "Select Existing") {
        category <- input$category
      } else if (input$new_or_existing == "Select From Budget") {
        category <- input$budget_category
      } else {
        category <- trimws(input$new_category)
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
    
    # Add item to database on submit 
    observeEvent(input$item_button, {
      
      # Add new row to displayed database if proxy has already been set
      # Otherwise it'll be set when the first row is added to the reactive database
      if (values$show_table) {
        # Display month name instead of number
        tmp_new_item <- new_item()
        tmp_new_item$month <- month.name[tmp_new_item$month]
        proxy %>% addRow(tmp_new_item, resetPaging = F)
      }
      
      # Update database
      new_row <- as.data.table(new_item())
      values$session_database <- rbindlist(list(values$session_database, new_row), fill = T)
      
      # Check if a new category needs to be added to the budget
      if (input$new_or_existing %in% c("Create New", "Select Existing")) {
        # Get current month/year
        mon.index <- as.numeric(data.table::month(Sys.Date()))
        year.index <- as.numeric(data.table::year(Sys.Date()))
        
        # Add category to budget only if the new item is from this year/month
        if(new_row$month == mon.index && new_row$year == year.index) {
          add_category_to_budget(new_row$category)
        }
      } 
      
      # clear inputs for a new submission
      reset_inputs()
      
    })
    
    # Stores newly uploaded data 
    new_data_table <- reactive({
      # Stop if no file has been selected
      req(input$file_1)
      
      file <- input$file_1
      
      # Get file extension
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
      
      # Read in data 
      new_data <- read.xlsx(file$datapath, sheet = as.integer(input$sheet))
      
      # Check that data has the correct columns
      expected_cols <- c("name", "description", "category", "price", "date")
      
      # Check that data has proper columns
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
      
      # Convert date from excel format to R
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
      
      # Add month & year columns
      new_rows$month <- data.table::month(format(as.Date(new_rows$date, format = "%m/%d/%y"), "%Y-%m-%d"))
      new_rows$year <- data.table::year(format(as.Date(new_rows$date, format = "%m/%d/%y"), "%Y-%m-%d"))
      
      # Remove whitepace around the ends of categories 
      new_rows$category <- trimws(new_rows$category) 
      new_rows
    })
   
    # Update database and budget whenever new data is uploaded  
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
      
      # Read in data 
      new_data <- read.xlsx(file$datapath)
      
      # Check that data has proper columns
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
      
      # Remove whitespace around the ends of categories
      new_rows$category <- trimws(new_rows$category) 
      new_rows  
    })
    
    # Update budget whenever new data is uploaded
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
      item_data <- data.frame(trimws(input$user_add_budget_category),
                              input$user_add_budget_price,
                              stringsAsFactors = F)      
      colnames(item_data) <- c("category", "budget")
      item_data
    })
    
    # Add new category to budget
    observeEvent(input$user_add_budget_category_button, {
      # If category is already in budget, acts as a budgeted amount update
      if (input$user_add_budget_category %in% values$session_budget$category) {
        values$session_budget[category == input$user_add_budget_category, "budget"] <- input$user_add_budget_price
        values$rerender_budget_table <- values$rerender_budget_table + 1
      } else {
        # Add new row to the displayed budget table
        budget_proxy %>% addRow(new_budget_category(), resetPaging = F)
        
        # Update database
        new_row <- as.data.table(new_budget_category())
        values$session_budget <- rbindlist(list(values$session_budget, new_row), fill = T)  
      }
      
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
    
#################################### Downloads ####################################################    
   
    # Download full expense table 
    output$full_expense_table_dw <- downloadHandler(
      filename <- function() {
        "expense_table.xlsx"
      },
      content <- function(file) {
        df <- values$session_database[,-c(6,7)]
        df$date <- as.Date(df$date, format = "%m/%d/%y")
        write.xlsx(df, file)
      } 
    )
    
    # Download budget table 
    output$full_budget_table_dw <- downloadHandler(
      filename <- function() {
        "budget.xlsx"
      },
      content <- function(file) {
        write.xlsx(values$session_budget, file)
      } 
    )
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
        temp <- data.table(category = "Category Name", 
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
     updateSelectInput(session, "gp", choices = c("Total", values$session_database$category))
     updateSelectInput(session, "yr", choices = year(as.Date(values$session_database$date, format = "%m/%d/%y")),
                       selected = max(as.numeric(year(as.Date(values$session_database$date, format = "%m/%d/%y")))))
    })
    
####################################### Dashboard #################################################
    
    # Expense table 
    output$expense_table <- DT::renderDataTable({
      
      # Re-render expense table 
      re_render <- values$rerender_expense_table
      
      if (values$show_table) {
        # Convert month index to name
        df = isolate(values$session_database)
        df$month <- sapply(df$month, function(x) month.name[x], USE.NAMES = F)
        DT::datatable(df,
                      options = list(lengthMenu = c(5, 10, 20, 50, 100),
                                     pageLength = 5
                                     #columnDefs = list(list(visible = F, targets = c(4)))
                      ),
                      rownames = F,
                      editable = F)
      }
    }, server = FALSE)
    
    # Average per category bar plot
    output$bp_by_category <- renderPlot({
      # Don't render before year has a chance to update
      if (values$show_table && !is.na(as.numeric(input$yr))) {
        barplot_by_category(values$session_database, as.numeric(input$yr), values$session_color_choices)
      }
    })
    
    # Spending over time line plot
    output$monthly_spending<- renderPlot({
      # Don't render before year has a chance to update
      if (values$show_table && !is.na(as.numeric(input$yr))) {
        monthly_spending(values$session_database, input$gp, as.numeric(input$yr))
      }
    })
    
    # Total spending in a given month bar plot
    output$month_totals_plot <- renderPlot({
      # Don't render before year has a chance to update
      if (values$show_table && !is.na(as.numeric(input$yr))) {
        month_totals_bp(values$session_database,input$mon, as.numeric(input$yr), values$session_color_choices)
      }
    })

 
####################################### Budget ####################################################
    
    # Budget barplot
    output$remaining_budget_plot <- renderPlot({
      monthly_budget_remaining(values$session_database, values$session_budget, values$session_color_choices)
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
    
    # Datatable showing the newly uploaded data 
    output$upload_table<- DT::renderDataTable({
      df = new_data_table() 
      df$month <- sapply(df$month, function(x) month.name[x], USE.NAMES = F)
      DT::datatable(df,
                    options = list(lengthMenu = c(5, 10, 20, 50, 100),
                                   pageLength = 5,
                                   columnDefs = list(list(visible = F, targets = c(4)))
                    ),
                    rownames = F,
                    editable = F) 
    }, server = FALSE)    
    
    # Datatable showing the uploaded budget 
    output$budget_upload_table <- DT::renderDataTable({
      DT::datatable(new_budget(),
                    options = list(lengthMenu = c(5, 10, 20, 50, 100),
                                   pageLength = 5
                    ),
                    rownames = F,
                    editable = F) 
    }, server = FALSE)    
    
    # Save data for next session
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
