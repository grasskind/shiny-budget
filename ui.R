library(shiny)
library(shinyhelper)
library(magrittr)
library(shinydashboard)
library(dashboardthemes)


source("shiny_budget_helpers.R")

DATE_SYSTEM_CHOICES <- get_date_system_choices()

# UI for Shiny Budget App
ui <- dashboardPage(
    
    dashboardHeader(title = "Shiny Budget"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Spending Tracker", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Budget", tabName = "budget_tab", icon = icon("wallet")),
        menuItem("New Purchase", tabName = "new_purchase", icon = icon("receipt")),
        menuItem("Upload Data", tabName = "data_upload", icon = icon("file-upload"))
      )
      
    ),
    dashboardBody(
      shinyDashboardThemes(
        theme = "poor_mans_flatly"
      ),
      tabItems(
        tabItem(tabName = "dashboard",
                fluidRow(
                  column(6,
                         tabBox(
                           selected = "Average Spending",
                           width = "100%",
                           tabPanel("Average Spending", plotOutput("bp_by_category"),
                                    selectInput("yr", label = "Year",choices = c()) 
                                    )
                           )
                  ),
                  column(6,
                         tabBox(
                           selected = "Month Total",
                           width = "100%",
                           tabPanel("Month Total", plotOutput("month_totals_plot"),
                                    selectInput("mon", label = "Month", choices = month.name,
                                                selected = month.name[as.numeric(data.table::month(Sys.Date()))])),
                           tabPanel("Over Time", plotOutput("monthly_spending"),
                                    selectInput("gp", label = "Category",choices = c()))
                         )
                  ) %>% 
                    helper(content = "Tour", style = "padding-right:30px"),
                  fluidRow(
                    column(12,
                           style = "padding-left:30px",
                           tabBox(
                             selected = "Expense Table",
                             width = "100%", 
                             tabPanel("Expense Table", DT::dataTableOutput("expense_table"))
                           ),
                           actionButton("delete_selected", "Delete Selected Rows"),
                           br(),
                           br(),
                           downloadButton("full_expense_table_dw", label = "Download Table")
                           )
                  )
                )),
        tabItem(tabName = "budget_tab", 
                fluidRow(
                  column(6,
                         tabBox(
                           selected = "This Month",
                           width = "100%",
                           tabPanel("This Month", plotOutput("remaining_budget_plot") %>%
                                      helper(content = "Budget")))
                         ),
                  column(6,
                         DT::dataTableOutput("current_budget_table"),
                         splitLayout(
                           cellWidths = c("50%", "50%"),
                           div(
                           actionButton("delete_selected_budget", "Delete Selected Rows"),
                           br(),
                           br(),
                           downloadButton("full_budget_table_dw", label = "Download Table")
                           ),
                           div(
                             textInput("user_add_budget_category", "Category"),
                             numericInput("user_add_budget_price", "Budget", value = 0),
                             actionButton("user_add_budget_category_button", "Add Category"))
                           )
                         )
                )),
        tabItem(tabName = "new_purchase",
                fluidRow(
                  column(6,
                         textInput("item_name", label = "Item Name", placeholder = "Ex. Detective Pikachu"),
                         
                         selectInput("new_or_existing", label = "Category", choices = c("Select From Budget", "Select Existing", "Create New")),
                         
                         conditionalPanel(
                           condition = "input.new_or_existing === 'Select Existing'",
                           selectInput("category", label = "Name", choices = c())
                         ),
                         
                         conditionalPanel(
                           condition = "input.new_or_existing === 'Create New'",
                           textInput("new_category", label = "Name")
                         ),
                         
                         conditionalPanel(
                           condition = "input.new_or_existing === 'Select From Budget'",
                           selectInput("budget_category", label = "Name", choices = c())
                         )
                  ),
                  column(6,
                         numericInput("price", label = "Price", value = 0),
                         
                         textInput("description", label = "Description", placeholder = "Ex. Movie ticket and popcorn"),
                         dateInput("new_item_date", "Date", format = "mm/dd/yy"),
                         br(),
                         h5(""),
                         
                         actionButton("item_button", "Submit")
                  )
                ) %>% 
                    helper(content = "NItem", style = "padding-right:30px"),
                  
        ),
        tabItem(tabName = "data_upload",
                 sidebarLayout(
                   sidebarPanel(
                    h3(tags$b("Spending Tracker")) %>% 
                    helper(content = "Upload"),
                    fileInput("file_1", 
                                    label = "Upload",
                                    accept = c(
                                      ".xlsx"
                                    )),
                    numericInput("sheet", label = "Sheet", value = 1),
                    selectInput("date_format", 
                                label = "Date System",
                                choices = DATE_SYSTEM_CHOICES,
                                selected = "1900"),
                    downloadButton("spending_tracker_template", label = "Download Template"),
                    h3(tags$b("Budget")),
                    fileInput("file_2", 
                                    label = "Upload",
                                    accept = c(
                                      ".xlsx"
                                   )),
                    downloadButton("budget_template", label = "Download Template")
                   ),
                   mainPanel(
                     DT::dataTableOutput("upload_table"),
                     DT::dataTableOutput("budget_upload_table")
                   )
                 ) 
        )
      )
    )
  )