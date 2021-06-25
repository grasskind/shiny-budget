library(shiny)
library(shinydashboard)
library(dashboardthemes)

source("shiny_budget_helpers.R")

DATE_SYSTEM_CHOICES <- get_date_system_choices()

# UI for Shiny Budget App
shinyUI(
  dashboardPage(
    
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
        theme = "onenote"
      ),
      tabItems(
        tabItem(tabName = "dashboard",
                fluidRow(
                  column(6,
                         tabBox(
                           selected = "Average Spending",
                           width = "100%",
                           tabPanel("Average Spending", plotOutput("bp_by_category"),
                                    selectInput("yr", label = "Year",choices = c()))
                         )
                  ),
                  column(6,
                         tabBox(
                           selected = "Over Time",
                           width = "100%",
                           tabPanel("Over Time", plotOutput("monthly_spending"),
                                    selectInput("gp", label = "Category",choices = c())),
                           tabPanel("Total", plotOutput("month_totals_plot"),
                                    selectInput("mon", label = "Month", choices = month.name))
                         )
                  ),
                  DT::dataTableOutput("expense_table")
                )),
        tabItem(tabName = "new_purchase",
                fluidRow(
                  column(6,
                         textInput("item_name", label = "Item Name", placeholder = "Ex. Detective Pikachu"),
                         
                         selectInput("new_or_existing", label = "Category", choices = c("Select Existing", "Create New")),
                         
                         conditionalPanel(
                           condition = "input.new_or_existing === 'Select Existing'",
                           selectInput("category", label = "Name", selected = "Categories", choices = c())
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
                          fileInput("file_1", 
                                    label = "Data File",
                                    accept = c(
                                      ".xlsx"
                                    )),
                          numericInput("sheet", label = "Sheet", value = 1),
                          selectInput("date_format", 
                                      label = "Date System",
                                      choices = DATE_SYSTEM_CHOICES,
                                      selected = "1900")
                   ),
                   column(6,
                          tableOutput("upload_table"))
                 )
        )
      )
    )
  )
)