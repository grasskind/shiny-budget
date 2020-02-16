library(shiny)
library(shinydashboard)
library(dashboardthemes)

date_system_choices <- c("1900", "1904")
names(date_system_choices) <- c("1900 (Windows)", "1904 (Mac)")


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
)