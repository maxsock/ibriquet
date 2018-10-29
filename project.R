library(shiny)
library(shinydashboard)
library("readxl")
options(stringsAsFactors = FALSE)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Upload File", icon = icon("upload"), tabName = "upload"),
    menuItem("All Users", tabName = "allUsers", icon = icon("users")),
    menuItem("Single User", icon = icon("user"), tabName = "singleUser")
  )
)

uploadPage <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      # Horizontal line ----
      tags$hr(),
     
      radioButtons("type", "File Type",
                   choices = c(csv = "csv",
                               excel = "xlsx"),
                   selected = "csv"),
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ",")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      #tableOutput("contents")
      box( title = "File Preview", status = "primary",width = "300",solidHeader = T, 
           column(width = 12,
                  DT::dataTableOutput("contents"),style = "height:400px; overflow-y: scroll;overflow-x: scroll;"
           ))
    )
    
  )
)


singleUserPage <- fluidPage(
  titlePanel("Single User"),
  selectInput("name",
              "User",
              c("Select User"))
)

ui <- dashboardPage(
  dashboardHeader(title = "iBriquet dashboard"),
  sidebar,
  dashboardBody(tabItems(
    tabItem(tabName = "upload",
          uploadPage
    ),
    
    tabItem(tabName = "allUsers",
            h2("All Users")
    ),
    tabItem(tabName = "singleUser",singleUserPage)
  ))
)

server <- function(input, output, session) {
  
  output$contents <- DT::renderDataTable(DT::datatable({

    req(input$file1)
    
    tryCatch(
      {
        if(input$type == "csv"){
          df1 <- read.csv(input$file1$datapath,
                         header = input$header,
                         sep = input$sep, fileEncoding = "MACROMAN")
          return(head(df1))
        } else {
          df2 <- read_excel(input$file1$datapath)
          updateSelectInput(session, "name",
                            choices = sort(df2$Name),
                            selected = head(sort(df2$Name), 1)
          )
          return(head(df2))
        }
      },
      error = function(e) {
       
         # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    
    
  }))
  
}

shinyApp(ui, server)