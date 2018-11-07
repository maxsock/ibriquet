library(shiny)
library(shinydashboard)
library(readxl)
library(shinyjs)
library(lubridate)
library(data.table)
library(dplyr)
library(plyr)
library(bda)
library(tools)
setwd("/Users/maximiliensock/ibriquet/")
source("test.R")
source("cleaning.R")


options(stringsAsFactors = FALSE)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Upload File", icon = icon("upload"), tabName = "upload"),
    menuItem("All Users", tabName = "allUsers", icon = icon("users")),
    menuItem("Single User", icon = icon("user"), tabName = "singleUser")
  )
)

uploadPage <- fluidPage(
  useShinyjs(),
  
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
      
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      actionButton("uploadButton", "Ajouter")
      
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
              c("Select User")),
  plotOutput(outputId = "distPlot"),
  plotOutput(outputId = "progRate")
)

allUsersPage <- fluidPage(
  titlePanel("All Users"),
  plotOutput(outputId = "dailyConsumption"),
  plotOutput(outputId = "engagement")
)

ui <- dashboardPage(
  dashboardHeader(title = "iBriquet dashboard"),
  sidebar,
  dashboardBody(tabItems(
    tabItem(tabName = "upload",
            uploadPage
    ),
    
    tabItem(tabName = "allUsers",allUsersPage),
    tabItem(tabName = "singleUser",singleUserPage)
  ))
)

server <- function(input, output, session) {
  
  observeEvent(input$uploadButton,{reset("file1");hide("contents")})
  observeEvent(input$type,{
    shinyjs::show("contents")
    if(input$type == "xlsx"){
      hide("sep");
    } else {
      shinyjs::show("sep");
    }})
  
  
  readCSV <- function(dfLogs,dfSurvey){
    dfLogs <- cleanData(dfLogs);
    output$dailyConsumption <- dailyCons(dfLogs);
    output$engagement <- engagement(dfLogs);
    
    observeEvent(input$name,{
      output$distPlot <- plotFunction(dfLogs,input$name)
      output$progRate <- progRate(dfLogs,input$name)
    })
    updateSelectInput(session, "name",
                      choices = sort(dfSurvey$Name),
                      selected = head(sort(dfSurvey$Name), 1)
    )
    
    return(dfLogs)
  }
  values <- reactiveValues(dfLogs = NULL,dfSurvey = NULL,uploaded = 0)
  
  
  
  
  
  
  observeEvent(input$file1,{
    tryCatch(
      {
        if(file_ext(input$file1$datapath)=="csv"){
          isolate({
            print("csv file")
            observeEvent(input$sep,{ print("observe separator"); values$dfLogs <- read.csv(input$file1$datapath,
                                                                                           header = input$header,
                                                                                           sep = input$sep, fileEncoding = "MACROMAN")
            output$contents <- DT::renderDataTable(DT::datatable({
              return(values$dfLogs);
            }))
            })
            observeEvent(input$uploadButton,{print("uploaded csv");
              values$uploaded = values$uploaded + 1})
          })
        } else {
          isolate({
            print("excel")
            values$dfSurvey <- read_excel(input$file1$datapath)
            output$contents <- DT::renderDataTable(DT::datatable({
              return(values$dfSurvey);
            }))
          })
          #  observeEvent(input$uploadButton,{values$uploaded = values$uploaded + 1})
        }
        
      },
      error = function(e) {
        
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )})
  
  observeEvent(values$uploaded,{
    print(values$uploaded)
    if(values$uploaded == 2)
      readCSV(values$dfLogs,values$dfSurvey)
  })
  
  
  
  
  
  
}






shinyApp(ui, server)