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
library(stringr)
library(ggplot2)
#setwd("C:/Users/Lisa/Documents/GitHub/ibriquet")
source("functions.R")
source("cleaning.R")
theme_update(plot.title = element_text(hjust = 0.5,face = "bold"))

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
      

      
      actionButton("uploadButton", "Ajouter")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel( 
      
      # Output: Data file ----
      #tableOutput("contents")
      box(title = "File Preview", status = "primary",width = "300",solidHeader = T, 
           column(width = 12,
                  DT::dataTableOutput("contents"),style = "height:400px; overflow-y: scroll;overflow-x: scroll;"
           ))
    )
    
  )
)


singleUserPage <- fluidPage(
  titlePanel("Single User"),
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      selectInput("name",
                  "User",
                  c("Select User")),
      htmlOutput(outputId = "infos"),
      tags$hr(),
      sliderInput("weekNumber", "Week Number:",
                  min = 0, max = 4,
                  value = 0)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel( 
      tabsetPanel(
        tabPanel("Weekly Stats", { 
         fluidRow(splitLayout(cellWidths = c("50%","50%"), plotOutput(outputId = "lastWeek"),plotOutput(outputId = "distPlot")))
          }),
        tabPanel("Overall Stats", {
          fluidRow(
            plotOutput(outputId = "progRate"),
            plotOutput(outputId = "userEngagement")
            )
          })
    )
    
  )

  
))
allUsersPage <- fluidPage(
  titlePanel("All Users"),
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      sliderInput("weekNumber2", "Week Number:",
                  min = 0, max = 21,
                  value = 0),
      tags$hr(),
      checkboxGroupInput("type", "Type of record: ",
                         c("Behaviour","Friend","On time","Skipped","Auto skipped","Cheated"),selected = c("Behaviour","Friend","On time","Skipped","Auto skipped","Cheated"))
      
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel( 
      tabsetPanel(
        tabPanel("Overall Stats", { 
          fluidRow(plotOutput(outputId = "dailyConsumption"),
                   plotOutput(outputId = "engagement"),
                   plotOutput(outputId = "progPerAgeBin"))
        }),
        tabPanel("Weekly Stats", {
          fluidRow(
            plotOutput(outputId = "weekTypes")
          )
        })
      )
    )
  )
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
  
  # Used to reset the upload field
  observeEvent(input$uploadButton,{reset("file1");})
 
  # Calls all the functions used to display information
  cleanData <- function(dfLogs,dfSurvey){
    
    # Clean the data frames
    dfLogs <- cleanLogs(dfLogs);
    dfSurvey <- cleanSurvey(dfSurvey);
    
    # Graphs for all the users
    output$dailyConsumption <- dailyCons(dfLogs);
    output$engagement <- engagement(dfLogs);
    output$progPerAgeBin <- progPerAgeBin(dfLogs, dfSurvey);
    
    # Graph per user
    observeEvent(input$name,{
      if(input$name!="Select User"){
        values$maxWeek <- getmaxWeek(dfLogs,input$name)
        updateSliderInput(session,"weekNumber",max=values$maxWeek)
        output$distPlot <- plotFunction(dfLogs,input$name,input$weekNumber)
        output$progRate <- progRate(dfLogs,input$name)
        output$infos <- getInfos(dfSurvey,input$name)
        output$lastWeek <- lastWeekCons(dfLogs,input$name,input$weekNumber)
        output$userEngagement<- userEngagement(dfLogs,input$name)
        
      }
        
    })
    observeEvent(input$weekNumber,{
      if(input$name!="Select User"){
      output$distPlot <- plotFunction(dfLogs,input$name,input$weekNumber)
      output$lastWeek <- lastWeekCons(dfLogs,input$name,input$weekNumber)
      }
    })
    observeEvent({input$type
      input$weekNumber2},{
      output$weekTypes <- weeklyTypes(dfLogs,input$weekNumber2,input$type)
    })
    
    # Fill the select input with the filtered names of users
    updateSelectInput(session, "name",
                      choices = sort(unique(dfLogs$User)),
                      selected = head(sort(unique(dfLogs$User)), 1)
    )
    
    
    
    return(dfLogs)
  }
  
  
  
  # Reactive values used as global updatable values
  values <- reactiveValues(dfLogs = NULL,dfSurvey = NULL,uploaded = 0,maxWeek=0)
  
  
  # Read the input fime depending on the type
  observeEvent(input$file1,{
    tryCatch(
      {
            observeEvent(input$uploadButton,{
              if(file_ext(input$file1$datapath)=="csv"){
              values$dfLogs <- read.csv(input$file1$datapath, header = TRUE,sep = ";", fileEncoding = "MACROMAN")
              output$contents <- DT::renderDataTable(DT::datatable({
                return(values$dfLogs);
              }))
              }
              else{
                values$dfSurvey <- read_excel(input$file1$datapath)
                output$contents <- DT::renderDataTable(DT::datatable({
                  return(values$dfSurvey);
                }))
              }
              values$uploaded = values$uploaded + 1
            })
        
        
      },
      error = function(e) {
        
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )})
  
  
  
  # If both files are uploaded we can clean the data
  observeEvent(values$uploaded,{
    if(values$uploaded == 2)
      cleanData(values$dfLogs,values$dfSurvey)
  })
  
}


shinyApp(ui, server)