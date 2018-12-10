library(shiny)
library(shinydashboard)
library(readxl)
library(shinyjs)
library(lubridate)
library(data.table)
library(plyr)
library(dplyr)
library(bda)
library(tools)
library(stringr)
library(ggplot2)
#setwd("C:/Users/Lisa/Documents/GitHub/ibriquet")
source("functions.R")
source("cleaning.R")
source("singleUser.R")
source("allUsers.R")

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
panel <- fluidRow( column(4,selectInput("name",
                                        "User",
                                        c("Select User"))))
panelClassic <- fluidRow(
                          column(8,sliderInput("weekNumber", "Week Number:",
                                                min = 0, max = 21,
                                                value = 0)))
info <- fluidRow(valueBoxOutput("category"),
                 valueBoxOutput("savings"),
                 valueBoxOutput("avProg"),
                 valueBoxOutput("avEng"),
                 valueBoxOutput("bRate"),
                 valueBoxOutput("cigWeek"),
                 valueBoxOutput("cigWeekend"),
                 valueBoxOutput("cig"),
                 valueBoxOutput("mostSmoked"))
classic <- fluidRow(box(
                    title = "Cigarette consumption per week"
                    ,status = "primary"
                    ,solidHeader = TRUE 
                    ,plotOutput("cigCons", height = "300px"))
                    ,
                    box(
                      title = "Mean and Std of Cigarette Consumption per weekday"
                      ,status = "primary"
                      ,solidHeader = TRUE 
                      ,plotOutput("meanCigCons", height = "300px")),
                    box(
                      title = "Overall Progress"
                      ,status = "primary"
                      ,solidHeader = TRUE 
                      ,plotOutput("overallProg", height = "300px")),
                    box(
                      title = "Rate of progress"
                      ,status = "primary"
                      ,solidHeader = TRUE 
                      ,plotOutput("progRate", height = "300px"))
                    )
week <-  fluidRow(box(
                  title = "Cigarette per weekday per time slot"
                  ,status = "primary"
                  ,solidHeader = TRUE 
                  ,plotOutput("cigSlot", height = "300px")),
                  box(
                    title = "Comparison of cigarettes consumption between weeks "
                    ,status = "primary"
                    ,solidHeader = TRUE 
                    ,plotOutput("cigComp", height = "300px")),
                  box(
                    title = "Cigarettes consumption per weekday"
                    ,status = "primary"
                    ,solidHeader = TRUE 
                    ,plotOutput("cigPerWeek", height = "300px")),
                  box(
                    title = "Mode usage per week"
                    ,status = "primary"
                    ,solidHeader = TRUE 
                    ,plotOutput("modeWeek", height = "300px"))
                  )
                
engagement <- fluidRow(box(
                        title = "Overall Engagement"
                        ,status = "primary"
                        ,solidHeader = TRUE 
                        ,plotOutput("overallEng", height = "300px")),
                       box(
                         title = "Engagement per day"
                         ,status = "primary"
                         ,solidHeader = TRUE 
                         ,plotOutput("engDay", height = "300px")))

allDays <- fluidRow(box(
                  title = "Mode Usage over all period"
                  ,status = "primary"
                  ,solidHeader = TRUE 
                  ,plotOutput("overallMode", height = "300px")),
                  box(
                    title = "Cigarettes consumption over all period"
                    ,status = "primary"
                    ,solidHeader = TRUE 
                    ,plotOutput("overallCons", height = "300px")))

singleUserPage <- fluidPage(
  fluidRow(
  box(width = "100px",panel),
  tabBox(width = "100%",
      tabPanel("Information",info),
      tabPanel("Classic",classic),
      tabPanel("Week",panelClassic,week),
      tabPanel("Engagement",engagement),
      tabPanel("All Days",allDays)
      
  
  ))
 )

allUsersInfo <- fluidRow(valueBoxOutput("totCigSaved"),
                 valueBoxOutput("totAvgCig"))


allUsersClassic <- fluidRow(box(
                            title = "Average progress of all users"
                            ,status = "primary"
                            ,solidHeader = TRUE 
                            ,plotOutput("totAvgProg", height = "300px")),
                            box(
                              title = "Mean and std of cigarette consumption per weekday "
                              ,status = "primary"
                              ,solidHeader = TRUE 
                              ,plotOutput("totCigCons", height = "300px")),
                              box(
                                title = "Cigarettes per weekday per time slots "
                                ,status = "primary"
                                ,solidHeader = TRUE 
                                ,plotOutput("totCigSlot", height = "300px")),
                            box(
                              title = "Average rate of progress of all users "
                              ,status = "primary"
                              ,solidHeader = TRUE 
                              ,plotOutput("totProgRate", height = "300px"))
)

allUsersEngagement <- fluidRow(box(
                              title = "Engagement over all period"
                              ,status = "primary"
                              ,solidHeader = TRUE 
                              ,plotOutput("totEng", height = "300px")))

allUsersPage <- fluidPage(
  fluidRow(
    tabBox(width = "100%",
           tabPanel("Information",allUsersInfo),
           tabPanel("Classic",allUsersClassic),
           tabPanel("Engagement",allUsersEngagement)
    ))
)

ui <- dashboardPage(
  dashboardHeader(title = "iBriquet dashboard"),
  sidebar,
  dashboardBody(tabItems(
    tabItem(tabName = "upload",uploadPage),
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
    
    
    output$totCigSaved <- totCigSaved(dfLogs)
    output$totAvgCig <- avgCig(dfLogs)
    output$totAvgProg <- totAvgProg(dfLogs)
    output$totCigCons <- totCigCons(dfLogs)
    output$totCigSlot <- totCigSlot(dfLogs)
    output$totProgRate <- totProgRate(dfLogs)
    output$totEng <- totEng(dfLogs)
  
    # Graph per user
    observeEvent(input$name,{
      if(input$name!="Select User"){
        updateSliderInput(session,"weekNumber",value=0)
        output$category <- ageCat(dfLogs,dfSurvey,input$name)
        output$savings <- cigSaved(dfLogs,input$name)
        output$avProg <- averageProgress(dfLogs,input$name)
        output$avEng <- averageEngagement(dfLogs,input$name)
        output$bRate <- bestRate(dfLogs,input$name)
        output$cigWeek <- averageCons(dfLogs,input$name,"week")
        output$cigWeekend <- averageCons(dfLogs,input$name,"weekend")
        output$cig <- averageCons(dfLogs,input$name," ")
        output$mostSmoked <- mostSmoked(dfLogs,input$name) 
        output$overallProg <- overallProgress(dfLogs,input$name) 
        output$progRate <- progRate(dfLogs,input$name)
        output$meanCigCons <- meanCigCons(dfLogs,input$name)
        output$cigCons <- cigCons(dfLogs,input$name)
        output$cigComp <- cigWeekComp(dfLogs,input$name)
        output$overallEng <- overallEngagement(dfLogs,input$name)
        output$engDay <- engDay(dfLogs,input$name)
        output$overallMode <- overallMode(dfLogs,input$name)
        output$overallCons <- overallCons(dfLogs,input$name)
        
      }
        
    })
    observeEvent({input$weekNumber
                 input$name},{
      if(input$name!="Select User"){
        values$maxWeek <- getmaxWeek(dfLogs,input$name)
        updateSliderInput(session,"weekNumber",max=values$maxWeek)
        output$cigSlot <- cigSlot(dfLogs,input$weekNumber,input$name)
        output$cigPerWeek <- cigWeek(dfLogs,input$weekNumber,input$name)
        output$modeWeek <- modeWeek(dfLogs,input$weekNumber,input$name)
      }
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