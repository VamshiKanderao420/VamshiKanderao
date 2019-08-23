
# clear the list
rm(list = ls())

# libraries required

library(shiny)
library(RSQLite)
library(shinythemes)
library(shinydashboard)
library(DT)
library(DBI)
library(dbplyr)
library(dplyr)
library(tidyverse)
library(ggplot2)

Logged = FALSE;

ui2 <- function(){tagList(tabPanel("Login"))}

ui <- fluidPage(theme = shinytheme("journal"),
                navbarPage(id = "mainpage4",
                           strong("Capstone Project",align = "center"),
                           
                           
                           navlistPanel( id = "Navp", widths =c(2, 10),
                                         
                                         tabPanel(title = "Main Menu", id = "Home3",
                                                  strong("This is where the lab data is displayed"),"                  Welcome to the Capstone project home page , please login/register to access dashboard and other features "
                                                  
                                                  
                                                  
                                                  
                                         ),
                                         
                                         tabPanel(
                                           title = "Sign in", 
                                           
                                           
                                           tagList(
                                             div(id = "Sign in",
                                                 wellPanel(textInput("userName", "Username"),
                                                           passwordInput("passwd", "Password"),
                                                           br(),actionButton("Signin", "sign in"))),
                                             tags$style(type="text/css", "signin {font-size:10px;   text-align: center;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
                                           )
                                           
                                           
                                         ),
                                         
                                         
                                         tabPanel(title = "Sign Up", 
                                                  h1(strong("Sign Up:")),
                                                  
                                                  
                                                  
                                                  tagList(
                                                    div(id = "NewUser",
                                                        wellPanel(
                                                          textInput('Name', 'Full Name:', width = '100%', placeholder = "Enter your name"),
                                                          textInput ('Role', 'Role:', "submitter", width = '100%'),
                                                          textInput('CustID', 'User ID:', width = '100%', placeholder = "Enter User ID"),
                                                          passwordInput('Password', 'Password:', width = '100%'),
                                                          br(),
                                                          actionButton("submit", "Submit"),
                                                          actionButton("cancel", "Cancel")
                                                        )
                                                    ),
                                                    tags$style(type="text/css", "login {font-size:12px;   text-align: center;position:absolute;top: 40%;left: 20%;margin-top: -100px;margin-left: -150px;}")
                                                  )
                                                  
                                         ),
                                         
                                         tabPanel(title = "Submitter", 
                                                  h1(strong("Order your Test")),
                                                  
                                                  tagList(
                                                    div(id = "Submitter",
                                                        wellPanel(textInput("CustID", "Submitter ID:"),
                                                                  selectInput("gender", "Gender:",c("Male", "Female","Transgender")),
                                                                  dateInput("RequestDate", "Request Date", format = "yyyy-mm-dd"),
                                                                  htmlOutput("TestName"),
                                                                  htmlOutput("LabLocation"),
                                                                  br(),actionButton("order", "Request"))),
                                                    tags$style(type="text/css", "login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
                                                  )
                                                  
                                                  
                                                  
                                         ),
                                         
                                         ############ Analyst page page UI
                                         
                                         tabPanel(title = "Analyst", 
                                                  h1(strong("Analyst Page")),
                                                  
                                                  navbarPage("", id = "analystpage",
                                                             
                                                             
                                                             frow1 <- fluidRow(                
                                                               
                                                               title = "Test Results", width = "1000px"
                                                               ,status = "primary"
                                                               ,solidHeader = TRUE 
                                                               ,collapsible = TRUE 
                                                               ,DTOutput("Results", height = "300px", width = "1000px")
                                                               ,actionButton("action", label = "Write to DB")
                                                             ),      
                                                             
                                                             
                                                             
                                                             
                                                             tabPanel("Add New Test Types", id= "testtypes",
                                                                      
                                                                      tagList(
                                                                        div(id = "TestTypes", br(), br(),
                                                                            wellPanel(
                                                                              
                                                                              br(),
                                                                              textInput ("TestName", "Test Name:"),
                                                                              
                                                                              br(),actionButton("save", "Save"))),
                                                                        tags$style(type="text/css", "login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
                                                                      )          
                                                             )
                                                  )
                                                  
                                         ),     #Analyst tab end
                                         
                                         
                                         ##Dashboard - UI Page
                                         
                                         
                                         tabPanel(title = "Dash board", 
                                                  
                                                  dashboardPage(
                                                    
                                                    dashboardHeader(),
                                                    dashboardSidebar(
                                                      
                                                      selectInput("LabLoc", "Lab Location:", choices = c("Mercy Hospital",     "Wash U school of medicine"),strong("Please Login/register to access dashboard and other features")),
                                                      
                                                      checkboxGroupInput("Ttype", label = h3("Test Type"), 
                                                                         choices = list("Lipid Profile" , "Glucose"),selected = 'Glucose'),
                                                      
                                                      fluidRow(column(2, verbatimTextOutput("Ttype"))),
                                                      
                                                      fluidRow(
                                                        column(3,
                                                               checkboxGroupInput("Sex", label = h3("Gender"), 
                                                                                  choices = list("Male" , "Female"),selected = 'Female')
                                                               
                                                               
                                                        )),
                                                      fluidRow(
                                                        column(3, verbatimTextOutput("Sex"))
                                                      )
                                                      
                                                      
                                                    ),
                                                    dashboardBody(
                                                      fluidRow(valueBoxOutput("value1")     
                                                      )
                                                      
                                                      ,plotOutput("distPie")
                                                      
                                                      ,fluidRow( 
                                                        box(
                                                          title = "Max Cholestrol or Diabetic"
                                                          ,status = "primary"
                                                          ,solidHeader = TRUE 
                                                          ,collapsible = TRUE 
                                                          ,plotOutput("MaxTestResultsbyType", height = "300px")
                                                        )
                                                        
                                                        ,box(
                                                          title = "Test Results by Customer"
                                                          ,status = "primary"
                                                          ,solidHeader = TRUE 
                                                          ,collapsible = TRUE 
                                                          ,plotOutput("TestResultsPerCustomer", height = "300px")
                                                        ) 
                                                        
                                                      )
                                                      
                                                      
                                                    )
                                                  )
                                                  
                                                  
                                         )   ##### Dashboard Page UI   end
                                         
                                         
                           )
                           
                ), 
                
                
                uiOutput("page")
                
)



sqlitePath <- "C:/Users/18127/Documents/Capstonefinaldb/data.sqlite"



#Defining the feilds from new user registration

NewUserRegistration <- c("Password","Name","CustID","Role")
NewTestTypes <- c("TestName")
NewTestOrder <- c("CustID","gender", "RequestDate","TestName","LabLocation", "Test1Std")





server <- function(input, output, session) {
  
  
  ############### New user  ################
  
  # When the form data for new USERS 
  
  formData <- reactive({
    
    data <- sapply(NewUserRegistration, function(x) input[[x]])
    data
  })
  
  # When the Submit button is used, save the New User data to USERS table
  table <- "USERS"
  
  observeEvent(input$submit, {
    
    saveData(formData())
    
  })
  
  # Save query for New user
  
  saveData <- function(data) {
    # Connect to the database
    db <- dbConnect(SQLite(), sqlitePath)
    
    # create the update query by creating a loop over the data fields
    query <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      table, 
      paste(names(data), collapse = ", "),
      paste(data, collapse = "', '")
    )
    # Submit the update query and disconnect
    dbGetQuery(db, query)
    dbDisconnect(db)
  }
  
  
  
  # When the form data for new tests 
  
  newTestFormData <- reactive({
    
    newtestdata <- sapply(NewTestTypes, function(x) input[[x]])
    newtestdata
  })
  
  # When the Save button is used in Add test Types in admin page, save to TestTypes
  table1 <- "TestTypes"
  
  observeEvent(input$save, {
    
    saveTestData(newTestFormData())
    
  })
  
  
  ####### Save query for New tests
  
  saveTestData <- function(newtestdata) {
    # Connect to the database
    db <- dbConnect(SQLite(), sqlitePath)
    
    # Construct the update query by looping over the data fields
    query <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      table1, 
      paste(names(newtestdata), collapse = ", "),
      paste(newtestdata, collapse = "', '")
    )
    # Submit the update query and disconnect
    dbGetQuery(db, query)
    dbDisconnect(db)
  }
  
  
  ################ New Test Orders    ######################
  
  
  # When the form data for test order
  
  TestOrderFormData <- reactive({
    
    orderdata <- sapply(NewTestOrder, function(x) input[[x]])
    
    if (orderdata$TestName == "Glucose") {
      
      orderdata$Test1 <- "Fasting"
      orderdata$Test1Std <- "60 - 100 mg/dL"
      
      orderdata$Test2 <- "Post-2Hrs"
      orderdata$Test2Std <- "120 - 180 mg/dL"
      
    }
    
    if (orderdata$TestName == "Lipid Profile") {
      
      orderdata$Test1 <- "Cholesterol"
      orderdata$Test1Std <- "<200 mg/dL"
      
      orderdata$Test2 <- "Triglycerides"
      orderdata$Test2Std <- "<150 mg/dL"
      
      orderdata$Test3 <- "HDL Cholesterol"
      orderdata$Test3Std <- ">40 mg/dL"
      
      orderdata$Test4 <- "LDL Calculated"
      orderdata$Test4Std <- "<130 mg/dL"
    }
    
    orderdata
  })
  
  # When the order button is clicked, save the test order form data to TestResults table
  
  ordertable <- "TestResults"
  
  observeEvent(input$order, {
    
    saveOrderData(TestOrderFormData())
    
  })
  
  ####### Save query for test order 
  
  saveOrderData <- function(orderdata) {
    # Connect to the database
    db <- dbConnect(SQLite(), sqlitePath)
    
    # Construct the update query by looping over the data fields
    query <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      ordertable, 
      paste(names(orderdata), collapse = ", "),
      paste(orderdata, collapse = "', '")
    )
    # Submit the update query and disconnect
    dbGetQuery(db, query)
    dbDisconnect(db)
  }
  
  
  
  #Cancel option
  
  observeEvent(input$cancel, {
    
    updateTextInput(session, "Name", value = '')
    updateTextInput(session, "CustID", value = '')
    updateTextInput(session, "Password", value = '')
  })
  
  #Log in validation 
  
  USER <- reactiveValues(Logged = Logged)
  
  inputdata <- reactive({
    
    validate(need(isolate(input$userName) == "", "Please Enter User name"))
    
  })
  
  
  
  #User Login
  
  observeEvent(input$Login, { 
    
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          
          
          query <- sprintf({"
            SELECT CustID, Role
            FROM USERS
            WHERE CustID ='%s' and Password ='%s'"},
                           Username, Password, serialize=F) 
          
          db <- dbConnect(SQLite(), sqlitePath)
          userrec <- dbGetQuery(db, query) 
          print(userrec)
          
          dbDisconnect(db)
          
          
          if (length(userrec$CustID) == 0 ) {
            
            # print error/ warning message
            
            
          } else {
            
            if ( userrec$CustID == Username ) {
              
              USER$Logged <- TRUE}
            
          }
          } 
        }
      }    
    
    print(userrec$CustID)
    
    
    if (USER$Logged == TRUE) 
    {
      
      updateTextInput(session, "userName", value = '')
      updateTextInput(session, "passwd", value = '')
      
      USER$Logged <- FALSE
      
      if ( userrec$Role == "analyst" ) { updateNavlistPanel(session, "Navp", selected = "Analyst")}
      if ( userrec$Role == "customer" ) {updateNavlistPanel(session, "Navp", selected = "Customer")}
      
    }
  })
  
  #####################Loaddata function 
  
  loadData <- function(fields, table, sortCol= '' , whereCls = ''){
    if (whereCls == "")
      query <- sprintf("SELECT %s FROM %s", fields, table)
    else
      query <- sprintf("SELECT %s FROM %s WHERE %s", fields, table, whereCls)
    db <- dbConnect(SQLite(), sqlitePath)
    dataDB <- dbGetQuery(db, query)
    
    if(sortCol != "") dataDB[order(dataDB[sortCol]),] 
    else dataDB
    dbDisconnect(db)
    
    print(dataDB)
  }
  
  # load Test Name in Customer tab
  
  Listdata <- loadData("TestName", "TestTypes","TestName","")
  print(Listdata)
  
  #Listdata<- rbind(data.frame("TestName" = ""), Listdata)
  
  Testnamelist <- setNames(Listdata$TestName, Listdata$TestName)
  
  output$TestName <- renderUI({
    selectInput("TestName", "Test Name: ", Testnamelist)
  })
  
  #Lab Locations load data from database
  
  Listdata1 <- loadData("LabLocation", "Location","LabLocation","")
  print(Listdata1)
  
  LabLoclist <- setNames(Listdata1$LabLocation, Listdata1$LabLocation)
  
  output$LabLocation <- renderUI({
    selectInput("LabLocation", "Lab: ", LabLoclist)
  })
  
  
  # Data Table
  
  db <- dbConnect(SQLite(), sqlitePath)
  
  datatb <- tbl(db, "TestResults")
  
  datatb <- datatb %>% as.data.frame()
  
  
  TestResults <- filter(datatb,  (is.na(TestResults) | TestResults ==""))
  
  
  
  output$Results <- renderDT(TestResults, options = 
                               list(scrollX = TRUE), editable = TRUE)
  
  
  #necessary code to replace data once edited
  proxy1 = dataTableProxy('Results')
  
  print(proxy1)
  
  
  TestResults_rows <- which(TestResults$TestName != "" | is.na(TestResults$TestName) )
  
  
  
  observeEvent(input$Results_cell_edit, {
    
    info = input$Results_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    
    #get new value
    new_value <- DT::coerceValue(v, TestResults[i, j])
    
    # update local copy of TestResuts
    TestResults[i, j] <<- new_value
    
    # update local copy of data
    
    datatb[TestResults_rows[i], j] <<- new_value
    
    # update browser
    replaceData(proxy1, TestResults, resetPaging = TRUE)  # important
    
    
  })
  
  
  observeEvent(input$action, {
    
    dbWriteTable(db, "TestResults", data.frame(datatb), overwrite = TRUE)
    
  })
  
  #DashBoard
  
  db <- dbConnect(SQLite(), sqlitePath)
  
  testresultstabel <- tbl(db, "TestResults")
  
  testresultstabel <- testresultstabel %>% as.data.frame()
  
  
  
  
  Dashboarddata <- reactive({
    Dashboarddata <- testresultstabel %>% filter(gender %in% input$Sex) 
    
    if(is.null(input$Sex))
      return()
    Dashboarddata
  })
  
  print(Dashboarddata)
  
  
  output$value1 <- renderValueBox({
    
    valueBox("Total Tests",
             formatC(count(Dashboarddata()), format="d", big.mark=','),
             paste('Total Tests:',count(Dashboarddata()))
             ,icon = icon("stats",lib='glyphicon')
             ,color = "purple") 
    
  })
  
  
  
  #creating the plotOutput content
  output$MaxTestResultsbyType <- renderPlot({
    
    ggplot(data =Dashboarddata(),
           aes(x=RawResult, y=TestResults, fill=factor(gender))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Test Results") + 
      xlab("RawResult") + theme(legend.position="bottom" 
                                ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Max Test Results by Type") + labs(fill = "gender")
  })
  
  output$MaxTestResultsbyType <- renderPlot({
    
    ggplot(data =Dashboarddata(),
           aes(x=TestName, y=TestResults, fill=factor(gender))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Test Results") + 
      xlab("TestName") + theme(legend.position="bottom" 
                                ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Max Test Results by Type") + labs(fill = "gender")
  })
  
  
  
  output$TestResultsPerCustomer <- renderPlot({
    ggplot(data = testresultstabel, 
           aes(x=CustID, y=TestResults, fill=factor(TestName))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Test Results)") + 
      xlab("Customer") + theme(legend.position="bottom" 
                               ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Test Results by Test Name") + labs(fill = "Test Name")
  })
  
  
  PieDATA <- reactive({
    PieDATA <- testresultstabel %>% filter(LabLocation %in% input$LabLoc) 
    
    if(is.null(input$Ttype))
      return()
    PieDATA
  })
  
  
  
  
  output$distPie <- renderPlot({
    
    pct <- count(Dashboarddata()) / count(PieDATA()) *100
    
    pie(pct, labels = lbls, main = "Pie Chart of Total Reserve Use")
    
    
    
  })
  
  
  
  
  
  
}


shinyApp(ui = ui, server = server)

