# clear the list

# rm(list = ls())



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

library(rmarkdown)

library(rJava)

library(gridExtra)

library(rsconnect)

library(here)



Logged = FALSE





ui <- fluidPage(theme = shinytheme("superhero"),
                
                navbarPage(id = "mainpage1",
                           
                           
                           
                           strong("Welcome to my capstone project"),
                           
                           
                           
                           
                           
                           
                           
                           
                           navlistPanel( id = "Navp", widths =c(2, 10),
                                         
                                         
                                         
                                         tabPanel(
                                           
                                           
                                           
                                           title = "Main Menu",
                                           
                                           id = "Home",
                                           
                                           
                                           
                                           verbatimTextOutput("HomeInfo"),
                                           
                                           br(),
                                           
                                           br(),
                                           
                                           
                                           
                                           br(),br(),br(),
                                           
                                           strong("Welcome to your Lab Data, Please sign in/sign Up inorder to review/analyse the data")
                                           
                                           
                                           
                                           ,br(), br(),br(), br(), br(),br()
                                           
                                           ,br(), br(),br(), br(), br(),br()
                                           
                                           ,h4 ("Sample Submission System")
                                           
                                           
                                           
                                         ),
                                         
                                         
                                         
                                         tabPanel(
                                           
                                           title = "Sign in",
                                           
                                           
                                           
                                           
                                           
                                           tagList(
                                             
                                             div(id = "login",
                                                 
                                                 wellPanel(textInput("userName", "Username"),
                                                           
                                                           passwordInput("passwd", "Password"),
                                                           
                                                           br(),actionButton("Login", "Log in"),
                                                           
                                                           verbatimTextOutput("dataInfo")
                                                           
                                                           
                                                           
                                                           
                                                           
                                                 )),
                                             
                                             tags$style(type="text/css", "login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
                                             
                                           )
                                           
                                           
                                           
                                           
                                           
                                         ),
                                         
                                         
                                         
                                         
                                         
                                         tabPanel(title = "New User",
                                                  
                                                  h1(strong("New User Registration:")),
                                                  
                                                  
                                                  
                                                  
                                                  
                                                  
                                                  
                                                  
                                                  tagList(
                                                    
                                                    div(id = "NewUser",
                                                        
                                                        wellPanel(
                                                          
                                                          textInput('Name', 'Full Name:', width = '100%', placeholder = "Enter your name"),
                                                          
                                                          textInput ('Role', 'Role:', "customer", width = '100%'),
                                                          
                                                          textInput('CustID', 'User ID:', width = '100%', placeholder = "Enter User ID"),
                                                          
                                                          passwordInput('Password', 'Password:', width = '100%'),
                                                          
                                                          br(),
                                                          
                                                          actionButton("submit", "Submit"),
                                                          
                                                          actionButton("cancel", "Cancel")
                                                          
                                                        )
                                                        
                                                    ),
                                                    
                                                    tags$style(type="text/css", "login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
                                                    
                                                  )
                                                  
                                                  
                                                  
                                         ),
                                         
                                         
                                         
                                         tabPanel(title = "Submitter",
                                                  
                                                  h1(strong("Order your Test")),
                                                  
                                                  
                                                  
                                                  tagList(
                                                    
                                                    div(id = "Customer",
                                                        
                                                        wellPanel(
                                                          
                                                          
                                                          
                                                          verbatimTextOutput("CustInfo"),
                                                          
                                                          
                                                          
                                                          htmlOutput("CustID"),
                                                          
                                                          selectInput("gender", "Gender:",c("Male", "Female")),
                                                          
                                                          dateInput("RequestDate", "Request Date", format = "yyyy-mm-dd"),
                                                          
                                                          htmlOutput("TestName"),
                                                          
                                                          htmlOutput("LabLocation"),
                                                          
                                                          br(),actionButton("order", "Order"))),
                                                    
                                                    tags$style(type="text/css", "login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
                                                    
                                                  )
                                                  
                                                  
                                                  
                                                  
                                                  
                                                  
                                                  
                                         ),
                                         
                                         
                                         
                                         ############ Analyst page page UI
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         tabPanel(title = "Analyst",
                                                  
                                                  h1(strong("Analyst Page")),
                                                  
                                                  
                                                  
                                                  navbarPage("", id = "analystpage",
                                                             
                                                             
                                                             
                                                             verbatimTextOutput("AnaInfo"),
                                                             
                                                             
                                                             
                                                             
                                                             
                                                             frow1 <- fluidRow(               
                                                               
                                                               
                                                               
                                                               title = "Test Results"
                                                               
                                                               ,actionButton("displayResults", label = "Display Records")
                                                               
                                                               ,actionButton("action", label = "Update Records")
                                                               
                                                               ,br(),br()
                                                               
                                                               , width = "1100px"
                                                               
                                                               ,status = "primary"
                                                               
                                                               ,solidHeader = TRUE
                                                               
                                                               ,collapsible = TRUE
                                                               
                                                               ,label = "View Results"   ### )
                                                               
                                                               ,DTOutput("Results", height = "300px", width = "1100px")
                                                               
                                                               
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
                                                  
                                                  
                                                  
                                         ),     
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         tabPanel(title = "Dash board",
                                                  
                                                  
                                                  
                                                  dashboardPage(
                                                    
                                                    
                                                    
                                                    dashboardHeader(),
                                                    
                                                    dashboardSidebar(
                                                      
                                                      
                                                      
                                                      
                                                      
                                                      selectInput("LabLoc", "Lab Location:", choices = c("Mercy Hospital",     "Wash U School of medicine")),
                                                      
                                                      
                                                      
                                                      radioButtons(inputId = "Ttype", label = h3("Test Type"),
                                                                   
                                                                   choices = list("Lipid Profile" , "Glucose"),selected = 'Glucose'),
                                                      
                                                      
                                                      
                                                      fluidRow(column(2, verbatimTextOutput("Ttype"))),
                                                      
                                                      
                                                      
                                                      fluidRow(
                                                        
                                                        column(3,
                                                               
                                                               radioButtons(inputId = "Sex", label = h3("Gender"),
                                                                            
                                                                            choices = list("Male" , "Female"),selected = 'Male')
                                                               
                                                               
                                                               
                                                               
                                                               
                                                        )),
                                                      
                                                      fluidRow(
                                                        
                                                        column(3, verbatimTextOutput("Sex"))
                                                        
                                                      )
                                                      
                                                      
                                                      
                                                      
                                                      
                                                    ), 
                                                    
                                                    
                                                    
                                                    dashboardBody(
                                                      
                                                      
                                                      
                                                      fluidRow(valueBoxOutput("value1"),
                                                               
                                                               valueBoxOutput("value2"),
                                                               
                                                               valueBoxOutput("value3"),        
                                                               
                                                               br(),br(),br(),
                                                               
                                                               
                                                               
                                                               downloadButton('downloadpdf')
                                                               
                                                               
                                                               
                                                      ),
                                                      
                                                      
                                                      
                                                      fluidRow(
                                                        
                                                        box(
                                                          
                                                          title = "Max cholesterol/Diabetic By Location"
                                                          
                                                          ,status = "primary"
                                                          
                                                          ,solidHeader = TRUE
                                                          
                                                          ,collapsible = TRUE
                                                          
                                                          ,plotOutput("MaxTestResultsbyType", height = "300px")
                                                          
                                                        )
                                                        
                                                        
                                                        
                                                        ,box(
                                                          
                                                          title = "cholesterol by Customer"
                                                          
                                                          ,status = "primary"
                                                          
                                                          ,solidHeader = TRUE
                                                          
                                                          ,collapsible = TRUE
                                                          
                                                          ,plotOutput("TestResultsPerCustomer", height = "300px")
                                                          
                                                        )
                                                        
                                                        
                                                        
                                                      )
                                                      
                                                      
                                                      
                                                      
                                                      
                                                    )   
                                                    
                                                    
                                                    
                                                  )  
                                                  
                                                  
                                                  
                                                  
                                                  
                                         )   
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         , tabPanel(actionButton("Logout", "Logout") )
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                           )
                           
                           
                           
                ),
                
                
                
                
                
                uiOutput("page")
                
                
                
)





sqlitePath <- "C:/Users/18127/Documents/Capstonefinaldb/data.sqlite"









NewUserRegistration <- c("Password","Name","CustID","Role")

NewTestTypes <- c("TestName")

NewTestOrder <- c("CustID","gender", "RequestDate","TestName","LabLocation", "Test1Std")







server <- function(input, output, session) {
  
  
  
  hideTab(inputId = "Navp", target = "Dash board")
  
  hideTab(inputId = "Navp", target = "Analyst")
  
  hideTab(inputId = "Navp", target = "Submitter")
  
  
  
  Logged = FALSE
  
  
  
  
  
  
  
  formData <- reactive({
    
    
    
    data <- sapply(NewUserRegistration, function(x) input[[x]])
    
    data
    
  })
  
  
  
  
  table <- "USERS"
  
  
  
  observeEvent(input$submit, {
    
    
    
    saveData(formData())
    
    updateNavlistPanel(session, "Navp", selected = "Login")
    
  })
  
  
  
  
  
  
  saveData <- function(data) {
    
    
    db <- dbConnect(SQLite(), sqlitePath)
    
    
    
    
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
  
  
  
  
  
  
  
  
  
  
  
  newTestFormData <- reactive({
    
    
    
    newtestdata <- sapply(NewTestTypes, function(x) input[[x]])
    
    newtestdata
    
  })
  
  
  
  
  table1 <- "TestTypes"
  
  
  
  observeEvent(input$save, {
    
    
    
    saveTestData(newTestFormData())
    
    
    
  })
  
  
  
  
  
  
  
  
  saveTestData <- function(newtestdata) {
    
    
    db <- dbConnect(SQLite(), sqlitePath)
    
    
    
    
    query <- sprintf(
      
      "INSERT INTO %s (%s) VALUES ('%s')",
      
      table1,
      
      paste(names(newtestdata), collapse = ", "),
      
      paste(newtestdata, collapse = "', '")
      
    )
    
    
    dbGetQuery(db, query)
    
    dbDisconnect(db)
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  TestOrderFormData <- reactive({
    
    
    
    
    
    orderdata <- sapply(NewTestOrder, function(x) input[[x]])
    
    
    
    orderdata$RequestDate <- as.character(orderdata$RequestDate)
    
    
    
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
  
  
  
  
  
  
  ordertable <- "TestResults"
  
  
  
  observeEvent(input$order, {
    
    
    
    
    
    
    if (input$CustID =="None") {
      
      
      
      output$CustInfo <- renderText({"Please login . Thank you!! "})
      
      return()
      
    }
    
    
    
    saveOrderData(TestOrderFormData())
    
    
    
  })
  
  
  
  
  
  
  saveOrderData <- function(orderdata) {
    
    
    db <- dbConnect(SQLite(), sqlitePath)
    
    
    
    
    query <- sprintf(
      
      "INSERT INTO %s (%s) VALUES ('%s')",
      
      ordertable,
      
      paste(names(orderdata), collapse = ", "),
      
      paste(orderdata, collapse = "', '")
      
    )
    
    
    dbGetQuery(db, query)
    
    dbDisconnect(db)
    
    
    
    
    
    output$CustInfo <- renderText({"You have successfully placed your tests. Thank you!!!!"})
    
    return()
    
    
    
    
    
  }
  
  
  
  
  
  
  
  
  
  observeEvent(input$cancel, {
    
    
    
    updateTextInput(session, "Name", value = '')
    
    updateTextInput(session, "CustID", value = '')
    
    updateTextInput(session, "Password", value = '')
    
  })
  
  
  
  
  USER <- reactiveValues(Logged = Logged)
  
  inputdata <- reactive({
    
    
    
    validate(need(isolate(input$userName) == "", "Please Enter User name"))
    
    
    
  })
  
  
  
  
  
  
  
  
  observeEvent(input$Login, {
    
    
    
    output$dataInfo <- renderText({""})
    
    
    
    ### Check if user already logged in
    
    
    
    if (USER$Logged) {
      
      
      
      output$dataInfo <- renderText(stop({"You have already logged in!!!!!!"}))
      
      
      
      return()
      
      
      
    }
    
    
    
    
    
    
    if(input$userName == "" & input$passwd == "") {
      
      
      
      output$dataInfo <- renderText({"Please check your credentials"})
      
      
      
      return()
      
    }
    
    
    
    if(input$userName == "" ) {
      
      
      
      output$dataInfo <- renderText({"Please check your User"})
      
      return()
      
    }
    
    
    
    if(input$passwd == "") {
      
      
      
      output$dataInfo <- renderText({"Please check your password"})
      
      return()
      
    }
    
    
    
    
    
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
          
          
          
          
          dbDisconnect(db)
          
          
          
          
          
          if (length(userrec$CustID) == 0 ) {
            
            
            
            
            
            
            
            
            output$dataInfo <- renderText({"If you are a new CUSTOMER please sign up first"})
            
            return()
            
            
            
          } else {
            
            
            
            if ( userrec$CustID == Username ) {
              
              
              
              USER$Logged <- TRUE}
            
            
            
            successInfo <- cbind ("You have successfully logged on now as", Username)
            
            
            
            output$HomeInfo <- renderText({successInfo})
            
            output$CustInfo <- renderText({successInfo})
            
            
            
            
            output$dataInfo <- renderText({""})   
            
            
            
          }
          
          }
        
        }
      
      }   
    
    
    
    
    
    if (USER$Logged == TRUE)
      
    {
      
      
      
      
      output$CustID <- renderUI({
        
        selectInput("CustID", "Customer ID", userrec$CustID) })
      
      
      
      
      
      
      
      
      updateTextInput(session, "userName", value = '')
      
      updateTextInput(session, "passwd", value = '')
      
      
      
      if ( userrec$Role == "analyst" ) {
        
        
        
        showTab(inputId = "Navp", target = "Dash board")
        
        showTab(inputId = "Navp", target = "Analyst")
        
        showTab(inputId = "Navp", target = "New User")
        
        
        
        hideTab(inputId = "Navp", target = "Login")
        
        #   hideTab(inputId = "Navp", target = "NewUser")
        
        hideTab(inputId = "Navp", target = "Customer")
        
        
        
        updateNavlistPanel(session, "Navp", selected = "Analyst")
        
        
        
        
        
      }
      
      if ( userrec$Role == "customer" ) {
        
        
        
        showTab(inputId = "Navp", target = "Customer")
        
        
        
        hideTab(inputId = "Navp", target = "Dash board")
        
        hideTab(inputId = "Navp", target = "Analyst")
        
        hideTab(inputId = "Navp", target = "Login")
        
        hideTab(inputId = "Navp", target = "New User")
        
        
        
        updateNavlistPanel(session, "Navp", selected = "Customer")}
      
      
      
    }
    
    })
  
  
  observeEvent(input$Logout, {
    
    
    
    
    
    USER$Logged <- FALSE
    
    
    
    hideTab(inputId = "Navp", target = "Customer")
    
    hideTab(inputId = "Navp", target = "Analyst")
    
    showTab(inputId = "Navp", target = "Login")
    
    hideTab(inputId = "Navp", target = "Dash board")
    
    showTab(inputId = "Navp", target = "New User")
    
    
    
    updateTextInput(session, "userName", value = '')
    
    updateTextInput(session, "passwd", value = '')
    
    
    
    output$dataInfo <- renderText({""})
    
    output$HomeInfo <- renderText({"You Have successfully Logged out"})
    
    output$CustInfo <- renderText({""})
    
    
    
    output$CustID <- renderUI({
      
      selectInput("CustID", "Customer ID", "") })
    
    
    
    updateNavlistPanel(session, "Navp", selected = "Home")
    
    
    
    
    
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
  
  
  
  
  output$CustID <- renderUI({
    
    selectInput("CustID", "Customer ID", "None") })
  
  
  
  
  
  Listdata <- loadData("TestName", "TestTypes","TestName","")
  
  
  
  Testnamelist <- setNames(Listdata$TestName, Listdata$TestName)
  
  output$TestName <- renderUI({
    
    selectInput("TestName", "Test Name: ", Testnamelist)
    
  })
  
  
  Listdata1 <- loadData("LabLocation", "Location","LabLocation","")
  
  
  LabLoclist <- setNames(Listdata1$LabLocation, Listdata1$LabLocation)
  
  output$LabLocation <- renderUI({
    
    selectInput("LabLocation", "Lab: ", LabLoclist)
    
  })
  
  
  
  
  
  
  
  
  observeEvent(input$displayResults, { 
    
    
    
    
    
    db <- dbConnect(SQLite(), sqlitePath)
    
    datatb <- tbl(db, "TestResults")
    
    datatb <- datatb %>% as.data.frame()
    
    
    
    
    TestResults <- datatb
    
    output$Results <- renderDT(TestResults, options =
                                 
                                 list(scrollX = TRUE), editable = TRUE)
    
    
    
    
    proxy1 = dataTableProxy('Results')
    
    
    
    TestResults_rows <- which(TestResults$TestName != "" | is.na(TestResults$TestName) )
    
    
    
    
    observeEvent(input$Results_cell_edit, {
      
      
      
      info = input$Results_cell_edit
      
      str(info)
      
      
      
      
      
      i = info$row
      
      j = info$col
      
      v = info$value
      
      
      
      
      new_value <- DT::coerceValue(v, TestResults[i, j])
      
      
      
      
      TestResults[i, j] <<- new_value
      
      
      
      
      
      
      datatb[TestResults_rows[i], j] <<- new_value
      
      
      
      
      replaceData(proxy1, TestResults, resetPaging = TRUE)  # important
      
      
      
      
      
      
    })
    
    
    
    observeEvent(input$action, {
      
      
      
      dbWriteTable(db, "TestResults", data.frame(datatb), overwrite = TRUE)
      
    })
    
    
  })   
  
  
  #dashboard
  db <- dbConnect(SQLite(), sqlitePath)
  
  
  
  testresultstabel <- tbl(db, "TestResults")
  
  
  
  testresultstabel <- testresultstabel %>% as.data.frame()
  
  
  
  
  
  
  vals <- reactiveValues(MaxTestResultsbyType=NULL, TestResultsPerCustomer = NULL)
  
  
  
  
  
  
  
  
  
  
  Dashboarddata <- reactive({
    
    Dashboarddata <- testresultstabel %>%
      
      filter(LabLocation %in% input$LabLoc)
    
    
    
    
    
    if(is.null(input$Sex))
      
      return()
    
    Dashboarddata
    
  })
  
  
  
  output$value1 <- renderValueBox({
    
    
    
    valueBox(h4("Total Tests by Hospital:"),
             
             formatC(count(Dashboarddata()), format="d", big.mark=','),
             
             paste('Total Tests by Hospital:',count(Dashboarddata()))
             
             ,icon = icon("stats",lib='glyphicon')
             
             ,color = "purple")
    
    
    
  })
  
  
  
  
  
  
  
  
  
  Dashboarddata2 <- reactive({
    
    Dashboarddata2 <- testresultstabel %>%
      
      filter(LabLocation %in% input$LabLoc) %>%
      
      filter(TestName %in% input$Ttype)
    
    
    
    if(is.null(input$Ttype))
      
      return()
    
    Dashboarddata2
    
    
    
    
    
  })
  
  
  
  
  
  output$value2 <- renderValueBox({
    
    
    
    valueBox(h4("Total Tests by Test Type:"),
             
             formatC(count(Dashboarddata2()), format="d", big.mark=','),
             
             paste('Total Tests',count(Dashboarddata2()))
             
             ,icon = icon("stats",lib='glyphicon')
             
             ,color = "fuchsia")
    
    
    
  })
  
  
  
  
  
  
  
  
  
  Dashboarddata3 <- reactive({
    
    Dashboarddata3 <- testresultstabel %>%
      
      filter(LabLocation %in% input$LabLoc)  %>%
      
      filter(TestName %in% input$Ttype) %>%
      
      filter(gender %in% input$Sex)
    
    
    
    if(is.null(input$Sex))
      
      return()
    
    Dashboarddata3
    
  })
  
  
  
  output$value3 <- renderValueBox({
    
    
    
    valueBox(h4("Total Tests by Gender:"),
             
             formatC(count(Dashboarddata3()), format="d", big.mark=','),
             
             paste('Total Tests by Gender:',count(Dashboarddata3()))
             
             ,icon = icon("stats",lib='glyphicon')
             
             ,color = "green")
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  #creating the plotOutput 
  
  output$MaxTestResultsbyType <- renderPlot({
    
    
    
    vals$MaxTestResultsbyType <-    ggplot(data =Dashboarddata2(),
                                           
                                           aes(x=TestName, y=TestResults, fill=factor(gender))) +
      
      geom_bar(position = "dodge", stat = "identity") + ylab("Test Results") +
      
      xlab("Test Name") + theme(legend.position="bottom"
                                
                                ,plot.title = element_text(size=15, face="bold")) +
      
      labs(fill = "gender")
    
    
    
    vals$MaxTestResultsbyType
    
    
    
  })
  
  
  
  
  
  
  
  
  Dashboarddata4 <- reactive({
    
    Dashboarddata4 <- testresultstabel %>%
      
      filter(LabLocation %in% input$LabLoc) %>%
      
      filter(TestName == "Lipid Profile")
    
    
    
    if(is.null(input$Ttype))
      
      return()
    
    Dashboarddata4
    
    
    
    
    
  })
  
  
  
  
  
  output$TestResultsPerCustomer <- renderPlot({
    
    
    
    
    
    
    
    
    vals$TestResultsPerCustomer <- ggplot(Dashboarddata4(),
                                          
                                          aes(x = CustID, y = TestResults,fill=factor(TestName))) +
      
      
      geom_point(size = 5, stat = "identity") + ylab("Test Results") +
      
      xlab("Customer") + theme(legend.position="bottom"
                               
                               ,plot.title = element_text(size=15, face="bold")) +
      
      ggtitle("Test Results by Customer") + labs(fill = "Test Name")
    
    
    
    vals$TestResultsPerCustomer
    
    
    
  })
  
  
  
  
  
  
  
  output$downloadReport <- downloadHandler(
    
    
    
    
    
    filename = function() {
      
      paste("downloadReport.pdf",sep="")},
    
    
    
    content = function(file) {
      
      pdf(file)
      
      
      
      grid.arrange(vals$MaxTestResultsbyType, vals$TestResultsPerCustomer)
      
      dev.off()
      
    }
    
  )
  
  }  












shinyApp(ui = ui, server = server)





