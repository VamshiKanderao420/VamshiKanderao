
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
library(rmarkdown)

library(rJava)

library(gridExtra)

library(rsconnect)

library(here)

Logged = FALSE;


ui <- fluidPage(theme = shinytheme("journal"),
                navbarPage(id = "mainpage4",
                           strong("Capstone Project",align = "center"),
                           
                           
                           navlistPanel( id = "Navp", widths =c(2, 10),
                                         
                                         tabPanel(title = "Main Menu", id = "Home", verbatimTextOutput("HomeInfo"),
                                                  
                                                  br(),
                                                  
                                                  br(),
                                                  
                                                  strong("This is where the lab data is displayed  , existing users please log in / create account and order their tests and logout. Analysts can create new users, update test results, view reports and download
                                                  
                                                  reports. Depending on the user login and role relavent tabs will be displayed")
                                                  
                                                  ,br(), br(),br(), br(), br(),br()
                                                  
                                                  ,br(), br(),br(), br(), br(),br()
                                                  
                                                  ,h4 ("Developed by Kanderao Vamshi Mohan
                                                       
                                                       as part of Capstone Project")
                                                  
                                                  
                                                  
                                                  ),
                                         
                                         
                                         
                                         tabPanel(
                                           
                                           title = "Login",
                                           
                                           
                                           
                                           
                                           
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
                                                          
                                                          textInput('UserId', 'User ID:', width = '100%', placeholder = "Enter User ID"),
                                                          
                                                          passwordInput('Password', 'Password:', width = '100%'),
                                                          
                                                          br(),
                                                          
                                                          actionButton("submit", "Submit"),
                                                          
                                                          actionButton("cancel", "Cancel")
                                                          
                                                        )
                                                        
                                                    ),
                                                    
                                                    tags$style(type="text/css", "login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
                                                    
                                                  )
                                                  
                                                  
                                                  
                                         ),
                                         
                                         
                                         
                                         tabPanel(title = "Customer",
                                                  
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
                                                               
                                                               ###           ,actionButton("action", label = "Write to DB")
                                                               
                                                             ),     
                                                             
                                                             
                                                             
                                                             ##### Add new tests tab
                                                             
                                                             
                                                             
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
                                         
                                         #### Dashboard Page UI
                                         
                                         
                                         
                                         
                                         
                                         tabPanel(title = "Dash board",
                                                  
                                                  
                                                  
                                                  dashboardPage(
                                                    
                                                    
                                                    
                                                    dashboardHeader(),
                                                    
                                                    dashboardSidebar(
                                                      
                                                      
                                                      
                                                      
                                                      
                                                      selectInput("LabLoc", "Lab Location:", choices = c("Mercy Hospital", "Wash U School of medicine")),
                                                      
                                                      
                                                      
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
                                                      
                                                      
                                                      
                                                      
                                                      
                                                    ), ####dashboardSidebar end
                                                    
                                                    
                                                    
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
                                                      
                                                      
                                                      
                                                      
                                                      
                                                    )   # Dashboard body end
                                                    
                                                    
                                                    
                                                  )  ####dashboardPage end
                                                  
                                                  
                                                  
                                                  
                                                  
                                         )   ##### Dashboard page tab panel UI   end
                                         
                                         
                                         
                                         ##############   Logout button  ###############
                                         
                                         
                                         
                                         , tabPanel(actionButton("Logout", "Logout") )
                                         
                                         
                                         
                                         
                                         
                                         ############################
                                         
                                         
                                         
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
    
    
    
    
    
    ########### Validate whether user logged in or not###########
    
    if (input$CustID =="None") {
      
      
      
      output$CustInfo <- renderText({"Please login before ordering your tests. Thank you!!!!"})
      
      return()
      
    }
    
    
    
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
  
  
  output$CustInfo <- renderText({"You have successfully placed your tests. Thank you!!!!"})
  
  return()
  
  
  
  
  
}
  
  
  #Cancel option
  

observeEvent(input$cancel, {
  
  
  
  updateTextInput(session, "Name", value = '')
  
  updateTextInput(session, "CustID", value = '')
  
  updateTextInput(session, "Password", value = '')
  
})



############################# Log in validation ################

USER <- reactiveValues(Logged = Logged)

inputdata <- reactive({
  
  
  
  validate(need(isolate(input$userName) == "", "Please Enter User name"))
  
  
  
})




#Login Validation

USER <- reactiveValues(Logged = Logged)

inputdata <- reactive({
  
  
  
  validate(need(isolate(input$userName) == "", "Please Enter User name"))
  
  
  
})





  
  #User Login
  

observeEvent(input$Login, {
  
  
  
  output$dataInfo <- renderText({""})
  
  
  
  ### Check if user already logged in
  
  
  
  if (USER$Logged) {
    
    
    
    output$dataInfo <- renderText(stop({"You have already logged in!!!!!!"}))
    
    
    
    return()
    
    
    
  }
  
  
  
  #  Check if User Name & Password entered or not
  
  
  
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
        
        #  print(userrec)
        
        
        
        dbDisconnect(db)
        
        
        
        
        
        if (length(userrec$CustID) == 0 ) {
          
          
          
          # print error/ warning message
          
          
          
          
          
          output$dataInfo <- renderText({"If you are a new user please register before login OR Check your credentials"})
          
          return()
          
          
          
        } else {
          
          
          
          if ( userrec$CustID == Username ) {
            
            
            
            USER$Logged <- TRUE}
          
          
          
          successInfo <- cbind ("You Have Successfully logged in as", Username)
          
          
          
          output$HomeInfo <- renderText({successInfo})
          
          output$CustInfo <- renderText({successInfo})
          
          ###     output$AnaInfo <- renderText({successInfo})
          
          
          
          output$dataInfo <- renderText({""})   ##### Clear previous message
          
          
          
        }
        
        }
      
      }
    
    }   
  
  
  
  
  
  if (USER$Logged == TRUE)
    
  {
    
    ######################### LOAD User Name in Customer tab ###############
    
    ###################################################
    
    
    
    output$CustID <- renderUI({
      
      selectInput("CustID", "Customer ID", userrec$CustID) })
    
    
    
    
    
    ########### Hide some Tabs when Login #######################
    
    
    
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

################### Logout logic#####################

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



############Initiallize user if not logged in ###############

output$CustID <- renderUI({
  
  selectInput("CustID", "Customer ID", "None") })

#############################################################



################# load Test Name in Customer tab

Listdata <- loadData("TestName", "TestTypes","TestName","")

#print(Listdata)

#Listdata<- rbind(data.frame("TestName" = ""), Listdata)

Testnamelist <- setNames(Listdata$TestName, Listdata$TestName)

output$TestName <- renderUI({
  
  selectInput("TestName", "Test Name: ", Testnamelist)
  
})

####### Lab Locations load data from database

Listdata1 <- loadData("LabLocation", "Location","LabLocation","")

# print(Listdata1)

LabLoclist <- setNames(Listdata1$LabLocation, Listdata1$LabLocation)

output$LabLocation <- renderUI({
  
  selectInput("LabLocation", "Lab: ", LabLoclist)
  
})





################ Data Table start###############################

###########################################################

###########################################################

observeEvent(input$displayResults, { 
  
  
  
  
  
  db <- dbConnect(SQLite(), sqlitePath)
  
  datatb <- tbl(db, "TestResults")
  
  datatb <- datatb %>% as.data.frame()
  
  
  
  #TestResults <- filter(datatb,  (is.na(Test1Results) | Test1Results == 0))
  
  TestResults <- datatb
  
  output$Results <- renderDT(TestResults, options =
                               
                               list(scrollX = TRUE), editable = TRUE)
  
  
  
  #necessary code to replace data once edited
  
  proxy1 = dataTableProxy('Results')
  
  #print(proxy1)
  
  ####TestResults_rows <- which(datatb$ TestName != "")
  
  TestResults_rows <- which(TestResults$TestName != "" | is.na(TestResults$TestName) )
  
  # print(TestResuts_rows)
  
  
  
  observeEvent(input$Results_cell_edit, {
    
    
    
    info = input$Results_cell_edit
    
    str(info)
    
    
    
    
    
    i = info$row
    
    j = info$col
    
    v = info$value
    
    
    
    ############ get new value
    
    new_value <- DT::coerceValue(v, TestResults[i, j])
    
    
    
    ############# update local copy of TestResuts
    
    TestResults[i, j] <<- new_value
    
    
    
    ############# update local copy of data
    
    
    
    datatb[TestResults_rows[i], j] <<- new_value
    
    
    
    ############# update browser
    
    replaceData(proxy1, TestResults, resetPaging = TRUE)  # important
    
    
    
    ##### dbDisconnect(db)
    
    
    
  })
  
  
  
  observeEvent(input$action, {
    
    
    
    dbWriteTable(db, "TestResults", data.frame(datatb), overwrite = TRUE)
    
  })
  
  ### dbDisconnect(db)
  
})   ########### end of display results

  
  #DashBoard

db <- dbConnect(SQLite(), sqlitePath)



testresultstabel <- tbl(db, "TestResults")



testresultstabel <- testresultstabel %>% as.data.frame()



################ initialize ##################



vals <- reactiveValues(MaxTestResultsbyType=NULL, TestResultsPerCustomer = NULL)







################ Value 1 ################################



Dashboarddata <- reactive({
  
  Dashboarddata <- testresultstabel %>%
    
    filter(LabLocation %in% input$LabLoc)
  
  ###   %>% filter(TestName %in% input$Ttype) %>%
  
  ###   filter(gender %in% input$Sex)
  
  
  
  if(is.null(input$Sex))
    
    return()
  
  Dashboarddata
  
})



output$value1 <- renderValueBox({
  
  
  
  valueBox(h4("Total Tests by Location:"),
           
           formatC(count(Dashboarddata()), format="d", big.mark=','),
           
           paste('Total Tests by Location:',count(Dashboarddata()))
           
           ,icon = icon("stats",lib='glyphicon')
           
           ,color = "purple")
  
  
  
})



################### Value 1 End ####################### 



################ Value 2 ################################



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
           
           ,color = "green")
  
  
  
})



################### Value 2 End #######################



################ Value 3 ################################



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
           
           ,color = "red")
  
  
  
})



################### Value 3 End #######################



################### Histogram - Max Results by Gender########################





#creating the plotOutput content

output$MaxTestResultsbyType <- renderPlot({
  
  
  
  vals$MaxTestResultsbyType <-    ggplot(data =Dashboarddata2(),
                                         
                                         aes(x=TestName, y=Test1Results, fill=factor(gender))) +
    
    geom_bar(position = "dodge", stat = "identity") + ylab("Test Results") +
    
    xlab("Test Name") + theme(legend.position="bottom"
                              
                              ,plot.title = element_text(size=15, face="bold")) +
    
    labs(fill = "gender")
  
  
  
  vals$MaxTestResultsbyType
  
  
  
})



###############





Dashboarddata4 <- reactive({
  
  Dashboarddata4 <- testresultstabel %>%
    
    filter(LabLocation %in% input$LabLoc) %>%
    
    filter(TestName == "Lipid Profile")
  
  
  
  if(is.null(input$Ttype))
    
    return()
  
  Dashboarddata4
  
  
  
  
  
})





output$TestResultsPerCustomer <- renderPlot({
  
  
  
  ##  vals$TestResultsPerCustomer <-   ggplot(data = testresultstabel,
  
  ##        aes(x=CustID, y=Test1Results, fill=factor(TestName))) +
  
  ##   geom_bar(position = "dodge", stat = "identity", colour='yellow') + ylab("Test Results)") +
  
  ##   xlab("Customer") + theme(legend.position="bottom"
  
  ##                          ,plot.title = element_text(size=15, face="bold")) +
  
  ##   ggtitle("Test Results by Customer") + labs(fill = "Test Name")
  
  
  
  
  
  
  
  vals$TestResultsPerCustomer <- ggplot(Dashboarddata4(),
                                        
                                        aes(x = CustID, y = Test1Results,fill=factor(TestName))) +
    
    ###  ggplot(Dashboarddata4(),
    
    ###        aes(x = CustID, y = Test1Results,fill=factor(TestName))) +
    
    
    
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


 











shinyApp(ui = ui, server = server)



