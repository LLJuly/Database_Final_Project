####################################
#####  Shiny APP for Clients   #####
#####    Date: 2023-12-10      #####
#####     Author: Li Liu       #####
####################################
library(shiny)
library(shinythemes)
library(shinyauthr)
library(shinyWidgets)
library(shinydashboard)

library(RSQLite)
library(RMySQL)
library(DT)
library(DBI)


# connect with SQL database
con = dbConnect(RMySQL::MySQL(),
                dbname='BCL',
                host='localhost',
                port=3306,
                user='Li',
                password='07150622Ll')
dbListTables(con)


# get all the tables needed using SQL
table_client           <- dbGetQuery(con, "select * from client")                                                                       # observe, delete, add
table_work_on_project  <- dbGetQuery(con, "select p.project_id, p.title, p.date_received, p.deadline, p.p_type as type, sf.name as BCL_student, sf.email, sf.highest_degree 
                                           from project as p, student_faculty as sf, work_on as wo
                                           where p.project_id = wo.project_id and wo.v_number = sf.v_number")                           # observe
table_project_progress <- dbGetQuery(con, "select * from project_progress")                                                             # observe
table_grants           <- dbGetQuery(con, "select * from grants")                                                                       # observe, delete, add
table_output           <- dbGetQuery(con, "select * from output")                                                                       # observe, delete, add
table_project          <- dbGetQuery(con, "select * from project")
table_vertical_work_on <- dbGetQuery(con, "select sf.v_number, sf.name, sf.sex, sf.email, sf.highest_degree, sf.title, vp.project_id
                                           from student_faculty as sf
                                           left join
                                                (select wo.v_number, wo.project_id from work_on as wo
                                                 union
                                                 select wo.supervisor1_id, wo.project_id from work_on as wo
                                                 union
                                                 select wo.supervisor2_id, wo.project_id from work_on as wo) as vp
                                           on sf.v_number = vp.v_number
                                           order by vp.project_id")                                                                     # observe
   
# get all the values of an attribute
# vector_project_number  <- dbGetQuery(con, "select project_id from client")
# vector_client_v_number <- dbGetQuery(con, "select v_number from client")
# vector_grant_number    <- dbGetQuery(con, "select number from grants")

# define the user_base to store usernames and passwords
user_base <- tibble::tibble(
  user = c("user1", "user10001","user10002", "user10003"),
  password = sapply(c("pass1", "pass10001", "pass10002", "pass10003"), sodium::password_store),
  permissions = c("standard","standard","standard","standard"),
  name = c("User One", "User Two", "User Three", "User Four")
)

# define UI page
ui <- fluidPage(
  shinyauthr::loginUI(id = "login"),
  uiOutput("content") 
)

# define server
server <- function(input, output, session) {
  
  # Authenticate user
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE
  )
  
  # Reactive expression to check if user is authenticated
  user_auth <- reactive({credentials()$info})
  
  # output$*** define content to show after logging in 
  output$content <- renderUI({
    req(user_auth())
    
    # navbarPage helps to make multi-page layouts
    navbarPage("Welcome to the Biostatistical Consulting Laboratory!", collapsible = TRUE, inverse = TRUE, theme = shinytheme("readable"),
               tabPanel("Observe",                                              
                        fluidPage(
                          tabsetPanel(
                            tabPanel("Basic Information", br(),
                                     sidebarLayout(
                                       
                                       sidebarPanel(
                                         textInput("input_v_number",      label = h4("Input your v_number")),
                                         textInput("input_project_number",label = h4("Input the project number")),
                                         checkboxGroupInput("input_client_role", label = h4("Select the role in your team"), choices = c("PI","Student", "Other"), selected = "PI"),
                                         p("Want to learn more about BCL? Visit the ",a("BCL homepage.", href = "https://biostatistics.vcu.edu/research/bcl/"))
                                       ),
                                       
                                       mainPanel(
                                         textOutput("output_text"),
                                         h4("Your projects"),
                                         dataTableOutput("output_selected_v"),
                                         br(),
                                         h4("Basic information of this project"),
                                         dataTableOutput("output_selected_project"),
                                         br(),
                                         h4("BCL members working on this project"),
                                         dataTableOutput("output_selected_p"),
                                         br(),
                                         h4("Your team members"),
                                         dataTableOutput("output_selected_client"),
                                         br(),
                                         h4("Check the grant status"),
                                         dataTableOutput("output_selected_grant")
                                       )
                                       
                                     )),
                            
                            
                            
                            tabPanel("Progress", br(),
                                     sidebarLayout(
                                       
                                       sidebarPanel(
                                         textInput("input_project_number_2",
                                                   label = h4("Input your project number")),
                                         checkboxGroupInput("input_progress_2",
                                                            label = h4("Select the progress type"),
                                                            choices = c("SOW finished","SAP finished", "Analysis report finished"), selected = "SOW finished"),
                                         dateInput("input_date_2",
                                                   label = h4("See the progress updates of your project after this date"),
                                                   value = "2023-01-01")
                                       ),
                                       
                                       mainPanel(
                                         textOutput("output_text_2"),
                                         h4("Check the progress of your project"),
                                         dataTableOutput("output_selected_progress_2")
                                       )
                                       
                                     )
                                     ),
                            
                            
                            
                            tabPanel("Output", br(),
                                     sidebarLayout(
                                       
                                       sidebarPanel(
                                         textInput("input_project_number_3",
                                                   label = h4("Input your project number"))
                                       ),
                                       
                                       mainPanel(
                                         textOutput("output_text_3"),
                                         h4("Check the output of your project"),
                                         dataTableOutput("output_selected_output_3")
                                       )
                                       
                                     )
                                     )
                            
                          ))),
               
               
               
               tabPanel("Add",
                        fluidPage(
                          tabsetPanel(
                            tabPanel(
                              "Your Information",
                              sidebarLayout(
                                sidebarPanel(
                                  textInput("input_v_number_4",         label = h4("Input your v_number")),
                                  textInput("input_project_id_4",       label = h4("Input your project number")),
                                  selectInput("input_role_4",           label = h4("Select your role"),            choices = c("PI", "Student", "Other")),
                                  textInput("input_name_4",             label = h4("Input your name")),
                                  textInput("input_school_4",           label = h4("Input your school name")),
                                  textInput("input_department_4",       label = h4("Input your department name")),
                                  textInput("input_title_4",            label = h4("Input your title")),
                                  selectInput("input_sex_4",            label = h4("Select your sex"),             choices = c("Female", "Male", "Other")),
                                  textInput("input_email_4",            label = h4("Input your email")),
                                  actionButton("input_insert_client_4", label = h4("Click to Add"))
                                ),
                                mainPanel(
                                  textOutput("output_client_text_4"),
                                  tableOutput("output_client_table_4")
                                )
                              )
                            ),
                            
                            
                            tabPanel(
                              "Grant Information",
                              sidebarLayout(
                                sidebarPanel(
                                  textInput("input_grant_number_5",     label = h4("Input your grant number")),
                                  selectInput("input_grant_status_5",   label = h4("Select your grant status"),       choices = c("Preparing", "Funded", "Other")),
                                  selectInput("input_IRB_status_5",     label = h4("Select your IRB status"),         choices = c("Approve", "Waiting for IRB approval", "Other")),
                                  textInput("input_project_id_5",       label = h4("Input your project number")),
                                  actionButton("input_insert_grant_5",  label = h4("Click to Add"))
                                ),
                                mainPanel(
                                  textOutput("output_grant_text_5"),
                                  tableOutput("output_grant_table_5")
                                )
                              )
                            ),
                            
                            
                            tabPanel(
                              "Project Output",
                              sidebarLayout(
                                sidebarPanel(
                                  dateInput("input_date_6",             label = h4("Select a date"),  value = "2023-01-01"),
                                  selectInput("input_category_6",       label = h4("Select a output category"),    choices = c("Paper Publication", "Presentation", "Other")),
                                  textInput("input_status_6",           label = h4("Input your output status")),
                                  textInput("input_project_id_6",       label = h4("Input your project id")),
                                  actionButton("input_insert_output_6", label = h4("Click to Add"))
                                ),
                                mainPanel(
                                  textOutput("output_output_text_6"),
                                  tableOutput("output_output_table_6")
                                )
                              )
                            )
                          ))),
               
               
               
               tabPanel("Delete",
                        fluidPage(
                          tabsetPanel(
                            tabPanel(
                              "Your Information",
                              sidebarLayout(
                                sidebarPanel(
                                  textInput("input_v_number_7",  label = h4("Input your v_number")),
                                  actionButton("input_delete_client_7", "Click to Delete"),
                                ),
                                mainPanel(
                                  textOutput("output_client_text_7"),
                                  tableOutput("output_selected_output_7")
                                )
                              )
                            ),
                            
                            
                            tabPanel(
                              "Grant Information",
                              sidebarLayout(
                                sidebarPanel(
                                  textInput("input_grant_number_8",  label = h4("Input your grant number")),
                                  actionButton("input_delete_grant_8", "Click to Delete"),
                                ),
                                mainPanel(
                                  textOutput("output_grant_text_8"),
                                  tableOutput("output_selected_output_8")
                                )
                              )
                            ),
                            
                            
                            tabPanel(
                              "Project Output",
                              sidebarLayout(
                                sidebarPanel(
                                  textInput("input_project_id_9",    label = h4("Input your project number")),
                                  selectInput("input_category_9",    label = h4("Select the output category"),       choices = c("Paper Publication", "Presentation", "Other")),
                                  actionButton("input_delete_output_9", "Click to Delete"),
                                ),
                                mainPanel(
                                  textOutput("output_output_text_9"),
                                  tableOutput("output_selected_output_9")
                                )
                              )
                            )
                          )))
    ) # navbarPage function stops here
    
  })
  
  # output of page observe-basic information
  output$output_text               <- renderText({paste("You are checking information of project ", input$input_project_number)})
  output$output_selected_v         <- renderDataTable({table_client[table_client$v_number   == input$input_v_number,]})
  output$output_selected_project   <- renderDataTable({table_work_on_project[table_work_on_project$project_id == input$input_project_number,]})
  output$output_selected_p         <- renderDataTable({table_vertical_work_on[table_vertical_work_on$project_id == input$input_project_number, ]})
  output$output_selected_client    <- renderDataTable({table_client[table_client$project_id == input$input_project_number & table_client$role %in% input$input_client_role,]})
  output$output_selected_grant     <- renderDataTable({table_grants[table_grants$project_id == input$input_project_number,]})
  
  # output of page observe-progress
  output$output_text_2             <- renderText({paste("You are checking the progress of project ", input$input_project_number_2)})
  output$output_selected_progress_2<- renderDataTable({table_project_progress[table_project_progress$project_id == input$input_project_number_2 
                                                                              & table_project_progress$status %in% input$input_progress_2 
                                                                              & table_project_progress$date > input$input_date_2,]}) 
  
  # output of page observe-output
  output$output_text_3             <- renderText({paste("You are checking the output of project ", input$input_project_number_3)})
  output$output_selected_output_3  <- renderDataTable({table_output[table_output$project_id == input$input_project_number_3,]})
  
  # output of page add-information
  observeEvent(input$input_insert_client_4, {
    # get input values
    v_number_4   <- input$input_v_number_4
    project_id_4 <- input$input_project_id_4
    role_4       <- input$input_role_4
    name_4       <- input$input_name_4
    school_4     <- input$input_school_4
    department_4 <- input$input_department_4
    title_4      <- input$input_title_4
    sex_4        <- input$input_sex_4
    email_4      <- input$input_email_4
   
    # Insert into SQL database
    query_4 <- paste0("INSERT INTO client (v_number, project_id, role, name, school, department, title, sex, email) VALUE ('",
                    v_number_4, "', '", project_id_4, "', '", role_4, "', '", name_4, "', '", school_4, "', '", department_4, "', '", 
                    title_4,"', '", sex_4,"', '", email_4, "')")
    dbExecute(con, query_4)
    
    output$output_client_text_4 <- renderText("Your have added your information.")
    table_added_client_4        <- dbGetQuery(con, "select * from client")
    output$output_client_table_4 <- renderTable({
      table_added_client_4[table_added_client_4$v_number ==  v_number_4, ]
    })
  })
  
  # output of page add-grant
  observeEvent(input$input_insert_grant_5, {
    # get input values
    grant_number_5   <- input$input_grant_number_5
    grant_status_5   <- input$input_grant_status_5
    IRB_status_5     <- input$input_IRB_status_5
    project_id_5     <- input$input_project_id_5
    
    # Insert into SQL database
    query_5 <- paste0("INSERT INTO grants (number, grant_status, IRB_status, project_id) VALUE ('",
                      grant_number_5, "', '", grant_status_5, "', '", IRB_status_5, "', '", project_id_5,  "')")
    dbExecute(con, query_5)
    
    output$output_grant_text_5   <- renderText("Your have added your grant information.")
    table_added_grant_5          <- dbGetQuery(con, "select * from grants")
    output$output_grant_table_5  <- renderTable({
      table_added_grant_5[table_added_grant_5$number ==  grant_number_5, ]
    })
  })
  
  # output of page add-output
  observeEvent(input$input_insert_output_6, {
    # get input values
    date_6          <- input$input_date_6
    category_6      <- input$input_category_6
    status_6        <- input$input_status_6
    project_id_6    <- input$input_project_id_6
    
    # Insert into SQL database
    query_6 <- paste0("INSERT INTO output (date, category, status, project_id) VALUE ('",
                      date_6, "', '", category_6, "', '", status_6, "', '", project_id_6,  "')")
    dbExecute(con, query_6)
    
    output$output_output_text_6    <- renderText("Your have added your project output information.")
    table_added_output_6           <- dbGetQuery(con, "select * from output")
    output$output_output_table_6   <- renderTable({
      table_added_output_6[table_added_output_6$project_id ==  project_id_6, ]
    })
  })
  
  # output of page delete-information
  observeEvent(input$input_delete_client_7, {
    # get input values
    v_number_7   <- input$input_v_number_7
    
    output$output_client_text_7   <- renderText("Your have deleted your information.")
    table_deleted_client_7        <- dbGetQuery(con, "select * from client")
    output$output_selected_output_7 <- renderTable({
      table_deleted_client_7[table_deleted_client_7$v_number ==  v_number_7, ]
    })
    query_7 <- paste0("DELETE FROM client WHERE v_number = '", v_number_7, "'")
    dbExecute(con, query_7)
  })
  
  # output of page delete-grant
  observeEvent(input$input_delete_grant_8, {
    # get input values
    grant_number_8   <- input$input_grant_number_8
    
    output$output_grant_text_8    <- renderText("Your have deleted your grant information.")
    table_deleted_grant_8         <- dbGetQuery(con, "select * from grants")
    output$output_selected_output_8 <- renderTable({
      table_deleted_grant_8[table_deleted_grant_8$number ==  grant_number_8, ]
    })
    query_8 <- paste0("DELETE FROM grants WHERE number = '", grant_number_8, "'")
    dbExecute(con, query_8)
  })
  
  # output of page delete-output
  observeEvent(input$input_delete_output_9, {
    # get input values
    project_id_9   <- input$input_project_id_9
    category_9     <- input$input_category_9
    
    output$output_output_text_9     <- renderText("Your have deleted your project output information.")
    table_deleted_output_9          <- dbGetQuery(con, "select * from output")
    output$output_selected_output_9 <- renderTable({
      table_deleted_output_9[table_deleted_output_9$project_id ==  project_id_9 & table_deleted_output_9$category == category_9, ]
    })
    query_9 <- paste0("DELETE FROM output WHERE project_id = '", project_id_9, "'", "and category = '", category_9, "'")
    dbExecute(con, query_9)
  })
}

shinyApp(ui, server)



