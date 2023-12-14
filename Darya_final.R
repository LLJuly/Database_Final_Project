library(RMySQL)
library(shiny)
library(DBI)
library(shiny)
library(shinyauthr)
library(DT)
library(shinydashboard)

#mysql connection
mysqlconnection <- dbConnect(RMySQL::MySQL(),
                             dbname = 'BCL',
                             host = 'localhost',
                             port = 3306,
                             user = 'root',
                             password = '951611028D@ryaSH')

# get all the tables needed using SQL
all_tables <- dbGetQuery(mysqlconnection,"show tables")
#user here will either input the project id or the project name
table_project_progress <- dbGetQuery(mysqlconnection, "SELECT title,project_id,status,date FROM project INNER JOIN project_progress  USING (project_id)")
table_project_deadline <- dbGetQuery(mysqlconnection, "SELECT title,deadline,project_id FROM project")

#user will input the department or the school and can see the client information
table_department <- dbGetQuery(mysqlconnection, "select c.name, c.role, c.school,c.department, p.title,p.project_id from project p INNER join client c using (project_id)")


#user will input the v number
table_students <- dbGetQuery(mysqlconnection, "select sf.v_number, sf.name, sf.email, sf.title, vp.project_id
                                           from student_faculty as sf
                                           left join
                                                (select wo.v_number, wo.project_id from work_on as wo
                                                 union
                                                 select wo.supervisor1_id, wo.project_id from work_on as wo
                                                 union
                                                 select wo.supervisor2_id, wo.project_id from work_on as wo) as vp
                                           on sf.v_number = vp.v_number
                                           order by vp.project_id")
table_student_project <- dbGetQuery(mysqlconnection, "
                                    select p.project_id,p.deadline, p.title,wo.supervisor1_id,wo.v_number,wo.supervisor2_id from project as p inner join work_on as wo using(project_id)")

table_student_status <- dbGetQuery(mysqlconnection,
                                  "select p.project_id, p.date,p.status, wo.v_number from project_progress p inner join work_on wo using(project_id) " )
user_base <- tibble::tibble(
  user = c("Darya", "Jasmine","Li"),
  password = sapply(c("123", "123","123"), sodium::password_store),
  permissions = c("admin", "standard","standard2"),
  name = c("User One", "User Two","User Three")
)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "sandstone"),
  mainPanel(
    img(src = "a1.jpg", height = 120, width = 200),
    div(),
    h1("BCL"),
    h2("Biostatistical Consulting Lab")
    
  ),
  shinyauthr::loginUI(id = "login"),
  uiOutput("content")
)
  
#login section
server <- function(input, output, session) {
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
  )


# Reactive expression to check if user is authenticated
  user_auth <- reactive({
    credentials()$info
  })
# Content to show after login
  output$content <- renderUI({
    req(user_auth())
    ## Design input ----
    navbarPage(
      "", 
      
      navbarMenu("Overview", 
                 tabPanel("All Projects",
                          DT::dataTableOutput("tableObs_prj")),
                 tabPanel("Client Information",
                          DT::dataTableOutput("tableObs_pi")),
                 tabPanel("Students and Faculties Information",
                          DT::dataTableOutput("tableObs_stu"))
                 ),
                 navbarMenu("Retrieve Information",
                            tabPanel("Project section", br(),
                                     sidebarLayout(
                                       
                                       sidebarPanel(
                                         textInput("input_project_number",label = h4("Input the project number")),
                                       ),
                                       
                                       mainPanel(
                                         textOutput("output_text"),
                                         br(),
                                         div("Deadline of this project is ", style = "color:red"),
                                         textOutput("output_project_deadline"),
                                         h4("Project Progress"),
                                         dataTableOutput("output_project_progress"),
                                         br(),

                                         br(),
                                         h4("BCL members working on this project"),
                                         dataTableOutput("output_selected_bcl")
                                       )
                                       
                                     )),
                            tabPanel("Client section", br(),
                                     sidebarLayout(
                                       
                                       sidebarPanel(
                                         selectInput("input_department", " Select the Department to view their projects  ",
                                                     choices = dbGetQuery(mysqlconnection, "SELECT department FROM client")),
                                         selectInput("input_school", " Select the VCU school to view their projects  ",
                                                     choices = dbGetQuery(mysqlconnection, "SELECT school FROM client")),
                                       ),
                                       
                                       mainPanel(
                                         h4("Projects of this department are: "),
                                         dataTableOutput("output_selected_department"),
                                         br(),
                                         h4("Projects of this school are: "),
                                         dataTableOutput("output_selected_school"),
                                         br(),
                                       )
                                       
                                     )),
                            tabPanel("Student section", br(),
                                     sidebarLayout(
                                       
                                       sidebarPanel(
                                         textInput("input_vnumber",label = h4("Input V number: "))
                                       ),
                                       
                                       mainPanel(
                                         h4("Your projects and deadlines: "),
                                         dataTableOutput("output_selected_projects"),
                                         br(),
                                         h4("Status of the projects assigned to you:"),
                                         dataTableOutput("output_project_status"),
                                         br(),
                                       )
                                       
                                     ))
                            
                 ),
      navbarMenu("Edit table", 
                 tabPanel("Insert New Project", sidebarLayout(
                   sidebarPanel(
                     textInput("prj_id", "Enter New Project ID: "),
                     textInput("prj_title", "Enter Project Title: "),
                     dateInput("date_received", label = "Enter Updated Date Received (yyyy-mm-dd): ",
                               value = "2021-01-01"),
                     dateInput("deadline", label = "Enter Updated Deadline Received (yyyy-mm-dd): ",
                               value = "2021-01-01"),
                     selectInput("type", "Select Project Type: ", choices = c("Consulting", "Project")),
                     actionButton("insertPrj", "Insert into Table"),
                   ),
                   mainPanel(
                     textOutput("status2a"),
                     tableOutput("tableOutput_Nprj")
                   )
                 )
                ),
                 tabPanel("Delete a Project", 
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("prj_id.rm", "Input Project ID to Remove: ",
                                          value = dbGetQuery(mysqlconnection, "SELECT project_id FROM project;")),
                              actionButton("rmPrjAssign", "Remove Project"),
                            ),
                            mainPanel(
                              textOutput("status2e.rm"),
                              tableOutput("tableOutput_Rmprj")
                            )
                          )
                        ),
                 tabPanel("Update a project",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("prj_id_assign.up", "Select Project ID to Update: ",
                                          choices = dbGetQuery(mysqlconnection, "SELECT project_id FROM project;")),
                              selectInput("v_number_assign.up", "Select Updated Assigned Student V Number: ",
                                          choices = dbGetQuery(mysqlconnection, "SELECT V_number FROM student_faculty WHERE title = 'Student';")),
                              selectInput("super1_id.up", "Select Updated Supervisor 1 ID: ",
                                          choices = dbGetQuery(mysqlconnection, "SELECT V_number FROM student_faculty WHERE title = 'Student' or 'Staff';")),
                              selectInput("super2_id.up", "Select Updated Supervisor 2 ID: ",
                                          choices = dbGetQuery(mysqlconnection, "SELECT V_number FROM student_faculty WHERE title IN ('Assistant Professor', 'Associate Professor', 'Professor');")),
                              actionButton("updatePrjAssign", "Update Table"),
                            ),
                            mainPanel(
                              textOutput("status2d.up"),
                              tableOutput("tableOutput_UpAprj")
                            )
                          )
                  )
      )
    )
  })
  # output of page observe-basic information
  output$output_text               <- renderText({paste("You are checking information of project ", input$input_project_number)})
  output$output_project_progress   <- renderDataTable({table_project_progress[table_project_progress$project_id   == input$input_project_number,]})
  output$output_project_deadline   <- renderText({paste(table_project_deadline[table_project_deadline$project_id == input$input_project_number,]$deadline)})
  output$output_selected_bcl         <- renderDataTable({table_students[table_students$project_id == input$input_project_number, ]})
  output$output_selected_department    <- renderDataTable({table_department[table_department$department == input$input_department,]})
  output$output_selected_school    <- renderDataTable({table_department[table_department$school == input$input_school,]})
  output$output_selected_projects    <- renderDataTable({table_student_project[table_student_project$v_number == input$input_vnumber,]})
  output$output_project_status    <- renderDataTable({table_student_status[table_student_status$v_number == input$input_vnumber,]})
  
    # Output of Observed table of Student and Faculty
  output$tableObs_stu <- DT::renderDataTable({
    table <- dbGetQuery(mysqlconnection, "SELECT * FROM student_faculty;")
    names(table) <- c('V number', 'Name', 'Sex', 'Email', 'Highest Degree', 'Title')
    DT::datatable(table)
  })
  
  # Output of Observed table of Client
  output$tableObs_pi <- DT::renderDataTable({
    table <- dbGetQuery(mysqlconnection, " SELECT * FROM client;")
    names(table) <- c('V Number', 'Project ID', 'Role', 'Name', 'School' , 'Department', 'Title', 'Sex', 'Email')
    DT::datatable(table)
  })
  
  # Output of Observed table of Project
  output$tableObs_prj <- DT::renderDataTable({
    table <- dbGetQuery(mysqlconnection, "
  SELECT project.project_id, title, date_received, deadline, p_type, v_number, supervisor1_id, supervisor2_id, number, grant_status, IRB_status 
  FROM project
  LEFT JOIN work_on ON project.project_id = work_on.project_id
  LEFT JOIN grants ON project.project_id = grants.project_id;
")
    names(table) <- c('ID', 'Title', 'Received', 'Deadline', 'Type', 'Working Student', 'Supervisor1', 'Supervisor2', 'Grant number', 'Grant Status', 'IRB Status')
    DT::datatable(table)
  })
  
  # Output of Table of New Project
  observeEvent(input$insertPrj, {
    # Get input values
    prj_id <- input$prj_id
    prj_title <- input$prj_title
    date_received <- input$date_received
    deadline <- input$deadline
    p_type <- input$type
    
    
    # Insert into SQL table
    query <- paste0("INSERT INTO project (project_id, title, date_received, deadline, p_type) VALUES ('",
                    prj_id, "', '", prj_title, "', '", date_received, "', '", deadline, "', '", p_type, "')")
    dbExecute(mysqlconnection, query)
    
    # Display status
    output$status2a <- renderText("Data inserted into Project Table.")
    # Display updated table
    output$tableOutput_Nprj <- renderTable({
      # Retrieve and display data
      query_select <- "SELECT * FROM project"
      dbGetQuery(mysqlconnection, query_select)
    })
  })
  
  # Update of Table of New Project
  observeEvent(input$updatePrj, {
    # Get input values
    prj_id <- input$prj_id.up
    prj_title <- input$prj_title.up
    date_received <- input$date_received.up
    deadline <- input$deadline.up
    p_type <- input$type.up
    
    
    # Insert into SQL table
    query <- paste0("UPDATE project SET title = '", prj_title, "', date_received = '", date_received, "', deadline = '", deadline, "', p_type = '", p_type,
                    "' WHERE project_id = '", prj_id, "'")
    
    dbExecute(mysqlconnection, query)
    
    # Display status
    output$status2c.up <- renderText("Data Updated into Project Table.")
    # Display updated table
    output$tableOutput_Nprj <- renderTable({
      # Retrieve and display data
      query_select <- "SELECT * FROM project"
      dbGetQuery(mysqlconnection, query_select)
    })
  })
  # Remove All for a project
  observeEvent(input$rmPrjAssign, {
    # Get input values
    project_id <- input$prj_id.rm
    
    # Delete from SQL project table
    query1 <- paste0("DELETE FROM project WHERE project_id = '", project_id, "'")
    dbExecute(mysqlconnection, query1)
    
    # Delete from SQL assigned project table
    query2 <- paste0("DELETE FROM work_on WHERE project_id = '", project_id, "'")
    dbExecute(mysqlconnection, query2)
    
    # Delete from SQL progress project table
    query3 <- paste0("DELETE FROM project_progress WHERE project_id = '", project_id, "'")
    dbExecute(mysqlconnection, query3)
    
    # Delete from SQL output project table
    query3 <- paste0("DELETE FROM output WHERE project_id = '", project_id, "'")
    dbExecute(mysqlconnection, query3)
    
    # Display status
    output$status2e.rm <- renderText("The project is removed from ALL tables.")
    output$tableOutput_Rmprj <-  renderTable({
      # Retrieve and display data
      query_select <- "SELECT * FROM project"
      dbGetQuery(mysqlconnection, query_select)
    })
  })
}



shinyApp(ui = ui, server = server)
