library(RMySQL)
library(shiny)
library(DBI)
library(shiny)
library(shinyauthr)
library(DT)
library(shinydashboard)

## Connect with the MySQL database
mysqlconnection <- dbConnect(RMySQL::MySQL(),
                             dbname = 'BCL',
                             host = 'localhost',
                             port = 3306,
                             user = 'root',
                             password = 'Mjmine123')


## Retreive the list of id

# Define the user_base
user_base <- tibble::tibble(
  user = c("user1", "user2"),
  password = sapply(c("pass1", "pass2"), sodium::password_store),
  permissions = c("admin", "staff"),
  name = c("User One", "User Two")
)

# Define UI
ui <- fluidPage(
  shinyauthr::loginUI(id = "login"),
  uiOutput("content")
)

# Define server
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
  user_auth <- reactive({
    credentials()$info
  })
  
  # Content to show after login
  output$content <- renderUI({
    req(user_auth())
    
    
    ## Design input ----
    navbarPage(
      "BCL Administration",
      
      
      ### Overview ----
      tabPanel(
        "Overview",
        fluidRow(
          valueBoxOutput("value1"),
          valueBoxOutput("value2"),
          valueBoxOutput("value3"),
        ),
        tags$hr(style = "margin-bottom: 60px;"),
        fluidRow(
          column(8, 
                 plotOutput("output_freqStatus_time")
          ),
          column(4, 
                 plotOutput("output_freqStatus_current")   
          )
        ),
        tags$hr(style = "margin-bottom: 60px;"),
        fluidRow(
          column(4, 
                 plotOutput("output_freqOutput_stat")
          ),
          column(4, 
                 plotOutput("output_freqGrant")   
          ),
          column(4, 
                 plotOutput("output_freqIRB")   
          )
        )
      ),
      
      
      
      
      tabPanel(
      ### Navigator ----
        "Insight Navigator",
        tabsetPanel(
          tabPanel(
            'Student and Faculty',
            fluidRow(
              column(
                12,
      
              )
            )
          ),
          tabPanel(
            'Clients',
            fluidRow(
              column(
                12,
               
              )
            )
          ),
          tabPanel(
            'Project',
            fluidRow(
              column(
                12,
               
              )
            )
          )
        )
      ),
      
      
      
      
      
      tabPanel(
        ### Observe ----
        "Observe Tables",
          tabsetPanel(
            tabPanel(
              'Student and Faculty',
              fluidRow(
                column(
                  12,
                  h2('Table of Student and Staff Information'),
                  hr(),
                  DT::dataTableOutput("tableObs_stu")
                )
              )
            ),
            tabPanel(
              'Clients',
              fluidRow(
                column(
                  12,
                  h2('Table of Clients Information'),
                  hr(),
                  DT::dataTableOutput("tableObs_pi")
                )
              )
            ),
            tabPanel(
              'Project',
              fluidRow(
                column(
                  12,
                  h2('Table of Project Information'),
                  hr(),
                  DT::dataTableOutput("tableObs_prj")
                )
              )
            )
          )
        ),
      
      
      
      
      
      
      ### Input Data ----
      navbarMenu(
        "Data Input",
        tabPanel(
          "Student and Staff",
          fluidPage(
            tabsetPanel(
              tabPanel(
                "Input New Information",
                sidebarLayout(
                  sidebarPanel(
                    textInput("v_number", "Enter V Number: "),
                    textInput("name", "Enter Name: "),
                    textInput("email", "Enter Email: "),
                    selectInput("gender", "Select Gender: ", choices = c("Male", "Female", "Other"), multiple = FALSE),
                    selectInput("highest_degree", "Select Highest Degree: ", choices = c("Bachelor", "Master", "PhD", "MD", "Other"), multiple = FALSE),
                    selectInput("title", "Select Title: ", choices = c("Assistant Professor", "Associate Professor", "Professor", "Student", "Staff", "Other"), multiple = FALSE),
                    actionButton("insertBtn", "Insert into Table"),
                  ),
                  mainPanel(
                    textOutput("status1"),
                    tableOutput("tableOutput_stu.stf")
                  )
                )
              ),
              tabPanel(
                "Update Information",
                sidebarLayout(
                  sidebarPanel(
                    selectInput("up.v_number", " Select V Number to Update:  ",
                                choices = dbGetQuery(mysqlconnection, "SELECT V_number FROM student_faculty WHERE title = 'Student';")),
                    textInput("up.name", "Enter Updated Name: "),
                    textInput("up.email", "Enter Updated Email: "),
                    selectInput("up.gender", "Select Updated Gender: ", choices = c("Male", "Female", "Other"), multiple = FALSE),
                    selectInput("up.highest_degree", "Select Updated Highest Degree: ", choices = c("Bachelor", "Master", "PhD", "MD", "Other"), multiple = FALSE),
                    selectInput("up.title", "Select Updated Title: ", choices = c("Assistant Professor", "Associate Professor", "Professor", "Student", "Staff"), multiple = FALSE),
                    actionButton("insertUpBtn", "Update into Table"),
                  ),
                  mainPanel(
                    textOutput("status1.up"),
                    tableOutput("tableOutput_stu.stf_update")
                  )
                )
              ),
              tabPanel(
                "Remove Information",
                sidebarLayout(
                  sidebarPanel(
                    selectInput("rm.v_number", " Select V Number to Remmove:  ",
                                choices = dbGetQuery(mysqlconnection, "SELECT V_number FROM student_faculty WHERE title = 'Student';")),
                    actionButton("removeButton", "Remove from Table"),
                  ),
                  mainPanel(
                    textOutput("status1.rm"),
                    tableOutput("tableOutput_stu.stf_rm")
                  )
                )
              )
            )
          )
        ),
        
        
        
      
        tabPanel(
          "Project",
          fluidPage(
            tabsetPanel(
              tabPanel(
                "Input New Project",
                sidebarLayout(
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
              tabPanel(
                "Assign New Project",
                sidebarLayout(
                  sidebarPanel(
                    selectInput("prj_id_assign", "Select Project ID: ",
                                choices = dbGetQuery(mysqlconnection, "SELECT project_id FROM project;")),
                    selectInput("v_number_assign", "Select Assigned Student V Number: ",
                                choices = dbGetQuery(mysqlconnection, "SELECT V_number FROM student_faculty WHERE title = 'Student';")),
                    selectInput("super1_id", "Select Supervisor 1 ID: ",
                                choices = dbGetQuery(mysqlconnection, "SELECT V_number FROM student_faculty WHERE title = 'Student' or 'Staff';")),
                    selectInput("super2_id", "Select Supervisor 2 ID: ",
                               choices = dbGetQuery(mysqlconnection, "SELECT V_number FROM student_faculty WHERE title IN ('Assistant Professor', 'Associate Professor', 'Professor');")),
                    actionButton("insertPrjAssign", "Insert into Table"),
                    tableOutput("tableInfor_stu")
                  ),
                  mainPanel(
                    textOutput("status2b_assign"),
                    tableOutput("tableOutput_Aprj")
                  )
                )
              ),
              tabPanel(
                "Update New Project",
                sidebarLayout(
                  sidebarPanel(
                    selectInput("prj_id.up", "Select Project ID to Update: ",
                                choices = dbGetQuery(mysqlconnection, "SELECT project_id FROM project;")),
                    textInput("prj_title.up", "Enter Updated Project Title: "),
                    dateInput("date_received.up", label = "Enter Updated Date Received (yyyy-mm-dd): ",
                              value = "2021-01-01"),
                    dateInput("deadline.up", label = "Enter Updated Deadline Received (yyyy-mm-dd): ",
                              value = "2021-01-01"),
                    selectInput("type.up", "Select Updated Project Type: ", choices = c("Consulting", "Project")),
                    actionButton("updatePrj", "Update Table"),
                ),
                  mainPanel(
                    textOutput("status2c.up"),
                    tableOutput("tableOutput_Upprj")
                    )
                  )
                ),
              tabPanel(
                "Update Project Assignment",
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
            ),
            tabPanel(
              "Remove Project",
              sidebarLayout(
                sidebarPanel(
                  selectInput("prj_id.rm", "Select Project ID to Remove: ",
                              choices = dbGetQuery(mysqlconnection, "SELECT project_id FROM project;")),
                  actionButton("rmPrjAssign", "Remove Project"),
                ),
                mainPanel(
                  textOutput("status2e.rm"),
                  tableOutput("tableOutput_Rmprj")
                )
              )
            )
          )
        )
      )
    )
  )  
  })
  
  
  ### Output ----
  
  
  # Output Overview for Number of project
  output$value1 <- renderUI({
    sum_prj = nrow(dbGetQuery(mysqlconnection, "SELECT project_id FROM project;"))
    tagList(
      tags$style(HTML('.custom-value-box { 
                     background-color: lightblue; 
                     text-align: center; 
                     color: black;
                     border-radius: 10px; 
                     box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
                   }')),
      div(
        class = 'custom-value-box',
        style = 'width: 300px; height: 150px; padding: 20px;',  # Adjust width, height, and padding here
        h3(formatC(sum_prj, format = "d", big.mark = ',')),
        p("Total number of Projects")
      )
    )
  })
  
  # Output Overview for Number of PI
  output$value2 <- renderUI({
    sum_client = nrow(dbGetQuery(mysqlconnection, "SELECT v_number FROM client;"))
    tagList(
      tags$style(HTML('.custom-value-box2 { 
                     background-color: #d8bfd8; 
                     text-align: center; 
                     color: black;
                     border-radius: 10px; 
                     box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
                   }')),
      div(
        class = 'custom-value-box2',
        style = 'width: 300px; height: 150px; padding: 20px;',  # Adjust width, height, and padding here
        h3(formatC(sum_client, format = "d", big.mark = ',')),
        p("Total number of Collaborators ")
      )
    )
  })
  
  # Output Overview for Number of project
  output$value3 <- renderUI({
    sum_stu = nrow(dbGetQuery(mysqlconnection, "SELECT v_number FROM student_faculty WHERE title = 'Student';"))
    tagList(
      tags$style(HTML('.custom-value-box3 { 
                     background-color: pink; 
                     text-align: center; 
                     color: black;
                     border-radius: 10px; 
                     box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
                   }')),
      div(
        class = 'custom-value-box3',
        style = 'width: 300px; height: 150px; padding: 20px;',  # Adjust width, height, and padding here
        h3(formatC(sum_stu, format = "d", big.mark = ',')),
        p("Total number of Working Students")
      )
    )
  })
  
  # Output for Overview Project status over time
  output$output_freqStatus_time <- renderPlot({
    table <- dbGetQuery(mysqlconnection, "SELECT * FROM project_progress")
    table$month <- format(as.Date(table$date), "%Y-%m")
    
    ggplot(table, aes(x = month, fill = status)) +
      geom_bar() +
      labs(title = "Project Status Frequency by Month", x = "Month - Year", y = "Frequency") +
      theme_minimal() +
      scale_fill_brewer(palette = "Pastel1")
  })
    
  # Output for Overview Project status current
  output$output_freqStatus_current <- renderPlot({
    table <- dbGetQuery(mysqlconnection, "SELECT * FROM project_progress AS t
                        WHERE t.date = (SELECT max(t2.date) from project_progress AS t2 where t2.project_id = t.project_id);")
    
    table.status <- data.frame(table(table$status))
    names(table.status)[1] <- c('Status')
    # Calculate percentages
    percentages <- table.status$Freq / sum(table.status$Freq) * 100
    
    library(scales)
    ggplot(table.status, aes(x = Freq, y = percentages, fill = Status)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      geom_text(aes(label = paste0(round(percentages), "%")),
                position = position_stack(vjust = 0.5)) +
      theme_minimal() +
      scale_fill_brewer(palette = "Pastel1") +
      ggtitle("Latest Projects Status") +
      theme(axis.text.y = element_blank(), axis.title.y = element_blank())
  })
    
  # Output for Overview frequency of output
  output$output_freqOutput_stat <- renderPlot({
    table <- dbGetQuery(mysqlconnection, "SELECT DISTINCT category, project_id FROM output;")
    
    table.cat <- data.frame(table(table$category))
    names(table.cat)[1] <- 'Output'
    # Calculate percentages
    percentages <- table.cat$Freq / sum(table.cat$Freq) * 100
    
    library(scales)
    ggplot(table.cat, aes(x = "", y = percentages, fill = Output)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      geom_text(aes(label = paste0(round(percentages), "%")),
                position = position_stack(vjust = 0.5)) +
      theme_minimal() +
      scale_fill_brewer(palette = "Pastel1") +
      ggtitle("Project Output") +
      theme(axis.text.y = element_blank(), axis.title.y = element_blank())
  })
  
  # Output for Overview frequency of Grant Status
  output$output_freqGrant <- renderPlot({
    table <- dbGetQuery(mysqlconnection, "SELECT DISTINCT grant_status, project_id FROM grants;")
    
    table.gr <- data.frame(table(table$grant_status))
    names(table.gr)[1] <- c('Status')
    # Calculate percentages
    percentages <- table.gr$Freq / sum(table.gr$Freq) * 100
    
    library(scales)
    ggplot(table.gr, aes(x = "", y = percentages, fill = Status)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      geom_text(aes(label = paste0(round(percentages), "%")),
                position = position_stack(vjust = 0.5)) +
      theme_minimal() +
      scale_fill_brewer(palette = "Pastel1") +
      ggtitle("Grant Status") +
      theme(axis.text.y = element_blank(), axis.title.y = element_blank())
  })
  
  # Output for Overview frequency of IRB
  output$output_freqIRB <- renderPlot({
    table <- dbGetQuery(mysqlconnection, "SELECT DISTINCT IRB_status, project_id FROM grants;")
    
    table.irb <- data.frame(table(table$IRB_status))
    names(table.irb)[1] <- c('Status')
    # Calculate percentages
    percentages <- table.irb$Freq / sum(table.irb$Freq) * 100
    
    library(scales)
    ggplot(table.irb, aes(x = "", y = percentages, fill = Status)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      geom_text(aes(label = paste0(round(percentages), "%")),
                position = position_stack(vjust = 0.5)) +
      theme_minimal() +
      scale_fill_brewer(palette = "Pastel1") +
      ggtitle("IRB Status") +
      theme(axis.text.y = element_blank(), axis.title.y = element_blank())
  })
    
  
  
  
  
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
  
  
  
  
  
  
  
  # Output of Table of Student and Faculty
  observeEvent(input$insertBtn, {
    # Get input values
    v_number <- input$v_number
    name <- input$name
    gender <- input$gender
    email <- input$email
    highest_degree <- input$highest_degree
    title <- input$title
    
    # Insert into SQL table
    query <- paste0("INSERT INTO student_faculty (V_number, name, sex, email, highest_degree, title) VALUES ('",
                    v_number, "', '", name, "', '", gender, "', '", email, "', '", highest_degree, "', '", title, "')")
    dbExecute(mysqlconnection, query)
    
    # Display status
    output$status1 <- renderText("Data inserted into Student and Faculty Information table.")
    # Display updated table
    output$tableOutput_stu.stf <- renderTable({
      # Retrieve and display data
      query_select <- "SELECT * FROM student_faculty"
      dbGetQuery(mysqlconnection, query_select)
    })
  })
  
  observeEvent(input$insertUpBtn, {
    # Get input values
    v_number <- input$up.v_number
    name <- input$up.name
    gender <- input$up.gender
    email <- input$up.email
    highest_degree <- input$up.highest_degree
    title <- input$up.title
    
    # Update SQL table
    query <- paste0("UPDATE student_faculty SET V_number = '", input$up.v_number, "', name = '", name, "', sex = '", gender, "', email = '", email, "', highest_degree = '", highest_degree, "', title = '", title,
                    "' WHERE V_number = '", v_number, "'")
    
    dbExecute(mysqlconnection, query)
    
    # Display status
    output$status1.up <- renderText("Data updated into Student and Faculty Information table.")
    # Display updated table
    output$tableOutput_stu.stf_update <- renderTable({
      # Retrieve and display data
      query_select <- "SELECT * FROM student_faculty"
      dbGetQuery(mysqlconnection, query_select)
    })
  })
 
  observeEvent(input$removeButton, {
    # Get input values
    v_number <- input$rm.v_number
    
    # Delete from SQL table
    query <- paste0("DELETE FROM student_faculty WHERE V_number = '", v_number, "'")
    dbExecute(mysqlconnection, query)
    
    # Display status
    output$status1.rm <- renderText("Data removed from Student and Faculty Information table.")
    # Display updated table
    output$tableOutput_stu.stf_rm <- renderTable({
      # Retrieve and display data
      query_select <- "SELECT * FROM student_faculty"
      dbGetQuery(mysqlconnection, query_select)
    })
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
  
  # Output of Table of Assigning Project
  observeEvent(input$insertPrjAssign, {
    # Get input values
    prj_id_assign <- input$prj_id_assign
    v_number_assign <- input$v_number_assign
    super1_id <- input$super1_id
    super2_id <- input$super2_id
    
    # Insert into SQL table
    query <- paste0("INSERT INTO work_on (v_number, supervisor1_id, supervisor2_id, project_id) VALUES ('",
                    v_number_assign, "', '", super1_id, "', '", super2_id, "', '", prj_id_assign, "')")
    dbExecute(mysqlconnection, query)
    
    # Display status
    output$status2b_assign <- renderText("Data inserted into Work_on Table.")
    # Display updated table
    output$tableOutput_Aprj <- renderTable({
      # Retrieve and display data
      query_select <- "SELECT * FROM work_on"
      dbGetQuery(mysqlconnection, query_select)
    })
  })
  
  # Display information table
  observeEvent(input$insertPrjAssign, {
    output$tableInfor_stu <- renderTable({
      query_select <- "SELECT V_number, name, title FROM student_faculty"
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
  
  # Output of Table of Assigning Project
  observeEvent(input$updatePrjAssign, {
    # Get input values
    prj_id_assign <- input$prj_id_assign.up
    v_number_assign <- input$v_number_assign.up
    super1_id <- input$super1_id.up
    super2_id <- input$super2_id.up
    
    # Update into SQL Work on table
    query <- paste0("UPDATE work_on SET supervisor1_id = '", super1_id, "', supervisor2_id = '", super2_id, 
                    "' WHERE project_id = '", prj_id_assign, "'")
    dbExecute(mysqlconnection, query)
    
    # Display status
    output$status2d.up <- renderText("Data Updated into Work_on Table.")
    # Display updated table
    output$tableOutput_UpAprj <- renderTable({
      # Retrieve and display data
      query_select <- "SELECT * FROM work_on"
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

shinyApp(ui, server)

