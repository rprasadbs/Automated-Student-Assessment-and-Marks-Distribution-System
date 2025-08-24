# Load required libraries
library(shiny)
library(shinydashboard)
library(DBI)
library(RSQLite)

# Connect to SQLite database
db <- dbConnect(SQLite(), "teacher_portal.db")

# Create tables if they do not exist
dbExecute(db, "CREATE TABLE IF NOT EXISTS questions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    question TEXT,
    model_answer TEXT
)")

dbExecute(db, "CREATE TABLE IF NOT EXISTS mcq_questions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    question TEXT,
    option1 TEXT,
    option2 TEXT,
    option3 TEXT,
    option4 TEXT,
    correct_answer TEXT
)")

dbExecute(db, "CREATE TABLE IF NOT EXISTS student_marks (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    student_name TEXT,
    student_id TEXT,
    total_score REAL
)")

# UI for the dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Teacher Portal"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Post Questions", tabName = "post", icon = icon("plus")),
      menuItem("View Questions", tabName = "view", icon = icon("eye")),
      menuItem("View Marks", tabName = "marks", icon = icon("chart-bar")),
      menuItem("Clear All", tabName = "clear", icon = icon("trash"))
    )
  ),
  dashboardBody(
    tabItems(
      # Post Questions Tab
      tabItem(tabName = "post",
              fluidRow(
                box(title = "Post Open-ended Questions", status = "primary", solidHeader = TRUE, width = 12,
                    textInput("q1", "Question 1"),
                    textInput("a1", "Model Answer 1"),
                    textInput("q2", "Question 2"),
                    textInput("a2", "Model Answer 2"),
                    textInput("q3", "Question 3"),
                    textInput("a3", "Model Answer 3")
                ),
                box(title = "Post MCQ Questions", status = "warning", solidHeader = TRUE, width = 12,
                    textInput("mq1", "MCQ Question 1"),
                    textInput("o1_1", "Option 1"),
                    textInput("o1_2", "Option 2"),
                    textInput("o1_3", "Option 3"),
                    textInput("o1_4", "Option 4"),
                    textInput("a1_correct", "Correct Answer"),
                    textInput("mq2", "MCQ Question 2"),
                    textInput("o2_1", "Option 1"),
                    textInput("o2_2", "Option 2"),
                    textInput("o2_3", "Option 3"),
                    textInput("o2_4", "Option 4"),
                    textInput("a2_correct", "Correct Answer")
                ),
                actionButton("submit", "Post Questions", class = "btn-primary")
              )
      ),
      
      # View Questions Tab
      tabItem(tabName = "view",
              fluidRow(
                box(title = "Open-ended Questions", status = "success", solidHeader = TRUE, width = 12,
                    tableOutput("open_questions")
                ),
                box(title = "MCQ Questions", status = "info", solidHeader = TRUE, width = 12,
                    tableOutput("mcq_questions")
                )
              )
      ),
      
      # View Marks Tab
      tabItem(tabName = "marks",
              fluidRow(
                box(title = "Student Marks", status = "primary", solidHeader = TRUE, width = 12,
                    tableOutput("student_marks")
                )
              )
      ),
      
      # Clear All Questions Tab
      tabItem(tabName = "clear",
              fluidRow(
                box(title = "Delete All Questions", status = "danger", solidHeader = TRUE, width = 12,
                    actionButton("clear_db", "Clear All Questions", class = "btn-danger"),
                    textOutput("clear_msg")
                )
              )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  
  
  # Insert questions into database
  observeEvent(input$submit, {
    dbExecute(db, "INSERT INTO questions (question, model_answer) VALUES (?, ?)", params = list(input$q1, input$a1))
    dbExecute(db, "INSERT INTO questions (question, model_answer) VALUES (?, ?)", params = list(input$q2, input$a2))
    dbExecute(db, "INSERT INTO questions (question, model_answer) VALUES (?, ?)", params = list(input$q3, input$a3))
    
    dbExecute(db, "INSERT INTO mcq_questions (question, option1, option2, option3, option4, correct_answer) 
                   VALUES (?, ?, ?, ?, ?, ?)", params = list(input$mq1, input$o1_1, input$o1_2, input$o1_3, input$o1_4, input$a1_correct))
    
    dbExecute(db, "INSERT INTO mcq_questions (question, option1, option2, option3, option4, correct_answer) 
                   VALUES (?, ?, ?, ?, ?, ?)", params = list(input$mq2, input$o2_1, input$o2_2, input$o2_3, input$o2_4, input$a2_correct))
    
    showNotification("Questions posted successfully!", type = "message")
  })
  
  # Retrieve open-ended questions
  output$open_questions <- renderTable({
    dbGetQuery(db, "SELECT question, model_answer FROM questions")
  })
  
  # Retrieve MCQ questions
  output$mcq_questions <- renderTable({
    dbGetQuery(db, "SELECT question, option1, option2, option3, option4, correct_answer FROM mcq_questions")
  })
  
  # Clear all questions
  observeEvent(input$clear_db, {
    dbExecute(db, "DELETE FROM questions")
    dbExecute(db, "DELETE FROM mcq_questions")
    dbExecute(db, "DELETE FROM student_marks")
    dbExecute(db, "DELETE FROM sqlite_sequence WHERE name='questions'")
    dbExecute(db, "DELETE FROM sqlite_sequence WHERE name='mcq_questions'")
    dbExecute(db, "DELETE FROM sqlite_sequence WHERE name='student_marks'")
    
    output$clear_msg <- renderText("All questions cleared successfully!")
    showNotification("All questions deleted!", type = "error")
  })
  
  # Retrieve student marks
  output$student_marks <- renderTable({
    dbGetQuery(db, "SELECT student_name, student_id, total_score FROM student_marks")
  })
}

# Run the application
shinyApp(ui = ui, server = server)