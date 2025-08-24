# Load necessary libraries
library(shiny)
library(shinydashboard)
library(DBI)
library(RSQLite)
library(tidyverse)
library(igraph)

# Connect to SQLite database
db <- dbConnect(SQLite(), "teacher_portal.db")

# -------------------- Scoring System --------------------

sanitize_response <- function(text) {
  text %>% 
    tolower() %>% 
    str_remove_all("[[:punct:]]") %>% 
    str_remove_all("[[:digit:]]") %>% 
    str_squish()
}

build_concept_web <- function(texts) {
  term_pairs <- map_dfr(texts, ~{
    words <- unlist(str_split(.x, " "))
    if(length(words) > 1) t(combn(words, 2)) %>% as_tibble()
  }) %>% 
    count(V1, V2) %>% 
    filter(n > 1)
  
  graph_from_data_frame(term_pairs, directed = FALSE) %>% 
    set_edge_attr("weight", value = term_pairs$n)
}

calculate_concept_power <- function(graph) {
  if (vcount(graph) == 0 || ecount(graph) == 0) {
    return(data.frame(name = character(0), importance = numeric(0)))
  }
  
  V(graph)$importance <- (
    0.4 * scale(betweenness(graph)) + 
      0.3 * scale(closeness(graph)) + 
      0.3 * scale(eigen_centrality(graph)$vector)
  )
  
  importance_df <- get.data.frame(graph, what = "vertices") %>% 
    select(name, importance) %>% 
    arrange(desc(importance))
  
  return(importance_df)
}

assess_response <- function(response, model_terms, concept_power) {
  response_terms <- unlist(str_split(response, " "))
  
  concept_match <- sum(concept_power$importance[match(response_terms, concept_power$name)], na.rm = TRUE)
  concept_match <- plogis(concept_match)
  
  matched_terms <- response_terms[response_terms %in% model_terms]
  order_penalty <- if(length(matched_terms) > 1) {
    model_pos <- match(matched_terms, model_terms)
    response_pos <- match(matched_terms, response_terms)
    1 - (sqrt(mean((model_pos - response_pos)^2)) / length(model_terms))
  } else 0.7  
  
  key_concepts <- model_terms  
  completeness <- sum(key_concepts %in% response_terms) / length(key_concepts)
  
  misconceptions <- c("photosynthesis", "brain", "store water", "dna", "membrane")
  error_penalty <- 1 - (sum(str_detect(response, misconceptions)) * 0.1)
  
  raw_score <- (0.4 * concept_match) + (0.3 * order_penalty) + (0.2 * completeness)
  final_score <- raw_score * error_penalty
  
  pmax(pmin(final_score, 1), 0)
}

# -------------------- Shiny Dashboard UI --------------------
ui <- dashboardPage(
  dashboardHeader(title = "Student Exam Portal"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Take Test", tabName = "test", icon = icon("pencil")),
      menuItem("Results", tabName = "results", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      # Exam Tab
      tabItem(tabName = "test",
              fluidRow(
                box(title = "Student Details", status = "primary", width = 12,
                    textInput("student_name", "Enter Name"),
                    textInput("student_id", "Enter ID")
                ),
                box(title = "Answer Open-ended Questions", status = "success", width = 12,
                    uiOutput("open_questions")
                ),
                box(title = "Answer MCQ Questions", status = "info", width = 12,
                    uiOutput("mcq_questions")
                ),
                actionButton("submit_answers", "Submit Test", class = "btn-primary")
              )
      ),
      
      # Results Tab
      tabItem(tabName = "results",
              fluidRow(
                box(title = "Student Information", status = "primary", width = 12,
                    textOutput("student_info")
                ),
                box(title = "Scores Per Question", status = "warning", width = 12,
                    uiOutput("question_scores")
                ),
                box(title = "Total Score", status = "success", width = 12,
                    textOutput("final_score")
                )
              )
      )
    )
  )
)

# -------------------- Shiny Server --------------------
server <- function(input, output, session) {
  
  # Retrieve Open-ended Questions
  open_questions <- reactive({
    dbGetQuery(db, "SELECT id, question, model_answer FROM questions")
  })
  
  output$open_questions <- renderUI({
    questions <- open_questions()
    q_list <- lapply(1:nrow(questions), function(i) {
      textAreaInput(paste0("q_", questions$id[i]), 
                    label = paste("Q", i, ":", questions$question[i]), 
                    width = "100%", height = "80px")
    })
    do.call(tagList, q_list)
  })
  
  # Retrieve MCQ Questions
  mcq_questions <- reactive({
    dbGetQuery(db, "SELECT id, question, option1, option2, option3, option4, correct_answer FROM mcq_questions")
  })
  
  output$mcq_questions <- renderUI({
    questions <- mcq_questions()
    q_list <- lapply(1:nrow(questions), function(i) {
      radioButtons(paste0("mcq_", questions$id[i]), 
                   label = paste("Q", i, ":", questions$question[i]), 
                   choices = c(questions$option1[i], questions$option2[i], 
                               questions$option3[i], questions$option4[i]),
                   selected = "")
    })
    do.call(tagList, q_list)
  })
  
  # Evaluate Answers
  observeEvent(input$submit_answers, {
    student_name <- input$student_name
    student_id <- input$student_id
    
    if (student_name == "" || student_id == "") {
      showNotification("Please enter your name and ID!", type = "error")
      return()
    }
    
    total_score <- 0
    scores_per_question <- list()
    
    # Evaluate Open-ended Questions
    questions <- open_questions()
    if (nrow(questions) > 0) {
      texts <- sanitize_response(questions$model_answer)
      graph <- build_concept_web(texts)
      concept_power <- calculate_concept_power(graph)
      
      for (i in 1:nrow(questions)) {
        student_response <- sanitize_response(input[[paste0("q_", questions$id[i])]])
        model_terms <- unlist(str_split(texts[i], " "))
        score <- assess_response(student_response, model_terms, concept_power)
        scores_per_question[[paste0("Q", i, " Score")]] <- score
        total_score <- total_score + score
      }
    }
    
    # Evaluate MCQ Questions
    mcq_data <- mcq_questions()
    if (nrow(mcq_data) > 0) {
      for (i in 1:nrow(mcq_data)) {
        selected_answer <- input[[paste0("mcq_", mcq_data$id[i])]]
        score <- if (!is.null(selected_answer) && selected_answer == mcq_data$correct_answer[i]) 1 else 0
        scores_per_question[[paste0("Q", i + nrow(questions), " Score")]] <- score
        total_score <- total_score + score
      }
    }
    
    # Save Student Scores
    dbExecute(db, "INSERT INTO student_marks (student_name, student_id, total_score) VALUES (?, ?, ?)",
              params = list(student_name, student_id, total_score))
    
    # Display Student Info
    output$student_info <- renderText({
      paste("Name:", student_name, "| ID:", student_id)
    })
    
    # Display Scores Per Question
    output$question_scores <- renderUI({
      score_texts <- lapply(names(scores_per_question), function(q) {
        p(paste(q, ":", round(scores_per_question[[q]], 2)))
      })
      do.call(tagList, score_texts)
    })
    
    # Display Total Score
    output$final_score <- renderText({
      paste("Total Score:", round(total_score, 2))
    })
    
    showNotification(paste("Test submitted! Your final score is", round(total_score, 2)), type = "message")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
