# Automated-Student-Assessment-and-Marks-Distribution-System
This project is a Shiny web application for teachers and students to manage and evaluate exam questions.  
It contains two dashboards:

- **Teacher Portal (`teacher_api.R`)** – Teachers can post open-ended and MCQ questions, view them, monitor student marks, and clear data.
- **Student Portal (`student_dashboard.R`)** – Students can take tests (open-ended + MCQs), submit answers, and receive evaluated scores.

The system uses an **SQLite database (`teacher_portal.db`)** to store questions and student marks.

---

## 📂 Project Structure
marks_distribution/
│── .RData # R workspace data (optional, can be ignored in GitHub)
│── .Rhistory # R command history (optional, can be ignored in GitHub)
│── teacher_portal.db # SQLite database storing questions & marks
│── teacher_api.R # Teacher-facing dashboard
│── student_dashboard.R # Student-facing dashboard


---

## 🚀 Features

### 👩‍🏫 Teacher Portal
- Post **open-ended questions** with model answers
- Post **MCQ questions** with options and correct answers
- View all posted questions
- View student marks
- Clear/reset all questions and marks

### 👨‍🎓 Student Portal
- Enter student details (Name & ID)
- Answer **open-ended questions** in free text
- Attempt **MCQs**
- Receive automatic evaluation:
  - Open-ended answers are scored using **concept web analysis**
  - MCQs are scored based on correct answers
- Get detailed score breakdown and total marks

---

## ⚙️ Installation & Setup

### 1. Install R and RStudio
Download and install from [CRAN](https://cran.r-project.org/) and [RStudio](https://posit.co/download/rstudio/).

### 2. Install Required Packages
In R console, run:

```r
install.packages(c("shiny", "shinydashboard", "DBI", "RSQLite", 
                   "tidyverse", "igraph"))

shiny::runApp("teacher_api.R")

shiny::runApp("student_dashboard.R")

Both apps will open in your default web browser.

Database Schema

The project uses teacher_portal.db (SQLite).
It contains 3 tables:

questions

id (INTEGER, PRIMARY KEY)

question (TEXT)

model_answer (TEXT)

mcq_questions

id (INTEGER, PRIMARY KEY)

question (TEXT)

option1 (TEXT)

option2 (TEXT)

option3 (TEXT)

option4 (TEXT)

correct_answer (TEXT)

student_marks

id (INTEGER, PRIMARY KEY)

student_name (TEXT)

student_id (TEXT)

total_score (REAL)

Scoring System (Student Dashboard)

Open-ended questions:

Uses concept web graph (via igraph)

Calculates:

Concept Match

Order Penalty

Completeness

Error Penalty

Final score is normalized between 0 and 1

MCQ questions:

1 point for correct answer

0 for incorrect answer

📊 Example Workflow

Teacher logs in → posts 3 open-ended + 2 MCQs

Student logs in → answers the posted questions

System evaluates and saves marks to database

Teacher can view all student scores from Teacher Portal

✅ Future Improvements

Authentication system for teacher & students

Export reports in PDF/CSV

Enhanced NLP-based scoring for open-ended answers

Graphical analytics of marks distribution
