
# Required packages -------------------------------------------------------

library(shiny)
library(DBI)
library(bigrquery)
library(dplyr)
library(tidyr)
library(ggplot2)
 
# DBI setup ---------------------------------------------------------------
  bq_auth(path = "../credentials.json")
  con <- dbConnect(
    bigquery(),
    project = "bigquery-public-data",
    dataset = "stackoverflow",
    billing = "stacktrace-321016"
  )
# Global constants --------------------------------------------------------
LANGUAGES <- c(
  R = "r",
  JavaScript = "javascript",
  HTML = "html",
  SQL = "sql",
  Python = "python",
  Java = "java",
  Bash = "bash",
  "C#" = "c#",
  Rust = "rust",
  Haskell = "haskell",
  PHP = "php",
  Ruby = "ruby",
  Assembly = "assembly",
  Go = "go",
  Julia = "julia",
  Scala = "scala"
)
TABLE_NAMES <- c(comments = "comments",
                 questions = "posts_questions",
                 answers = "posts_answers")

DATA_NAMES <- c("user", "questions", "answers", "comments")

# General ggplot theme
theme_standard <- ggplot2::theme(
  panel.background = element_rect(fill = "#FFECB3"),
  plot.background = element_rect(fill = "white", color = "white"),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  panel.grid.major.x = element_line(color = "gray93"),
  legend.background = element_rect(fill = "gray93"),
  plot.title = element_text(
    size = 17,
    family = "sans",
    face = "bold",
    vjust = 1.3
  ),
  plot.title.position = "plot",
  plot.subtitle = element_text(size = 10, family = "sans"),
  legend.title = element_text(
    size = 12,
    family = "sans",
    face = "bold"
  ),
  axis.title = element_text(
    size = 9,
    family = "sans",
    face = "bold"
  ),
  axis.text = element_text(size = 8, family = "sans"),
  strip.background = element_rect(color = "black", fill = "black"),
  strip.text.x = element_text(color = "white"),
  strip.text.y = element_text(color = "white"),
  legend.position = "bottom"
)
ggplot2::theme_set(theme_standard)
