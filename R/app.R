library(shiny)
library(DBI)
library(bigrquery)
library(dplyr)
library(tidyr)
library(ggplot2)

  bq_auth(path = "../credentials.json")
  source("utils.R")
  con <- dbConnect(
    bigquery(),
    project = "bigquery-public-data",
    dataset = "stackoverflow",
    billing = "stacktrace-321016"
  )
LANGUAGES <- c(
  R = "r",
  JavaScript = "javascript",
  HTML = "html",
  SQL = "sql",
  Python = "python",
  Java = "java",
  Bash = "bash",
  "C#" = "c#"
)
TABLE_NAMES <- c(comment = "comments",
                 question = "posts_questions",
                 answer = "posts_answers")

id <- NULL
ui <- navbarPage(
  "Navbar",
  tabPanel(
    "Users",
    sidebarLayout(
      sidebarPanel(div(id = "textInput",
        textInput(inputId = "username", label = "Enter a username:"),
                   actionButton("username_button", "Search")
      )),
      mainPanel(
        fluidRow(column(6, tableOutput("table_user")), column(
          6,
          plotOutput(
            "plot_contributions"
          )
        )),
        fluidRow(
          column(6, htmlOutput(
            "text_user_top_rated"
          )),
          column(6, plotOutput(
            "plot_user_activity"
          ))
        ),
        tableOutput("table_tags")
      )
    )
  ),
  tabPanel(
    "Languages",
    sidebarPanel(
      radioButtons(
        inputId = "language",
        "Select language",
        choices = LANGUAGES,
        selected = character(0)
      )
    ),
    mainPanel(
      fluidRow(
        column(
          6
        ),
        column(
          6,
          plotOutput("plot_language_activity")
        )
      ),
      fluidRow(column(
        6,
        textOutput("text_top_answer")
      ), column(
        6,
        plotOutput(
          "plot_tags_pairs"
        )
      )),
      fluidRow(
        column(
          6,
          plotOutput(
            "plot_bigrams"
          )
        ), # replace this, no comment tags)),
        column(
          6,
          tableOutput("table_language_stats")
        )
      )
    ),
    inverse = TRUE,
    collapsible = TRUE
  )
)
server <- function(input, output, session) {
 
  id <- reactiveVal(integer()) 
  user_key <- eventReactive(input$username_button, {
    data <- dbGetQuery(
          con,
          "SELECT id, display_name AS Name, creation_date AS Date,
                         location AS Location, reputation AS Reputation
                      FROM users WHERE display_name = @x",
          parameters = list(x = input$username)
        ) %>% 
        mutate(Date = format(Date, "%B %d, %Y"))
      
    validate_df(data, input$username, "No user named") 
    data
    }
  )
  
  user_filtered <- eventReactive(id(), {
    user_key()[user_key()$id == id(),]
  })
  user_data <- eventReactive(user_filtered(), {
      questions <-
        dbGetQuery(
          con,
          "SELECT id, title AS Title, creation_date AS Date, tags AS Tags, body AS Text,
                        score AS Score
                    FROM posts_questions WHERE owner_user_id = @x",
          parameters = list(x = user_filtered()$id)
        )
      answers <-
        dbGetQuery(
          con,
          "SELECT id, title AS Title, creation_date AS Date, tags AS Tags, body AS Text, score AS Score
                    FROM posts_answers WHERE owner_user_id = @x",
          parameters = list(x = user_filtered()$id)
        )
      comments <-
        dbGetQuery(
          con,
          "SELECT id, creation_date AS Date, score AS Score, text AS Text,
                       FROM comments WHERE user_id = @x",
          parameters = list(x = user_filtered()$id)
        )
      comments[, c("Title", "Tags")] <-
        NA_character_
      data <- dplyr::bind_rows(
        question = questions,
        answer = answers,
        comment = comments,
        .id = "Type"
      )
      # Get id and submission type of user's higherst rated,
      # tiebreaking by date
      top_type <-
        data[which.max(data$Score), c("id", "Type", "Date", "Score")]
      top_type <-
        top_type[which.max(top_type$Date), c("id", "Type", "Score")]
      attr(data, "top_type") <- top_type
      
    error <-  validate_df(data, input$username, "No submissions found for")
    if(!is.null(error)){
      error
    }else{
      data
    }
  }
  )
  
  excess_users <- observeEvent(user_key(), {
    if(nrow(user_key()) > 1){
      insertUI(
        selector= "#textInput",
        ui = tags$div(
          radioButtons("select_user", 
                       choices = setNames(user_key()$id, user_key()$Name),
                       selected = character(0),
                       label = sprintf("%d users named %s. Choose one:", 
                       nrow(user_key()), input$username)),
        id = "userSelect"
        ),
        where = "afterEnd"
      )
    }else{
      id(user_key()$id)
    }
  }) 
 
  disambiguate_user <- observeEvent(input$select_user, {
    id(input$select_user)
    removeUI(selector = "#userSelect", immediate = TRUE)
  }) 
  
  output$table_user <-
    renderTable({
      user_filtered()[, c("Name", "Date", "Location", "Reputation")]
    })
  
  output$text_user_top_rated <-
    renderText({
      type <- attr(user_data(), "top_type")
      print(type)
      query <- paste("SELECT",
                     dbQuoteIdentifier(con, ifelse(type$Type == "comment", "text", "body")),
                                       "FROM",
                     dbQuoteIdentifier(con, unname(TABLE_NAMES[type$Type])),
                          "WHERE id = @x")
      text <- dbGetQuery(
        con,
          query, 
        parameters = list(
          x = type$id
        )
      )[[1]]
      validate(need(text, "No text found to display"))
      text
    })
  
  output$plot_contributions <-
    renderPlot({
      user_data() %>%
        ggplot(aes(x = Type, fill = Type)) +
        geom_bar(position = "dodge")
    })
  output$plot_user_activity <-
    renderPlot({
      time_plot(user_data(), Contributions)
    })
  output$table_tags <-
    renderTable({
      raw_tags <- user_data()$Tags[complete.cases(user_data()$Tags)]
      validate(need(raw_tags, paste("No tags associated with", input$username)))
      all_tags <-
        unlist(lapply(raw_tags, function(x) {
          unlist(strsplit(x, "\\|"))
        }))
      tab <- table(all_tags)
      tab <-
        sort(tab, decreasing = TRUE)[1:(min(length(tab), 5))]
      list2DF(as.list(tab))
    })
  
  language_query <- eventReactive(input$language, {
        strings <-
          escape_like_query(input$language, "|", con)
        sql <-
          c(" WHERE (tags LIKE ", " OR tags LIKE ", " OR tags LIKE ", " OR tags = ")
          paste0(combine_vectors(sql, strings), ")")
  }, ignoreInit = TRUE,
  ignoreNULL = TRUE)
  
  language_data <-
    eventReactive(language_query(),
      {
        # Guard against SQL injection
          data <- dbGetQuery(
            con,
            paste0(
              "SELECT id, title AS Title, creation_date AS Date, tags AS Tags,
                        score AS Score
                    FROM posts_questions",
              language_query()
            )
          )
    error <-  validate_df(data, input$language, "No questions found for language")
    if(!is.null(error)){
      error
    }else{
      data
    }
      }
    )

  output$plot_language_activity <-
    renderPlot({
      time_plot(language_data(), Questions, group = FALSE)
    })
  output$plot_tags_pairs <- renderPlot({
    processed <- language_data() %>%
      select(id, Tags) 
    validate_df(processed, input$language, "No tags found for language")
      processed <- processed %>% 
      separate_rows(Tags, sep = "\\|") %>% #Keep up to 20 most common tags
      filter(Tags != input$language & Tags %in% names(sort(table(Tags), decreasing = TRUE)[1:20])) %>% 
      group_by(id) %>% 
      pivot_wider(names_from = Tags, values_from = Tags, values_fill = 0,
                  values_fn = function(x) 1) %>% 
      ungroup() %>% 
      select(-id) %>%
      as.matrix() %>% 
        {t(.) %*% .} 
      
        processed %>% as.data.frame() %>% 
      mutate(x = rownames(.)) %>% 
      pivot_longer(-x, names_to = "y", values_to = "Count") %>% 
      mutate(across(-Count, ~as.factor(paste0('"', .x, '"'))),
      y = factor(y, rev(levels(y)))) %>% 
      ggplot(aes(x = x, y = y, fill = Count))+
      geom_tile() +
      scale_fill_gradient(low = "#E5D9B4", high = "#3B3A37") +
      labs(title = paste("Tag Co-occurrences in", names(LANGUAGES)[LANGUAGES ==input$language],
                         "Questions"), subtitle = "Diagonal indicates total count",
          x = "", y = "") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })

  output$plot_bigrams <- renderPlot({
    sample_rows <- language_data()$id[sample(seq_along(language_data()$id), min(2000, nrow(language_data()$id)))]
    text <- dbGetQuery(con, paste0("SELECT body AS text FROM posts_questions", language_query(),
                       " AND id IN UNNEST(@x)"), parameters = list(x = sample_rows)) %>% 
      mutate(text = extract_text(text))
    
    bigrams <- text %>% 
      # gsub('[:;\\\\"/()]', "", x= .) %>% 
      # gsub("[?!,.](?=\\s|$)", "", x = ., perl = TRUE) %>% 
      # sapply(., function(x) unlist(strsplit(x, "\\s+"), recursive = FALSE), USE.NAMES = FALSE) %>%
      # unlist() %>% 
      tidytext::unnest_tokens(word1, text, token = "words") %>% 
      mutate(word1 = ifelse(word1 %in% tidytext::stop_words$word | grepl("\\d", x =  word1), NA_character_, word1),
             word2 = lead(word1)) %>%   
       filter(rowSums(is.na(.))  == 0)  %>% 
      unite("bigram", word1, word2, sep = " ") %>% 
      count(bigram, sort = TRUE, name = "Count") %>% 
      mutate(Proportion = Count / sum(Count), bigram = paste0('"', bigram, '"') %>% {factor(., levels = reorder(., Proportion))}) %>% 
      filter(Proportion >= .003) %>% 
      mutate(ymax = cumsum(Proportion), ymin = lag(ymax, default = 0))
      validate_df(bigrams, input$language, "Not enough content words found for language")
        #Donut plot
        # See https://www.r-graph-gallery.com/128-ring-or-donut-plot.html
        bigrams %>% ggplot(aes(xmin = 3.5, xmax = 5, ymin = ymin, ymax = ymax, fill = bigram)) +
    geom_rect(color = "black") +
      coord_polar(theta = "y") +
      xlim(c(2, 5.5)) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(x = "", y = "Percentage",
           title = "Most Common Bigrams as Percentage of Total",
           subtitle = "(sample of 2,000)",
           fill = "Bigram") +
      guides(fill = guide_legend(title.position = "top", title.hjust = .5)) +
      theme(legend.position = "bottom",# axis.line = element_blank(),
            axis.text.y  = element_blank(),
            axis.ticks.y = element_blank(),
            legend.box.margin = margin(t = -10),
            panel.grid = element_blank())
  })
}
shinyApp(ui, server)
