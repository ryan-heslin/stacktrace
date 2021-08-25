source("global.R")
source("utils.R")
ui <- navbarPage(
  title = "stacktrace",
  theme = bslib::bs_theme(
    bg = "#B1E6F3", fg = "#545F32",
    primary = "#FF9200", secondary = "#CCC000", base_font = bslib::font_google("Ubuntu"),
    font_scale = 1.2, `enable-shadows` = TRUE
  ),
  tags$head((
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  )),
  tabPanel("About",
           tags$body(
            tags$p(tags$strong("Stacktrace"), class = "header"),
             tags$div(id = "about",
           tags$br(),
           tags$p("It's getting late, the project is behind schedule, and you've hit
                  a roadblock.",  tags$i("Surely
                  someone else must have dealt with this problem before,"),
                  "you think to yourself. You trawl increasingly arcane StackOverflow postings for
                  some clue, but reap only frustration and a steadily increasing
                  number of purple links."), tags$p(
             "If you're anything like me, you've spent more time than you care
             to admit this way (hopefully, finding,", tags$i("that one post"), "in the end).
             It's no fun to get stuck. But it occurred to me all that frustration must have
             at least generated some interesting data. So I used Google's", tags$a(href = "https://cloud.google.com/bigquery", "BigQuery API"), 
             "to send SQL queries to Stackoverflow's", tags$a(href = "https://www.kaggle.com/stackoverflow/stackoverflow",
                                                              "massive remote database")," of
             postings and metadata, then built this Shiny app so you could explore it."),
           
           tags$p("On the users tab, enter any StackOverflow username in the search bar to start. These aren't necessarily
                  unique, so you may be prompted to select a specific user. That will bring up plots
                  portraying the user's activity over time, tables of their basic data and most frequently
                  used tags, and their most highly upvoted post"),
           tags$p("Over on the Languages tab, you can select one of StackOverflow's more popular languages.
                  Then stacktrace will retrieve all questions tagged with the name of that language (it may
                  take a long time; there are a", tags$i("lot"), "of data). Then you'll see plots depicting
                  question volume for that language over time, as well as the most commonly associated pairs
                  of tags. Another plot shows the shares of the most common bigrams (pairs of words)
                  in a sample of questions. On the bottom is the highest-upvoted question for that language."),
           tags$p("I hope you enjoy finding out how others have spent their time on the site programmers love
                  and resent in equal measure. And yes, I couldn't have writen this app without it."),
           tags$hr()),
           tags$p("Developed by Ryan Heslin for the",
                  tags$a(href = "https://cs50.harvard.edu/", "CS50 final project."),
            style = "text-align: center"))),
  
  tabPanel(
    "Users",
    tags$div(
      id = "textInput",
      textInput(inputId = "username", label = "Enter a username:"),
      tags$br(),
      actionButton("username_button", "Search")
    ),
    tags$br(),
    htmlOutput("user_header", container = tags$p, class = "header"),
    fluidRow(column(
      6,
      textOutput("title_user_stats"),
      tableOutput("table_user")
    ), column(
      6,
      plotOutput(
        "plot_contributions"
      ))),
    tags$br(),
    fluidRow(
      column(
        6,
        textOutput("title_user_tags"),
        tableOutput("table_tags")
      ),
      column(6, plotOutput(
        "plot_user_activity"
      ))),
    tags$br(),
    textOutput("title_user_top_rated"),
    tags$br(),
    tags$div(htmlOutput("text_user_top_rated"),
             class = "posting")
  ),
  tabPanel(
    "Languages",
    verticalLayout(
      radioButtons(
        inputId = "language",
        "Select language:",
        choices = LANGUAGES,
        selected = character(),
        inline = TRUE
      ),
      tags$br(),
      htmlOutput("language_header", container = tags$p, class = "header"),
      fluidRow(
        column(
          6,
          textOutput("title_language_tags"),
          tableOutput("table_language_tags")
        ),
        column(
          6,
          textOutput("title_language_stats"),
          tableOutput("table_language_stats"),
        )
      ), tags$br(),
      fluidRow(
        column(
          6,
          plotOutput("plot_language_visibility"),
        ),
        column(
          6,
          plotOutput(
            "plot_tags_pairs"
          )
        )
      ), tags$br(),
      fluidRow(
        column(
          6,
          tags$div(
            plotOutput(
            "plot_bigrams"
          )
        )),
        column(
          6,
          plotOutput("plot_language_activity")
        )
      ),
      tags$br(),
      textOutput("title_language_top_question"),
      tags$br(),
      tags$div(htmlOutput("text_top_language_question"),
               class = "posting")
    ),
    inverse = TRUE,
    collapsible = TRUE,
    selected = "About"
  )
)

server <- function(input, output, session) {
  id <- reactiveVal(integer())
  top_language_username <- reactiveVal(character())
  user_key <- eventReactive(input$username_button, {
    data <- dbGetQuery(
      con,
      "SELECT id, display_name AS Name, creation_date AS Date,
                         location AS Location, reputation AS Reputation
                      FROM users WHERE display_name = @x",
      parameters = list(x = input$username)
    )

    validate_df(data, input$username, "No user named")
    data %>%
      arrange(Date) %>%
      mutate(Date = format(Date, "%B %d, %Y"))
  })

  excess_users <- observeEvent(user_key(), {
    if (nrow(user_key()) > 1) {
      button_names <- with(user_key(), sprintf(
        "%s (user ID %d; location %s; joined %s)",
        Name, id, Location, Date
      ))
      insertUI(
        selector = "#textInput",
        ui = tags$div(
          radioButtons("select_user",
            choices = setNames(user_key()$id, button_names),
            selected = character(0),
            label = sprintf(
              "%d users found named %s. Choose one:",
              nrow(user_key()), isolate(input$username)
            )
          ),
          id = "userSelect"
        ),
        where = "beforeEnd"
      )
    } else {
      id(user_key()$id)
    }
  })

  disambiguate_user <- observeEvent(input$select_user, {
    id(input$select_user)
    removeUI(selector = "#textInput > #userSelect", immediate = TRUE)
  })

  user_filtered <- eventReactive(id(), {
    user_key()[user_key()$id == id(), ]
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
      questions = questions,
      answers = answers,
      comments = comments,
      .id = "Type"
    ) %>%
      mutate(Type = factor(Type, levels = c("questions", "answers", "comments")))
    # Get id and submission type of user's higherst rated,
    # tiebreaking by date
    top_type <-
      data[order(data$Score, data$Date, decreasing = TRUE)[1], c("id", "Type", "Date", "Score")]
    top_type$Type <- as.character(top_type$Type)
    attr(data, "top_type") <- top_type

    error <- validate_df(data, input$username, "No submissions found for")
    if (!is.null(error)) {
      error
    } else {
      data
    }
  })
  output$table_user <-
    renderTable({
      user_filtered()[, c("Name", "Date", "Location", "Reputation")]
    })

  output$text_user_top_rated <-
    renderText({
      type <- attr(user_data(), "top_type")
      query <- paste(
        "SELECT",
        dbQuoteIdentifier(con, ifelse(type$Type == "comments", "text", "body")),
        "FROM",
        dbQuoteIdentifier(con, TABLE_NAMES[type$Type]),
        "WHERE id = @x"
      )
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
        geom_bar(position = "dodge") +
        theme(legend.position = "bottom", axis.text.x = element_blank()) +
        labs(title = paste(isolate(input$username), "Activity"), , x = "", y = "Contributions")
    }, bg = NA, height = 350, width = 450)
  output$plot_user_activity <-
    renderPlot({
      time_plot(user_data(), Contributions, group = TRUE) +
        ggtitle(paste(isolate(input$username), "Activity over Time"))
    }, height = 350, width = 450, bg = NA)

  output$table_tags <-
    renderTable({
      tags_table(user_data()$Tags, input$username)
    })

  language_query <- eventReactive(input$language,
    {
      strings <-
        escape_like_query(input$language, "|", con)
      sql <-
        c(" WHERE (tags LIKE ", " OR tags LIKE ", " OR tags LIKE ", " OR tags = ")
      paste0(combine_vectors(sql, strings), ")")
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )

  language_data <-
    eventReactive(language_query(), {
      # Guard against SQL injection
      data <- dbGetQuery(
        con,
        paste0(
          "SELECT id, title AS Title, creation_date AS Date , last_activity_date , tags AS Tags,
                        score AS Score, view_count AS Views
                    FROM posts_questions",
          language_query()
        )
      )
      error <- validate_df(data, input$language, "No questions found for language")
      if (!is.null(error)) {
        error
      } else {
        data
      }
    })

  output$text_top_language_question <- renderText({
    top_id <- language_data()$id[order(language_data()$Score, language_data()$last_activity_date, decreasing = TRUE)[1]]
    text <- dbGetQuery(con, "SELECT body, owner_display_name from posts_questions WHERE id = @x",
      parameters = list(x = top_id)
    )
    top_language_username(text$owner_display_name)
    validate(need(text, paste("Could not find top-scoring question for", names(LANGUAGES)[LANGUAGES == input$language])))
    text$body
  })
  output$plot_language_activity <-
    renderPlot({
      time_plot(language_data(), agg_column = Questions, group = FALSE) +
        ggtitle(paste(names(LANGUAGES)[LANGUAGES == isolate(input$language)], "Questions over Time"))
    }, height = 350, width = 450, bg = NA)
  output$plot_language_visibility <- renderPlot({
    language_data() %>%
      pivot_longer(c(Views, Score),
        names_to = "Type",
        values_to = "Number"
      ) %>%
      time_plot(agg_column = Number, agg_expr = sum(Number, na.rm =TRUE), group = TRUE) +
      ggtitle(paste(names(LANGUAGES)[LANGUAGES == isolate(input$language)], "Activity over Time"))
  }, height = 350, width = 450, bg = NA)
  
  output$plot_tags_pairs <- renderPlot({
    processed <- language_data() %>%
      select(id, Tags)
    validate_df(processed, input$language, "No tags found for language")
    processed <- processed %>%
      separate_rows(Tags, sep = "\\|") %>%
      # Keep up to 20 most common tags
      filter(Tags != input$language & Tags %in% names(sort(table(Tags), decreasing = TRUE)[1:20])) %>%
      group_by(id) %>%
      pivot_wider(
        names_from = Tags, values_from = Tags, values_fill = 0,
        values_fn = function(x) 1
      ) %>%
      ungroup() %>%
      select(-id) %>%
      as.matrix() %>%
      {
        t(.) %*% .
      }

    processed %>%
      as.data.frame() %>%
      mutate(x = rownames(.)) %>%
      pivot_longer(-x, names_to = "y", values_to = "Count") %>%
      mutate(across(-Count, ~ as.factor(paste0('"', .x, '"'))),
        y = factor(y, rev(levels(y)))
      ) %>%
      ggplot(aes(x = x, y = y, fill = Count)) +
      geom_tile() +
      scale_fill_gradient(low = "#E5D9B4", high = "#3B3A37") +
      labs(
        title = paste(
          "Tag Co-occurrences in", names(LANGUAGES)[LANGUAGES == input$language],
          "Questions"
        ), subtitle = "(Diagonal indicates total count)",
        x = "", y = ""
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }, height = 350, width = 450, bg = NA)

  output$plot_bigrams <- renderPlot({
    rows <- nrow(language_data())
    sample_rows <- language_data()$id[sample(seq_len(rows), min(2000, rows),
      replace = FALSE
    )]
    text <- dbGetQuery(con, paste0(
      "SELECT body AS text FROM posts_questions", language_query(),
      " AND id IN UNNEST(@x)"
    ), parameters = list(x = sample_rows)) %>%
      mutate(text = extract_text(text))
    bigrams <- text %>%
      tidytext::unnest_tokens(word1, text, token = "words") %>%
      mutate(
        word1 = ifelse(word1 %in% tidytext::stop_words$word | grepl("\\d", x = word1), NA_character_, word1),
        word2 = lead(word1)
      ) %>%
      filter(rowSums(is.na(.)) == 0) %>%
      unite("bigram", word1, word2, sep = " ") %>%
      count(bigram, sort = TRUE, name = "Count") %>%
      mutate(Proportion = Count / sum(Count), bigram = paste0('"', bigram, '"') %>% {
        factor(., levels = reorder(., Proportion))
      }) %>%
      filter(bigram %in% names(sort(table(bigram), decreasing = TRUE))[1:5]) %>%
      mutate(ymax = cumsum(Proportion), ymin = lag(ymax, default = 0))

    validate_df(bigrams, names(LANGUAGES)[LANGUAGES == input$language], "Not enough content words found for language")

    # Donut plot
    # See https://www.r-graph-gallery.com/128-ring-or-donut-plot.html
    bigrams %>% ggplot(aes(xmin = 3.5, xmax = 5, ymin = ymin, ymax = ymax, fill = bigram)) +
      geom_rect(color = "black") +
      coord_polar(theta = "y", clip = "off") +
      xlim(c(2, 5.5)) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        x = "", y = "Percentage",
        title = paste("Most Common Bigrams in",
                      names(LANGUAGES)[LANGUAGES == input$language], "Questions"),
        subtitle = "( % of total bigrams in sample of 2,000)",
        fill = "Bigram"
      ) +
      guides(fill = guide_legend(title.position = "top", title.hjust = .5)) +
      theme(
        legend.position = "right", # axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.box.margin = margin(t = -10),
        panel.grid = element_blank(),
        panel.grid.major = element_blank()
      )
  }, height = 400, width = 400, bg = NA)
  output$table_language_stats <- renderTable({
    language_data() %>% summarize(
      `Average Score` = mean(Score),
      `Average Views` = mean(Views),
      `Average Tags per Post` = mean(lengths(gregexec("\\|", Tags)) + 1),
      `Average Duration` = format(mean(difftime(last_activity_date, Date, units = "days")))
    )
  })
  output$table_language_tags <- renderTable({
    tags_table(
      language_data()$Tags, names(LANGUAGES)[LANGUAGES == input$language],
      input$language
    )
  })
   output$title_language_tags <-
     renderText( {
       if (length(input$language)) paste("Tags for", names(LANGUAGES)[LANGUAGES == input$language])
     })
 output$title_language_stats <- renderText({
    if (length(input$language)) paste("Statistics for", names(LANGUAGES)[LANGUAGES == input$language])
  })
  output$title_language_top_question <- renderText({
    if (length(input$language)) sprintf("Top-rated question for %s (score %d, posted by %s)", names(LANGUAGES)[LANGUAGES == input$language],
                                       max(language_data()$Score), top_language_username())
  })
  output$title_user_stats <- renderText( {
    if (nrow(user_filtered()) == 1){
      paste("Vital statistics for", input$username)
    }
  })
    output$title_user_tags <- renderText( {
  if (nrow(user_filtered()) == 1) {
      paste("Tags for", input$username)
    }
  })
    output$title_user_top_rated <- renderText( {
  if (nrow(user_filtered()) == 1) {
      type <- attr(user_data(), "top_type")
      sprintf("Top-rated submission (a(n) %s) for %s (score %d)",
              gsub("s$", "", type$Type),
              input$username,
              type$Score
              )
    }
  })
    output$user_header <- renderText({
      if(nrow(user_filtered()) == 1){
        paste0("<strong>",input$username, " Overview</strong>")
      }
    })
    output$language_header <- renderText({
      if(length(input$language)){
        paste0("<strong>",names(LANGUAGES)[LANGUAGES == input$language],
               " Overview</strong>")
      }
    })
 }

shinyApp(ui, server)
