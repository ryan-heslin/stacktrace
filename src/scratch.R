library(bigrquery)
library(DBI)
bq_auth(path = "./credentials.json")
con <- dbConnect(bigquery(),
                 project = "bigquery-public-data",
                 dataset = "stackoverflow",
                 billing = "stacktrace-321016")
dbListTables(con)
# Not run


dbGetQuery(con, "SELECT ID from posts_answers WHERE id = 14614782")
dbGetQuery(con, "SELECT * from `.INFORMATION_SCHEMA.TABLES`") %>% 
  readr::write_csv(file = "schema.csv")

dbGetQuery(con, "SELECT * FROM `.INFORMATION_SCHEMA.COLUMNS`") %>% 
  readr::write_csv(file = "../app/data/columns.csv")

answers <- dbGetQuery(con, "SELECT * FROM posts_questions LEFT JOIN  posts_answers ON
                      posts_questions.accepted_answer_id = posts_answers.id LIMIT 100")
R_posts <- dbGetQuery(con, "SELECT * FROM posts_questions WHERE tags LIKE '%|r|%'
                      OR tags LIKE 'r|%' OR tags LIKE '%|r' OR tags = 'r' LIMIT 100") 
power_users <- dbGetQuery(con, "SELECT id, display_name, reputation, up_votes FROM users ORDER BY reputation
                          DESC LIMIT 100")
sql <- "SELECT * FROM (
SELECT comments.user_display_name,
 comments.creation_date,
 comments.text, comments.score,
   ROW_NUMBER() OVER (PARTITION BY comments.user_display_name ORDER BY comments.score DESC) rn
  FROM
    comments INNER JOIN users ON comments.user_display_name =
                                   users.display_name
) 
WHERE rn = 1 AND user_display_name  IN  UNNEST(@x)"
x < bq_param_array(power_users$display_name)
query <- dbSendQuery(con, sql, parameters = list(x = power_users$display_name))
top_users_top_comments <- dbFetch(query, nrow = 100)
#test <- dbGetQuery(con, 'SELECT * FROM posts_questions WHERE creation_date > PARSE_TIMESTAMP("%F %T", "2021-01-01 00:00:00")')

test_user <- dbGetQuery(con, "SELECT id, display_name AS Username, age AS age,
           creation_date AS `Account_Created`, location AS Location,
           reputation AS Reputation FROM users WHERE display_name = @x
           ORDER BY Account_Created DESC LIMIT 1",
           parameters = list(x = "John Skeet" ))
questions <- dbGetQuery(con, "SELECT title AS Title, creation_date AS Date, tags AS Tags,
                        score AS score
                    FROM posts_questions WHERE owner_user_id = @x",
                    parameters = list (x = power_users$id[1]))

answers <- dbGetQuery(con, "SELECT title AS Title, creation_date AS Date, tags AS Tags, score AS score
                    FROM posts_answers WHERE owner_user_id = @x",
                    parameters = list (x = power_users$id[1]))
comments <- dbGetQuery(con, "SELECT creation_date AS Date, score AS score
                       FROM comments WHERE user_id = @x",parameters = list(x = power_users$id[1]) )
combined <- bind_rows(questions = questions, answers = answers, comments = comments, .id = "Type")
# contributions <- dbGetQuery(con, "SELECT posts_questions.title, posts_answers.title,
#                             posts_questions.creation_date, posts_answers.creation_date,
#                             posts_questions.tags, posts_answers.tags,
#                             comments.text, comments.creation_date,
#                             FROM comments INNER JOIN posts_questions ON comments.user_id = posts_questions.owner_user_id
#                             INNER JOIN posts_answers ON posts_questions.owner_user_id = posts_answers.owner_user_id
#                             WHERE comments.user_id = @x", parameters = list(x  = power_users$id[1]))

dbExecute(con, "CREATE TABLE `users_cluster`
          CLUSTER BY id
          OPTIONS(
          require_partition_filter=false
          )
          AS SELECT id, display_name, creation_date, location, reputation
          FROM users")
x = "r"
quoted <- sapply(c("%|?x|%", "?x|%", "%|?x", "x"), dbQuoteString, conn = con, x=x, ANSI())
sql <- paste0("SELECT * FROM posts_questions WHERE tags LIKE,'%|?x|%'
                      OR tags LIKE '?x|%' OR tags LIKE '%|?x' OR tags = '?x' LIMIT 100",
                      x = x)
test_df <- tibble(Date = seq(as.Date("1910/1/1"), as.Date("1999/1/1"), "months"), Type = rep(letters[1:3], length.out = 1069,
                                                                                             Contrib = runif(1069)) )
language_data <- dbGetQuery(con, "SELECT * FROM posts_questions WHERE tags LIKE'%|r|%'
       OR tags LIKE 'r|%' OR tags LIKE '%|r' OR tags = 'r' LIMIT 1000")

processed <- language_data %>%
  select(id, tags) %>% 
  separate_rows(tags, sep = "\\|") %>% 
  filter(tags != "r" & tags %in% names(sort(table(tags), decreasing = TRUE)[1:20])) %>% 
  group_by(id) %>% 
  pivot_wider(names_from = tags, values_from = tags, values_fill = 0,
              values_fn = function(x) 1) %>% 
  ungroup() %>% 
  select(-id) %>%
  as.matrix() %>% 
  {t(.) %*% .} 

processed %>% as.data.frame() %>% 
  rownames_to_column("x") %>% 
  pivot_longer(-x, names_to = "y", values_to = "Count") %>% 
  mutate(across(-Count, ~as.factor(paste0('"', .x, '"'))),
  y = factor(y, rev(levels(y)))) %>% 
  ggplot(aes(x = x, y = y, fill = Count))+
  geom_tile() +
  scale_fill_gradient(low = "#E5D9B4", high = "#3B3A37") +
  labs(title = paste("Tag Co-occurrences in"), subtitle = "Diagonal indicates total count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
library(tidytext)
dbGetQuery(con, "SELECT body FROM posts_questions WHERE id IN UNNEST(@x)",
           parameters = list(x = language_data$id)) %>% 
  mutate(body = unlist(lapply(body, htmltools::HTML)))
  unnest_tokens(bigram, body, token = "ngrams", n = 2) %>% 
  separate(bigram, into = c("word1", "word2"), sep = "\\s") %>% 
  filter(rowSums(is.na(.)) < 2)

#   mutate(word = if_else(word %in% stop_words, NA_character_, word),
#          id = seq_len(nrow(.)), key = rep(c("word1", "word2"), each = nrow(.)/2)) %>% 
#   group_by(key) %>% 
#   pivot_wider(names_from = "key", values_from = "word" ) %>% 
#   filter(rowSums(is.na(.)) < 2) %>% 
# select(-id)
  text <- dbGetQuery(con, "SELECT body FROM posts_questions WHERE id IN UNNEST(@x)",
                       parameters = list(x = language_data$id)) %>% .[[1]]
  text <- extract_text(text)

  extracted <- unlist(mapply(extract_groups, text, matches))
  
  extract_groups <- function(string, positions){
    if(any(positions == -1)) return("")
    #positions <- cbind(positions[2,], positions[2,] + attr(positions, "match.length")[2,]) 
    mapply(substr, start = positions[2,], stop = positions[2,] + attr(positions, "match.length")[2,] -1, MoreArgs = list(x = string))
  }
  text_df <- text %>% 
    # gsub('[:;\\\\"/()]', "", x= .) %>% 
    # gsub("[?!,.](?=\\s|$)", "", x = ., perl = TRUE) %>% 
    # sapply(., function(x) unlist(strsplit(x, "\\s+"), recursive = FALSE), USE.NAMES = FALSE) %>%
    # unlist() %>% 
     as.data.frame() %>% 
    setNames("text") %>% 
    unnest_tokens(word1, text, token = "words") %>% 
    mutate(word1 = ifelse(word1 %in% stop_words$word | grepl("\\d", x =  word1), NA_character_, word1),
           word2 = lead(word1)) %>%   
     filter(rowSums(is.na(.))  == 0)  %>% 
    unite("bigram", word1, word2, sep = " ") %>% 
    count(bigram, sort = TRUE, name = "Count") %>% 
    mutate(Proportion = Count / sum(Count), bigram = paste0('"', bigram, '"') %>% {factor(., levels = reorder(., Proportion))}) %>% 
    filter(Proportion >= .003) %>% 
    mutate(ymax = cumsum(Proportion), ymin = lag(ymax, default = 0))
    
  text_df %>% ggplot(aes(xmin = 3.5, xmax = 5, ymin = ymin, ymax = ymax, fill = bigram)) +
  geom_rect(color = "black") +
    coord_polar(theta = "y") +
    xlim(c(2, 5.5)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = "", y = "Percentage",
         title = "Most Common Bigrams as Percentage of Total",
         fill = "Bigram") +
    guides(fill = guide_legend(title.position = "top", title.hjust = .5)) +
    theme(legend.position = "bottom",# axis.line = element_blank(),
          axis.text.y  = element_blank(),
          axis.ticks.y = element_blank(),
          legend.box.margin = margin(t = -10),
          panel.grid = element_blank())
  
legend_data <- t


  
