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
power_users <- dbGetQuery(con, "SELECT display_name, reputation, up_votes FROM users ORDER BY reputation
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
user_id <- power_users[1]





