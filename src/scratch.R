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

answers <- dbGetQuery(con, "SELECT * FROM posts_questions LEFT JOIN  posts_answers ON
                      posts_questions.accepted_answer_id = posts_answers.id LIMIT 100")
R_posts <- dbGetQuery(con, "SELECT * FROM posts_questions WHERE tags LIKE '%|r|%'
                      OR tags LIKE 'r|%' OR tags LIKE '%|r' OR tags = 'r' LIMIT 100") 
power_users <- dbGetQuery(con, "SELECT display_name, reputation, up_votes FROM users ORDER BY reputation
                          DESC LIMIT 100")
query <- paste0("SELECT display_name, creation_date, text FROM
                                   comments INNER JOIN users ON comments.user_display_name =
                                   users.display_name WHERE users.display_name =
                                   = @ GROUP BY comments.user_display_name ORDER BY comments.score
                                   DESC LIMIT 1", users = power_users$display_name)
query <- dbSendQuery(con, sql)
dbBind(query, params = list(x = power_users$display_name))
top_users_top_comments <- dbFetch(posts_q, nrow = 100)
