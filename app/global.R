
# Required packages -------------------------------------------------------
library(DBI)
library(bigrquery)
library(ggplot2)


# Initial app state -------------------------------------------------------
bq_auth(path = "../credentials.json")
con <- dbConnect(bigquery(),
                 project = "bigquery-public-data",
                 dataset = "stackoverflow",
                 billing = "stacktrace-321016")

# File sourcing -----------------------------------------------------------


