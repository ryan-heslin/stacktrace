# STACKTRACE

## Overview

Stacktrace is a Web app for interactively exploring and visualizing data from
[the StackOverflow public dataset](https://www.kaggle.com/stackoverflow/stackoverflow). It uses the Google Cloud Platform's [BigQuery](https://cloud.google.com/bigquery) service to transmit SQL queries to the large remote database.
The  R language's [Shiny](https://cran.r-project.org/web/packages/shiny/index.html)  and i[bigqueryr](https://cran.r-project.org/web/packages/bigQueryR/index.html) packages provide the user interface. It also uses several additional packages, mainly `dplyr` for data transformation and `tidytext` for text processing.

## Features

Stacktrace's web interface is divided into two pages. The Users page prompts app users to enter the name of a StackOverflow user, then searches the database for a match. If a valid user is selected, the app displays their vital statistics (reputation, date of account creation, etc.). It also plots the distribution of their contributions (question, answer, or comment) to the site over time and displays their highest-rated post. 

The Languages tab allows users to select one of several popular programming
languages. It then obtains all questions tagged with the name of that language and produces a time plot of questions with that tag.(StackOverflow questions may be posted with tags related to the topic of the question, e.g. "linear regression"). An additional plot compares the co-occurrences of tags associated with questions for the language. 
The most technically complex plot visualizes the distribution of the most common bigrams found in a random sample of questions in the language. (Bigrams are ordered pairs of words that occur in a text, e.g. "once upon"). The plot highlights unusually pairs of words that crop up frequently in the question text, which often reflect something about the language being analyzed. For R, the most common is usually "data frame," since data frames are a class of R object.

## Design 

The bulk of the project lies in the R folder, which contains all R code necessary to run the app. 
The main file is `app.R`, which contains the server and UI functions that power the app. In hindsight, I should have split it into separate modules.
Supporting files include `global.R`, which defines constants and loads required packages, and `utils.R`, a collection of utility functions. The parallel `tests` folder contains rudimentary test code, and `src` contains experimental code used to test functions when designing the app.

Aside from this README, the top level also contains the `JSON` of credentials needed for users to access the app on my behalf.

Developing this app was hard. Major challenges included configuring the Google Cloud project, dynamically generating safe SQL queries, and organizing the interlocking components of the server function. 
Reactive expressions, the core of Shiny apps, behave in counterintuitive ways and are tricky to debug. 

I opted to put most server-side computation in reactive events, which 
return values when an input value changes. This made it easy to channel
needed values to the output functions (reactive consumers, in Shiny parlance) that produced the plots. The app file borders on too complex for a single script, but I was able at least to turn a good deal of
its tasks into separate functions.

And yes, I consulted StackOverflow all the time while writing the app.
