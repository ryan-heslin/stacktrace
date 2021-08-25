library(shinytest)

path = tempfile() 
app <- shinytest::ShinyDriver$new("R/app.R")
app$sendKeys("username", "Jon Skeet")
app$getValue("username")
#app$takeScreenshot(path, "user")

app$setInputs("language" = "r")
expect_equal(app$getValue("language"), "r")
app$getValue("top_id")

