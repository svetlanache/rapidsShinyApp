# Load package functions
#library(rapidsShiny)
library(jsonlite)
library(future)
library(promises)

# App
shinyApp(
  ui = rapidsShiny::create_ui(),
  server = rapidsShiny::create_server
)
