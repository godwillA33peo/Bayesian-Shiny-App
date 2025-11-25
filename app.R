library(shiny)
#library(TeachBayes)
source("global.R", local = TRUE)
source("ui/ui_main.R",     local = TRUE)
source("server/server_main.R", local = TRUE)
#source("TeachBayes.r", local = TRUE)

shinyApp(ui = ui, server = server)
