library(shiny)
source("global.R", local = TRUE)
source("ui/ui_main.R",     local = TRUE)
source("server/server_main.R", local = TRUE)

shinyApp(ui = ui, server = server)
