

mod_reporting_ui <- function(id) {
  ns <- NS(id)
  fluidPage(tags$h1("Hello Guys"))
  
}


mod_reporting_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
  })
  }