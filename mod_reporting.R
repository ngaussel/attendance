library(dplyr)
library(tidyr)

mod_reporting_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      tags$h2("Live Presence"),
      uiOutput(ns("live"))
    ),
    fluidRow(
      tags$h2("Generate reports"),
      selectInput(ns("lecture"), "Lecture", choices=COURSE_MENU, selected=COURSE_MENU[1]),
    column(6,
      tags$h3("Professor"),       
      actionButton(ns("prof"), "Generate", class = "btn btn-primary"),     
           ),
    column(6,
          tags$h3("Administration"),
          dateInput(ns("date"),label = "Select date"),
          uiOutput(ns("admin"))
    )
    )
    )
}

mod_reporting_server <- function(id,params) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    roster <- googlesheets4::read_sheet(SHEET_ID, sheet = SHEET_NAME_ROSTER,col_types = "ccccc")
    
    # Live presence
    report_day_columns <- c("STUDENT_ID","NOM","PRENOM","EMAIL","MASTER","TOKEN","TIME_STAMP")
    output$live<- renderUI({
      
      invalidateLater(10000, session)
      logs <- googlesheets4::read_sheet(SHEET_ID, sheet = SHEET_NAME_LOG,col_types = "cccccccccc")
      
      if (params$session_presenter){
        
        # prez <-logs |> 
        #   filter(LECTURE==params$live_lecture,
        #          DATE==params$live_date) |> 
        #   left_join(roster |> select(-MASTER), by="STUDENT_ID") |> 
        #   select(all_of(report_day_columns))
        
        prez <-logs |> 
          filter(LECTURE==params$live_lecture,
                DATE==params$live_date)
        
        
        caption = paste0(params$live_lecture, " ",params$live_date)
        
        tags$div(
          tags$p(paste0("Registered: ",nrow(prez))),
          tags$div(DT::datatable(prez,
                                 options = list(
                                   scrollX = TRUE,      # active le scroll horizontal
                                   autoWidth = TRUE,    # adapte la largeur automatique
                                   pageLength = 10       # optionnel : nombre de lignes visibles
                                 ),
                                 caption = caption)
                   )
          )
        
      }else{
        tags$p(tags$strong("No live session"))
      }
    })
 
    # Professor
    
  })
}
