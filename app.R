library(shiny)
library(googlesheets4)
library(tibble)
library(glue)
library(qrencoder)
library(png)
library(shinyjs)
library(future)
library(promises)
plan(multisession,workers=2)


# -- Sources ---------------
source("globals.R")
source("mod_emargement.R")
source("mod_student.R")
source("helper.R")



# --- DYNAMIC UI: route vers la bonne vue -----------------------------------
ui <- function(request) {
  query <- parseQueryString(request$QUERY_STRING)
  if (!is.null(query$t)) {
    fluidPage(mod_student_ui("student"))
  } else {
    # Interface principale avec les modules
    navbarPage(
      div(class = "logo", img(src = "sorbonne_logo.png", height = "50px")),
      title = "Attendance Management",
      header = tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
        tags$link(rel = "icon", type = "image/png", href = "fav1.png")),
      tabPanel(
        "Attendance",
        mod_emargement_ui("attendance")  # avec namespace "attendance"
      ),
    )
  }
}

# --- SERVER ----------------------------------------------------------------

server <- function(input, output, session) {

  params  <- reactiveValues(token = NULL,
                            started = NULL,
                            session_presenter=FALSE)
  
  mod_emargement_server("attendance",params=params)
   
     observe({
      query <- parseQueryString(session$clientData$url_search)
      if (!is.null(query$t)) {
        mod_student_server("student", token=query$t)
      }
    })
    
}


# --- LANCE APP -------------------------------------------------------------
shinyApp(ui, server)
