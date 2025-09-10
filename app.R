library(shiny)
library(googlesheets4)
library(tibble)
library(glue)
library(qrencoder)
library(png)
library(shinyjs)

# --- CONFIG ENV ----------------------------------------------------------------
Sys.setenv(ATTENDANCE_SHEET_ID = Sys.getenv("ATTENDANCE_SHEET_ID", unset = "1RiiC0gsPn-29YfOGgFsuaivjmkwWsYDknfcZipIdwKg"))
Sys.setenv(GS_SERVICE_ACCOUNT_JSON = Sys.getenv("GS_SERVICE_ACCOUNT_JSON", unset = "id_api_google.json"))

SHEET_ID           <- Sys.getenv("ATTENDANCE_SHEET_ID")
SHEET_NAME_TOKENS  <- "tokens"
SHEET_NAME_LOG     <- "log"
SHEET_NAME_ROSTER     <- "roster"
SHEET_NAME_MMMEF     <- "MMMEF"
ROSTER_SHEET_NAME  <- "roster"
DOMAIN             <- "etu.univ-paris1.fr"
REQUIRE_ROSTER     <- FALSE
COURSE_MENU <- c("Financial Products", "Interest Rates", "Advanced Topics")

TOKEN_TTL_SECONDS  <- 25L
FILL_SECONDS <- 30L
TOKEN_LENGTH <- 8L
ALPHABET <- "ABCDEFGHJKLMNPQRSTUVWXYZ23456789"

# -- Sources ---------------
source("mod_emargement.R")
source("mod_student.R")
source("mod_reporting.R")
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
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
      tabPanel(
        "Attendance",
        mod_emargement_ui("attendance")  # avec namespace "attendance"
      ),
      tabPanel(
        "Reporting",
        mod_reporting_ui("reporting")    # avec namespace "reporting"
      )
    )
  }
}

# --- SERVER ----------------------------------------------------------------

server <- function(input, output, session) {

  params  <- reactiveValues(token = NULL, 
                            started = NULL, 
                            session_presenter=FALSE,
                            nsubmit=0)
  
  mod_emargement_server("attendance",params=params)
   
     observe({
      query <- parseQueryString(session$clientData$url_search)
      if (!is.null(query$t)) {
        mod_student_server("student", token=query$t)
      }
    })
    
    mod_reporting_server("reporting",params=params)
  
}


# --- LANCE APP -------------------------------------------------------------
shinyApp(ui, server)
