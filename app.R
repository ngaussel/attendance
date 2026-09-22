library(shiny)
library(qrencoder)
library(png)
library(shinyjs)
library(httr2)


# -- Sources ---------------
source("globals.R")
source("mod_emargement.R")
source("R/utils.R")
source("R/supabase.R")



# --- UI ----------------------------------------------------------------
# La vue étudiant est désormais une page statique (www/checkin.html) qui
# valide la présence via la fonction Supabase validate_attendance() —
# cf. supabase/schema.sql. Plus de session Shiny par étudiant.
ui <- function(request) {
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

# --- SERVER ----------------------------------------------------------------

server <- function(input, output, session) {

  params  <- reactiveValues(session_presenter = FALSE)

  mod_emargement_server("attendance", params = params)
}


# --- LANCE APP -------------------------------------------------------------
shinyApp(ui, server)
