library(shiny)
library(qrencoder)
library(png)
library(shinyjs)
library(httr2)


# -- Sources ---------------
source("globals.R")
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
      shinyjs::useShinyjs(),
      div(class = "container",
          titlePanel("Attendance Presenter View"),
          fluidRow(
            column(3, selectInput("lecture", "Lecture", choices=COURSE_MENU, selected=COURSE_MENU[1])),
            column(3, dateInput("date", "Date", value = Sys.Date()))
            ),
            selectInput("venue", "Location",
              choices = c("Ma position actuelle" = "gps",VENUES,"Sans géolocalisation" = "none")),
          conditionalPanel(
            condition = "input['venue'] === 'gps'",
            div(style = "margin-bottom:10px;",
              actionButton("capture_gps", "📍 Capturer ma position", class = "btn btn-secondary btn-sm"),
              span(style = "margin-left:10px;", textOutput("gps_status", inline = TRUE))
            )
          ),
          sliderInput("geo_radius", "Geolocation radius (m)",
            min = 0, max = 1000, value = 250, step = 50),
          shinyjs::hidden(actionButton("start_session", "Launch", class = "btn btn-primary")),
          br(), br(),
          uiOutput("qr_zone")
      )
    ),
  )
}

# --- SERVER ----------------------------------------------------------------

server <- function(input, output, session) {

  current <- reactiveVal(list())
  presenter_coords <- reactiveValues(lat = NULL, lon = NULL)
  live_session_id <- reactiveVal(NULL)

  token_timer <- reactiveTimer(TOKEN_TTL_SECONDS * 1000, session)

  # --- GPS CAPTURE -------------------------------------------
  gps_js <- function() {
    shinyjs::runjs('
      navigator.geolocation.getCurrentPosition(
        function(pos) {
          Shiny.setInputValue("gps_lat", pos.coords.latitude);
          Shiny.setInputValue("gps_lon", pos.coords.longitude);
        },
        function(err) { alert("Erreur géolocalisation : " + err.message); },
        {enableHighAccuracy: true, timeout: 10000}
      );
    ')
  }

  # Auto-déclenche la capture dès que "gps" est sélectionné (y compris au chargement)
  observeEvent(input$venue, {
    req(input$venue == "gps")
    gps_js()
  })

  # Bouton pour re-capturer manuellement
  observeEvent(input$capture_gps, {
    gps_js()
  })

  observeEvent(input$gps_lat, {
    req(input$gps_lat, input$gps_lon)
    presenter_coords$lat <- input$gps_lat
    presenter_coords$lon <- input$gps_lon
  })

  observe({
    req(input$venue)
    if (!(input$venue == "gps" && is.null(presenter_coords$lat))) {
      shinyjs::show("start_session")
    }
  })

  output$gps_status <- renderText({
    if (!is.null(presenter_coords$lat))
      sprintf("✅ %.5f, %.5f", presenter_coords$lat, presenter_coords$lon)
    else ""
  })

  # --- PRÉSENTATEUR ------------------------------------------
  observeEvent(input$start_session, {
    req(nzchar(input$lecture), !is.null(input$date))

    if (input$venue == "none") {
      venue_lat <- NULL
      venue_lon <- NULL
    } else if (input$venue == "gps") {
      if (is.null(presenter_coords$lat)) {
        showNotification("Capturez d'abord votre position.", type = "warning")
        return()
      }
      venue_lat <- presenter_coords$lat
      venue_lon <- presenter_coords$lon
    } else {
      parts <- as.numeric(strsplit(input$venue, ",")[[1]])
      venue_lat <- parts[1]
      venue_lon <- parts[2]
    }

    new_session_id <- tryCatch(
      supabase_create_session(
        course       = input$lecture,
        venue_lat    = venue_lat,
        venue_lon    = venue_lon,
        geo_radius_m = input$geo_radius
      ),
      error = function(e) {
        showNotification(
          paste("Erreur création session Supabase :", conditionMessage(e)),
          type = "error"
        )
        NULL
      }
    )
    req(new_session_id)
    live_session_id(new_session_id)
  })

  observe({
    req(nzchar(input$lecture), !is.null(input$date))
    req(live_session_id())
    token_timer()  # redéclenche tous les TOKEN_TTL_SECONDS

    # génère le token et le pousse dans Supabase (source de vérité pour
    # la validation côté étudiant, cf. supabase/schema.sql)
    tkn <- rand_token()
    now <- now_utc()
    exp <- now + TOKEN_TTL_SECONDS

    ok <- tryCatch({
      supabase_rotate_token(live_session_id(), tkn)
      TRUE
    }, error = function(e) {
      showNotification(
        paste("Erreur rotation token :", conditionMessage(e)),
        type = "error"
      )
      FALSE
    })
    req(ok)

    base_path <- session$clientData$url_pathname
    if (!endsWith(base_path, "/")) base_path <- paste0(base_path, "/")

    landing <- paste0(
      session$clientData$url_protocol, "//",
      session$clientData$url_hostname,
      if (nzchar(session$clientData$url_port)) paste0(":", session$clientData$url_port) else "",
      base_path, "checkin.html",
      "?s=", live_session_id(),
      "&t=", URLencode(tkn),
      "&course=", URLencode(input$lecture),
      "&date=", format(input$date, "%Y-%m-%d")
    )

    current(list(
      token = tkn,
      landingUrl = landing,
      expiresAt = exp,
      sessionId = live_session_id()
    ))
  })

  session$onSessionEnded(function() {
    sid <- isolate(live_session_id())
    if (!is.null(sid)) {
      tryCatch(supabase_close_session(sid), error = function(e) NULL)
    }
  })

  output$tok_txt <- renderText({ req(current()$token); current()$token })
  output$landing_link <- renderUI({
    req(current()$landingUrl)
    tags$a(href = current()$landingUrl, target = "_blank", current()$landingUrl)
  })

  output$rotate_cd <- renderText({
    invalidateLater(1000, session)
    remaining <- difftime(current()$expiresAt, Sys.time(), units = "secs")
    sprintf("%.0f s", max(0, as.numeric(remaining)))
  })

  output$qr_zone <- renderUI({
    c <- current()
    req(c$token)

    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp))
    qr <- qrencode_raw(c$landingUrl, level = 3)
    qr_resized <- qr[rep(1:nrow(qr), each = 11), rep(1:ncol(qr), each = 11)]
    writePNG(qr_resized, target = tmp)
    img_b64 <- base64enc::base64encode(tmp)

    div(
      style = "display: flex; flex-direction: column; gap: 12px; margin-top: 12px;",
      tags$img(
        src    = paste0("data:image/png;base64,", img_b64),
        width  = "350",
        height = "350",
        alt    = "QR code",
        style  = "display: block; flex-shrink: 0;"
      ),
      div(
        "Token: ",
        code(span(style = "font-size: 18px; font-weight: bold;",
                  textOutput("tok_txt", inline = TRUE)))
      ),
      div("Check-in URL: ", uiOutput("landing_link")),
      div("Next refresh in: ", textOutput("rotate_cd", inline = TRUE))
    )
  })

}


# --- LANCE APP -------------------------------------------------------------
shinyApp(ui, server)
