mod_emargement_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    shinyjs::useShinyjs(),
    div(class = "container",
        titlePanel("Attendance Presenter View"),
        fluidRow(
          column(3, selectInput(ns("lecture"), "Lecture", choices=COURSE_MENU, selected=COURSE_MENU[1])),
          column(3, dateInput(ns("date"), "Date", value = Sys.Date()))
          ),
          selectInput(ns("venue"), "Location",
            choices = c("Ma position actuelle" = "gps",VENUES,"Sans géolocalisation" = "none")),
        conditionalPanel(
          condition = paste0("input['", ns("venue"), "'] === 'gps'"),
          div(style = "margin-bottom:10px;",
            actionButton(ns("capture_gps"), "📍 Capturer ma position", class = "btn btn-secondary btn-sm"),
            span(style = "margin-left:10px;", textOutput(ns("gps_status"), inline = TRUE))
          )
        ),
        sliderInput(ns("geo_radius"), "Geolocation radius (m)",
          min = 0, max = 1000, value = 250, step = 50),
        actionButton(ns("start_session"), "Launch", class = "btn btn-primary"),
        br(), br(),
        uiOutput(ns("qr_zone"))
    )
  )
}

mod_emargement_server <- function(id,params) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    current <- reactiveVal(list())
    presenter_coords <- reactiveValues(lat = NULL, lon = NULL)

    token_timer <- reactiveTimer(TOKEN_TTL_SECONDS * 1000, session)
    
    # --- RÉCUPÉRATION DU TOKEN VIA URL --------------------------
    observe({
      query <- parseQueryString(session$clientData$url_search)
      if (!is.null(query$t)) {
        params$token <- query$t
        params$started <- Sys.time()
      }
    })
    
    # --- GPS CAPTURE -------------------------------------------
    gps_js <- function() {
      shinyjs::runjs(sprintf('
        navigator.geolocation.getCurrentPosition(
          function(pos) {
            Shiny.setInputValue("%s", pos.coords.latitude);
            Shiny.setInputValue("%s", pos.coords.longitude);
          },
          function(err) { alert("Erreur géolocalisation : " + err.message); },
          {enableHighAccuracy: true, timeout: 10000}
        );
      ', ns("gps_lat"), ns("gps_lon")))
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
      if (input$venue == "gps" && is.null(presenter_coords$lat)) {
        shinyjs::hide("start_session")
      } else {
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
        params$session_lat <- NULL
        params$session_lon <- NULL
      } else if (input$venue == "gps") {
        if (is.null(presenter_coords$lat)) {
          showNotification("Capturez d'abord votre position.", type = "warning")
          return()
        }
        params$session_lat <- presenter_coords$lat
        params$session_lon <- presenter_coords$lon
      } else {
        parts <- as.numeric(strsplit(input$venue, ",")[[1]])
        params$session_lat <- parts[1]
        params$session_lon <- parts[2]
      }

      params$session_presenter <- TRUE
      params$live_lecture <- input$lecture
      params$live_date <- input$date
    })
    
    observe({
      req(nzchar(input$lecture), !is.null(input$date))
      req(params$session_presenter)
      token_timer()  # redéclenche tous les TOKEN_TTL_SECONDS
      
      # génère le token
      tkn <- rand_token()
      now <- now_utc()
      exp <- now + TOKEN_TTL_SECONDS
      session_id <- paste0(input$lecture, "_", format(input$date, "%Y-%m-%d"))
      
      landing <- paste0(
        session$clientData$url_protocol, "//",
        session$clientData$url_hostname,
        if (nzchar(session$clientData$url_port)) paste0(":", session$clientData$url_port) else "",
        session$clientData$url_pathname,
        "?t=", URLencode(tkn)
      )
      
      token_store[[tkn]] <<- list(
        session_id = session_id,
        expires_at = exp,
        lat = params$session_lat,
        lon = params$session_lon,
        geo_radius = input$geo_radius
      )

      current(list(
        token = tkn,
        landingUrl = landing,
        expiresAt = exp,
        sessionId = session_id
      ))
    })
    
    output$qr_img <- renderImage({
      c <- current()
      req(c$landingUrl)
      tmp <- tempfile(fileext = ".png")
      qr <- qrencode_raw(c$landingUrl,level =3 )
      qr_resized <- qr[rep(1:nrow(qr), each = 11), rep(1:ncol(qr), each = 11)]
      writePNG(qr_resized, target = tmp)
      list(src = tmp, contentType = "image/png", alt = "QR code")
    }, deleteFile = TRUE)
    
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
      req(c$token)  # n'affiche rien tant qu'on n'a pas de token

      div(
        style = "display: flex; flex-direction: column; gap: 10px; margin-top: 12px;",
        div(style = "width: 350px; height: 350px; flex-shrink: 0;",
            imageOutput(ns("qr_img"), width = "350px", height = "350px")),
        div(
          "Token: ",
          code(
            span(
              style = "font-size: 18px; font-weight: bold;",
              textOutput(ns("tok_txt"), inline = TRUE)
            )
          )
        ),
        div("Check-in URL: ", uiOutput(ns("landing_link"))),
        div("Next refresh in: ", textOutput(ns("rotate_cd"), inline = TRUE))
      )
    })
    
    
  })
}