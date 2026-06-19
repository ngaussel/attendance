mod_student_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$link(rel = "icon", type = "image/png", href = "fav2.png"),
      tags$script(src = "https://cdn.jsdelivr.net/npm/@fingerprintjs/fingerprintjs@3/dist/fp.min.js"),
      tags$script(HTML(sprintf('
        $(document).on("shiny:connected", function() {
          FingerprintJS.load()
            .then(function(fp) { return fp.get(); })
            .then(function(result) {
              Shiny.setInputValue("%s", result.visitorId);
            });
        });
      ', ns("fingerprint"))))
    ),
    div(class = "container",
        tags$img(src = "sorbonne_logo.png", id = "sorbonne-logo"),
        h2("Attendance Check-in"),
        div(class = "ok", "Valid token ✅"),
        
        p(
          span(class = "muted", "Session: "),
          textOutput(ns("session_info"), inline = TRUE)
        ), 
        
        p(span(class = "muted", "This link expires in ", strong(textOutput(ns("cd"), inline = TRUE)))),
        tags$p("All fields are required."),
        tags$p(HTML("Please be patient, confirmation might take a bit of time. <br> Only register once.")),

        div(id = "form_zone",  # zone du formulaire
            textInput(ns("email"), "Email", placeholder = "your@email.com"),
            textInput(ns("sid"), "Student Id", placeholder = "123456..."),
            selectInput(ns("master"), "Master", choices=c("IRFA","MMMEF","Other"), selected="IRFA"),
            textInput(ns("lnid"), "Last Name", placeholder = "..."),
            textInput(ns("fnid"), "First Name", placeholder = "..."),
            
            actionButton(ns("submit"), "I'm present", class = "btn btn-primary")
        ),
        
        div(id = "feedback_zone",
            div(class = "ok", textOutput(ns("success_msg"))),
            div(class = "err", textOutput(ns("error_msg")))
        )
    )
  )
}

mod_student_server <- function(id,token) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    p_student <- reactiveValues(check_success = FALSE)
    sessionStartTime <- reactiveVal(NULL)

    # Déclenche la géolocalisation dès le chargement de la page
    observe({
      shinyjs::runjs(sprintf('
        if (navigator.geolocation) {
          navigator.geolocation.getCurrentPosition(
            function(pos) {
              Shiny.setInputValue("%s", pos.coords.latitude);
              Shiny.setInputValue("%s", pos.coords.longitude);
              Shiny.setInputValue("%s", Math.round(pos.coords.accuracy));
            },
            function(err) { Shiny.setInputValue("%s", true); },
            {enableHighAccuracy: true, timeout: 15000}
          );
        }
      ', ns("geo_lat"), ns("geo_lon"), ns("geo_accuracy"), ns("geo_denied")))
    })
    
    observe({
      req(token)
      if (is.null(sessionStartTime())) {
        sessionStartTime(Sys.time())
      }
    })
    
    output$session_info <- renderText({
      req(token)
      entry <- token_store[[token]]
      if (is.null(entry)) return("Unknown session")
      entry$session_id
    })
    
    cd_remaining <- reactive({
      invalidateLater(1000, session)
      start <- sessionStartTime()
      req(start)
      remaining <- FILL_SECONDS - as.numeric(difftime(Sys.time(), start, units = "secs"))
      max(0, remaining)
    })
    
    output$cd <- renderText({
      sprintf("%.0f s", cd_remaining())
    })
    
    observe({
      invalidateLater(1000, session)
      if (cd_remaining() <= 0 && !p_student$check_success) {
        shinyjs::disable("submit")
        showNotification("⏳ Time's up! The form has expired.", type = "error")
      }
    })
    
    
    observeEvent(input$submit, {
      req(token)

      # Validations instantanées (0 appel API)
      entry <- token_store[[token]]
      if (is.null(entry)) {
        showNotification("Invalid token.", type = "error")
        return()
      }
      if (now_utc() > entry$expires_at + FILL_SECONDS) {
        showNotification("Token expired. Please scan the QR code again.", type = "error")
        return()
      }
      dedup_key <- paste0(entry$session_id, "|", input$sid)
      if (dedup_key %in% submitted_presences) {
        showNotification("Already submitted.", type = "warning")
        return()
      }

      fp <- input$fingerprint %||% ""
      if (nzchar(fp) && fp %in% (device_fingerprints[[entry$session_id]] %||% character(0))) {
        showNotification("This device has already been used to register for this session.", type = "error")
        return()
      }

      email <- tolower(trimws(input$email %||% ""))
      if (email == "") {
        showNotification("Please enter your email.", type = "error")
        return()
      }
      if (trimws(input$sid) == "") {
        showNotification("Please enter your student Id.", type = "error")
        return()
      }
      if (trimws(input$lnid) == "") {
        showNotification("Please enter your last name.", type = "error")
        return()
      }
      if (trimws(input$fnid) == "") {
        showNotification("Please enter your first name.", type = "error")
        return()
      }

      # Vérification géolocalisation
      if (!is.null(entry$lat)) {
        if (isTRUE(input$geo_denied)) {
          showNotification("📍 Location access is required to validate attendance.", type = "error")
          return()
        }
        if (is.null(input$geo_lat)) {
          showNotification("📍 Acquiring your position, please wait...", type = "warning")
          return()
        }
        dist_m <- haversine_m(input$geo_lat, input$geo_lon, entry$lat, entry$lon)
        radius <- if (is.null(entry$geo_radius)) GEO_RADIUS_METERS else entry$geo_radius
        if (dist_m > radius) {
          showNotification(
            sprintf("📍 You appear to be too far from the classroom (%.0f m):", dist_m),
            type = "error"
          )
          return()
        }
      }

      # Marquer immédiatement dans le thread principal (anti race condition)
      submitted_presences <<- c(submitted_presences, dedup_key)
      if (nzchar(fp)) {
        device_fingerprints[[entry$session_id]] <<- c(
          device_fingerprints[[entry$session_id]] %||% character(0), fp
        )
      }
      shinyjs::disable("submit")
      updateActionButton(session, "submit", label = "⏳ Submitting...")

      # Capturer les valeurs réactives avant d'entrer dans le future
      parts    <- strsplit(entry$session_id, "_")[[1]]
      lecture  <- parts[1]
      date     <- parts[2]
      presence <- tibble(
        ts           = format(now_utc(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        email        = email,
        lecture      = lecture,
        date         = date,
        student_id   = input$sid,
        student_lnid = input$lnid,
        student_fnid = input$fnid,
        master       = input$master,
        token        = token,
        geo_accuracy = input$geo_accuracy %||% NA_integer_,
        ok           = TRUE
      ) |>
        mutate_all(as.character) |>
        mutate_all(\(x) ifelse(x == "NULL", NA, x))

      # Écriture Sheets en arrière-plan
      current_token <- googlesheets4::gs4_token()
      future({
        googlesheets4::gs4_auth(token = current_token)
        googlesheets4::sheet_append(SHEET_ID, sheet = SHEET_NAME_LOG, data = presence)
      }) %...>% (function(...) {
        p_student$check_success <- TRUE
        showNotification(glue("✓ Attendance recorded for {email}"), type = "message")
        updateActionButton(session, "submit", label = "✔️ Submitted")
      }) %...!% (function(err) {
        showNotification("Error recording attendance. Please retry.", type = "error")
        submitted_presences <<- setdiff(submitted_presences, dedup_key)
        shinyjs::enable("submit")
        updateActionButton(session, "submit", label = "I'm present")
      })
    })
    
    output$success_msg <- renderText({
      if (p_student$check_success) {
        "✓ Attendance recorded. You can close this window."
      } else {
        ""
      }
    })
    
    
    
  })
}