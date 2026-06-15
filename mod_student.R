mod_student_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    div(class = "container",
        tags$img(src = "sorbonne_logo.png", id = "sorbonne-logo"),
        h2("Attendance Check-in"),
        div(class = "ok", "Valid token ✅"),
        
        p(
          span(class = "muted", "Session: "),
          textOutput(ns("session_info"), inline = TRUE)
        ), 
        
        p(span(class = "muted", "This link expires in ", strong(textOutput(ns("cd"), inline = TRUE)))),
        
        div(id = "form_zone",  # zone du formulaire
            textInput(ns("email"), "Institutional email (required)", placeholder = glue::glue("name@{DOMAIN}")),
            textInput(ns("sid"), "Student Id (required)", placeholder = "123456..."),
            selectInput(ns("master"),"Master(required)",choices=c("IRFA","MMMEF","Other"),selected="IRFA"),
            textInput(ns("lnid"), "Student Last Name", placeholder = "..."),
            textInput(ns("fnid"), "Student First Name", placeholder = "..."),
            
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
    
    p_student<- reactiveValues(check_success = FALSE)
    sessionStartTime <- reactiveVal(NULL)
    
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
      if (token %in% used_tokens) {
        showNotification("Already submitted.", type = "warning")
        return()
      }

      email <- tolower(trimws(input$email %||% ""))
      if (!is_valid_domain_email(email, DOMAIN)) {
        showNotification(glue("Please use your @{DOMAIN} email."), type = "error")
        return()
      }
      if (input$sid == "") {
        showNotification("Please enter your student Id", type = "error")
        return()
      }

      # Marquer le token consommé immédiatement dans le thread principal
      used_tokens <<- c(used_tokens, token)
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
        ok           = TRUE
      ) |>
        mutate_all(as.character) |>
        mutate_all(\(x) ifelse(x == "NULL", NA, x))

      # Écriture Sheets en arrière-plan
      future({
        googlesheets4::gs4_auth(path = Sys.getenv("GS_SERVICE_ACCOUNT_JSON"))
        googlesheets4::sheet_append(SHEET_ID, sheet = SHEET_NAME_LOG, data = presence)
      }) %...>% (function(...) {
        p_student$check_success <- TRUE
        showNotification(glue("✓ Attendance recorded for {email}"), type = "message")
        updateActionButton(session, "submit", label = "✔️ Submitted")
      }) %...!% (function(err) {
        showNotification("Error recording attendance. Please retry.", type = "error")
        used_tokens <<- setdiff(used_tokens, token)  # libérer le token si l'écriture échoue
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