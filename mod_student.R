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
      df <- googlesheets4::read_sheet(SHEET_ID, sheet = SHEET_NAME_TOKENS)
      names(df) <- tolower(names(df))
      i <- which(df$token == token)
      if (length(i) == 0) return("Unknown session")
      df$session_id[i]
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
      
      df <- googlesheets4::read_sheet(SHEET_ID, sheet = SHEET_NAME_TOKENS)
      names(df) <- tolower(names(df))
      i <- which(df$token == token)
      if (length(i) == 0) {
        showNotification("Invalid token.", type = "error")
        return()
      }
      
      
      row <- df[i, , drop = FALSE]
      email <- tolower(trimws(input$email %||% ""))
      
      if (!is_valid_domain_email(email, DOMAIN)) {
        showNotification(glue("Please use your @{DOMAIN} email."), type = "error")
        return()
      }
      
      if (input$sid=="") {
        showNotification(glue("Please enter your student Id"), type = "error")
        return()
      }
      
      # Marquer comme utilisé
      googlesheets4::range_write(
        data = tibble(used = TRUE),
        ss = SHEET_ID,
        sheet = SHEET_NAME_TOKENS,
        range = glue("E{i + 1}"),
        col_names = FALSE
      )
      
      # Découper session_id en lecture et date
      parts <- strsplit(row$session_id, "_")[[1]]
      lecture <- parts[1]
      date <- parts[2]
      
      # Log simplifié
    
      presence = tibble(
        ts         = format(now_utc(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        email      = email,
        lecture    = lecture,
        date       = date,
        student_id = input$sid,
        student_lnid = input$lnid,
        student_fnid = input$fnid,
        token      = row$token,
        ok         = TRUE
      ) |> 
        mutate_all(as.character) |> 
        mutate_all(\(x) {ifelse(x=="NULL",NA,x)})
      
      
      
      googlesheets4::sheet_append(SHEET_ID, sheet = SHEET_NAME_LOG, data = presence)
      
      p_student$check_success <- TRUE
      showNotification(glue("✓ Attendance recorded for {email}"), type = "message")
      params$nsubmit=params$nsubmit+1
      shinyjs::disable("submit")
      updateActionButton(session, "submit", label = "✔️ Submitted", disabled = TRUE)
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