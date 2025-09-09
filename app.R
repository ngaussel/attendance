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
ROSTER_SHEET_NAME  <- "roster"
DOMAIN             <- "etu.univ-paris1.fr"
REQUIRE_ROSTER     <- FALSE

TOKEN_TTL_SECONDS  <- 25L
FILL_SECONDS <- 30L
TOKEN_LENGTH       <- 8L
ALPHABET           <- "ABCDEFGHJKLMNPQRSTUVWXYZ23456789"

# --- HELPERS (ajoute ici tes fonctions helpers)
source("helper.R")

# --- UI --------------------------------------------------------------------
student_ui <- function() {
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    div(class = "container",
      tags$img(src = "sorbonne_logo.png", id = "sorbonne-logo"),
      h2("Attendance Check-in"),
      div(class = "ok", "Valid token ✅"),

    p(
      span(class = "muted", "Session: "),
      textOutput("session_info", inline = TRUE)
    ), 
    
    p(span(class = "muted", "This link expires in ", strong(textOutput("cd", inline = TRUE)))),
    
    div(id = "form_zone",  # zone du formulaire
        textInput("email", "Institutional email (required)", placeholder = glue::glue("name@{DOMAIN}")),
        textInput("lnid", "Student Last Name", placeholder = "..."),
        textInput("fnid", "Student First Name", placeholder = "..."),
        actionButton("submit", "I'm present", class = "btn btn-primary")
    ),
    
    div(id = "feedback_zone",
        div(class = "ok", textOutput("success_msg")),
        div(class = "err", textOutput("error_msg"))
    )
  )
  )
}

presenter_ui <- function() {
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
  div(class = "container",
    tags$img(src = "sorbonne_logo.png", id = "sorbonne-logo"),
    titlePanel("Attendance Presenter View"),
    fluidRow(
      textInput("lecture", "Lecture", placeholder = "ex: Finance"),
      dateInput("date", "Date", value = Sys.Date())
    ),
    actionButton("start_session", "Launch", class = "btn btn-primary"),
    br(), br(),
    conditionalPanel(
      condition = "output.showQR == true",
      imageOutput("qr_img", width = "350px", height = "350px"),
      div("Token: ", code(textOutput("tok_txt", inline = TRUE))),
      div("Check-in URL: ", uiOutput("landing_link")),
      div("Next refresh in: ", textOutput("rotate_cd", inline = TRUE))
    )
  )
  )
}


# --- DYNAMIC UI: route vers la bonne vue -----------------------------------
ui <- function(request) {
  query <- parseQueryString(request$QUERY_STRING)
  if (!is.null(query$t)) {
    student_ui()
  } else {
    presenter_ui()
  }
}



# --- SERVER ----------------------------------------------------------------

server <- function(input, output, session) {

    # ensure_headers() Rajouter éventuellement un check des onglets de la Google Sheet
  
  current <- reactiveVal(list())
  params  <- reactiveValues(token = NULL, 
                            started = NULL, 
                            check_success = FALSE,
                            session_active=FALSE)
  
  token_timer <- reactiveTimer(TOKEN_TTL_SECONDS * 1000, session)
  
  
  
  # --- RÉCUPÉRATION DU TOKEN VIA URL --------------------------
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$t)) {
      params$token <- query$t
      params$started <- Sys.time()
    }
  })
  
  # --- PRÉSENTATEUR ------------------------------------------
  observeEvent(input$start_session, {
    req(nzchar(input$lecture), !is.null(input$date))
    params$session_active <- TRUE
  })  
  
  observe({
      req(nzchar(input$lecture), !is.null(input$date))
      req(params$session_active)
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
      
      googlesheets4::sheet_append(SHEET_ID, sheet = SHEET_NAME_TOKENS, data = tibble(
        token = tkn,
        session_id = session_id,
        issued_at = format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        expires_at = format(exp,  "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        used = FALSE
      ))
      
      current(list(
        token = tkn,
        landingUrl = landing,
        expiresAt = exp,
        sessionId = session_id
      ))
    })
  
  output$showQR <- reactive({
    !is.null(current()$token)
  })
  outputOptions(output, "showQR", suspendWhenHidden = FALSE)
  
  
  output$qr_img <- renderImage({
    c <- current()
    req(c$landingUrl)
    tmp <- tempfile(fileext = ".png")
    qr <- qrencode_raw(c$landingUrl)
    qr_resized <- qr[rep(1:nrow(qr), each = 13), rep(1:ncol(qr), each = 13)]
    writePNG(qr_resized, target = tmp)
    list(src = tmp, contentType = "image/png", alt = "QR code")
  }, deleteFile = TRUE)
  
  output$tok_txt <- renderText({ req(current()$token); current()$token })
  output$landing_link <- renderUI({
    req(current()$landingUrl)
    tags$a(href = current()$landingUrl, target = "_blank", current()$landingUrl)
  })
  
  output$rotate_cd <- renderText({
    invalidateLater(500, session)
    remaining <- difftime(current()$expiresAt, Sys.time(), units = "secs")
    sprintf("%.0f s", max(0, as.numeric(remaining)))
  })
  
  # --- ÉTUDIANT -------------------------------------------------------------
  sessionStartTime <- reactiveVal(NULL)
  
  observe({
    req(params$token)
    if (is.null(sessionStartTime())) {
      sessionStartTime(Sys.time())
    }
  })
  
  output$session_info <- renderText({
    req(params$token)
    df <- googlesheets4::read_sheet(SHEET_ID, sheet = SHEET_NAME_TOKENS)
    names(df) <- tolower(names(df))
    i <- which(df$token == params$token)
    if (length(i) == 0) return("Unknown session")
    df$session_id[i]
  })
  
  output$cd <- renderText({
    invalidateLater(1000, session)
    start <- sessionStartTime()
    if (is.null(start)) return("")
    
    remaining <- FILL_SECONDS - as.numeric(difftime(Sys.time(), start, units = "secs"))
    sprintf("%.0f s", max(0, remaining))
  })
  
  observeEvent(input$submit, {
    req(params$token)
    
    df <- googlesheets4::read_sheet(SHEET_ID, sheet = SHEET_NAME_TOKENS)
    names(df) <- tolower(names(df))
    i <- which(df$token == params$token)
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
    
    elapsed <- as.numeric(difftime(Sys.time(), sessionStartTime(), units = "secs"))
    if (elapsed > FILL_SECONDS) {
      showNotification("⏳ Time's up! The form has expired.", type = "error")
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
    googlesheets4::sheet_append(SHEET_ID, sheet = SHEET_NAME_LOG, data = tibble(
      ts         = format(now_utc(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      email      = email,
      lecture    = lecture,
      date       = date,
      student_lnid = input$lnid,
      student_fnid = input$fnid,
      token      = row$token,
      ok         = TRUE
    ))
    
    params$check_success <- TRUE
    showNotification(glue("✓ Attendance recorded for {email}"), type = "message")
    shinyjs::disable("submit")
    updateActionButton(session, "submit", label = "✔️ Submitted", disabled = TRUE)
  })
  
  output$success_msg <- renderText({
    if (params$check_success) {
      "✓ Attendance recorded. You can close this window."
    } else {
      ""
    }
  })
}





# --- LANCE APP -------------------------------------------------------------
shinyApp(ui, server)
