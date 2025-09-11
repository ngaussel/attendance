mod_emargement_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    div(class = "container",
        titlePanel("Attendance Presenter View"),
        fluidRow(
          column(3,selectInput(ns("lecture"), "Lecture", choices=COURSE_MENU, selected=COURSE_MENU[1])),
          column(3,dateInput(ns("date"), "Date", value = Sys.Date()))
        ),
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
      
      tagList(
        tags$br(),
        imageOutput(ns("qr_img"), width = "450px", height = "450px"),
        div("Token: ", code(textOutput(ns("tok_txt"), inline = TRUE))),
        div("Check-in URL: ", uiOutput(ns("landing_link"))),
        div("Next refresh in: ", textOutput(ns("rotate_cd"), inline = TRUE))
      )
    })
    
    
  })
}