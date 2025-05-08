# app.R
library(shiny)

ui <- fluidPage(
  titlePanel("nePOCUS"),
  sidebarLayout(
    sidebarPanel(
      wellPanel(
      h2("VExUS"),
      img(src = "diagrama.jpeg", height = 550, width = 325, ),
      # Diámetro IVC
      numericInput(
        inputId = "ivc",
        label   = "Diámetro VCI (cm)",
        value   = 1.5,
        min     = 0,
        max     = 5,
        step    = 0.1
      ),
      # Patrones Doppler hepático
      selectInput(
        inputId = "hepatic",
        label   = "Doppler vena hepática",
        choices = c(
          "S > D (Normal)"             = "normal",
          "S < D (Mild Abn)"           = "mild",
          "S Reversal (Severe Abn)"    = "severe"
        )
      ),
      # Pulsatility Index portal
      sliderInput(
        inputId = "portal",
        label   = "Índice de pulsatilidad vena portal (%)",
        min     = 0,
        max     = 100,
        value   = 10,
        step    = 1
      ),
      # Patrones Doppler intrarrenal
      selectInput(
        inputId = "renal",
        label   = "Doppler vena intrarrenal",
        choices = c(
          "Continuo monophasic (Normal)"                   = "normal",
          "Bifásico discontinuo (Mild Abn)"                 = "mild",
          "Monofásico discontinuo solo diastólico (Severe)" = "severe"
        )
      )
    )),
    mainPanel(
      h4("Resultado VExUS:"),
      verbatimTextOutput("score")
    )
  )
)

server <- function(input, output, session) {
  output$score <- renderText({
    # 1) Categorizar severidad del índice portal
    portal_sev <- if (input$portal < 30) {
      "normal"
    } else if (input$portal < 50) {
      "mild"
    } else {
      "severe"
    }
    
    # 2) Lógica de cálculo de VExUS
    if (input$ivc < 2) {
      vu_score <- 0
    } else {
      # contar patrones severos entre hepático, portal e intrarrenal
      severos <- sum(
        input$hepatic == "severe",
        portal_sev      == "severe",
        input$renal    == "severe"
      )
      # asignar grado
      vu_score <- switch(
        as.character(min(severos, 3)),
        "0" = 1,
        "1" = 2,
        "2" = 3,
        "3" = 3
      )
    }
    
    # 3) Devolver texto en pantalla
    paste("Grado VExUS:", vu_score)
  })
}

shinyApp(ui, server)
