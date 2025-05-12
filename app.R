# app.R
library(shiny)
library(tidyverse)

VExUStexto <- function(input, vu_score) {
  if (input$renal == "no evaluado") {
    paste("Grado VExUS:", vu_score, "\n",
          "Patron de vena renal no evaluado")
  } else {
    paste("Grado VExUS:", vu_score)
  }
}

ui <- fluidPage(
  titlePanel("nePOCUS"),
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        h2("LUS-HF"),
        img(src = "lus.jpeg", height = 550, width = 325),
        # numero de lineas B en LUS
        h3("Número de líneas B en LUS"),
        sliderInput(
          inputId = "lines",
          label   = "R1",
          min     = 0,
          max     = 10,
          value   = 0,
          step    = 1
        ),
        sliderInput(
          inputId = "lines2",
          label   = "R2",
          min     = 0,
          max     = 10,
          value   = 0,
          step    = 1
        ),
        sliderInput(
          inputId = "lines3",
          label   = "R3",
          min     = 0,
          max     = 10,
          value   = 0,
          step    = 1
        ),
        sliderInput(
          inputId = "lines4",
          label   = "R4",
          min     = 0,
          max     = 10,
          value   = 0,
          step    = 1
        ),
        sliderInput(
          inputId = "lines5",
          label   = "L1",
          min     = 0,
          max     = 10,
          value   = 0,
          step    = 1
        ),
        sliderInput(
          inputId = "lines6",
          label   = "L2",
          min     = 0,
          max     = 10,
          value   = 0,
          step    = 1
        ),
        sliderInput(
          inputId = "lines7",
          label   = "L3",
          min     = 0,
          max     = 10,
          value   = 0,
          step    = 1
        ),
        sliderInput(
          inputId = "lines8",
          label   = "L4",
          min     = 0,
          max     = 10,
          value   = 0,
          step    = 1
        )
      ),
      wellPanel(
        h2("VExUS"),
        img(src = "diagrama.jpeg", height = 550, width = 325),
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
          label   = "Índice de pulsatilidad vena porta (%)",
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
            "No evaluado"                                                  = "no evaluado",
            "Continuo monophasic (Normal)"                                 = "normal",
            "Bifásico discontinuo (Levemente anormal)"                     = "mild",
            "Monofásico discontinuo solo diastólico (Severamente anormal)" = "severe"
          )
        )
      )),
    mainPanel(
      h4("Resultado POCUS:"),
      verbatimTextOutput("LUSHF"),
      verbatimTextOutput("VEXUS")
    ),
  )
)


server <- function(input, output, session) {
  
  ### 1) Calcular número total de líneas B
  total_lines <- reactive({
    sum(
      input$lines, input$lines2, input$lines3, input$lines4,
      input$lines5, input$lines6, input$lines7, input$lines8
    )
  })
  
  ### 2) Lógica LUS-HF según protocolo (>3 B-lines = congestión)
  congestion_flag <- reactive({
    if (total_lines() <= 3) {
      "No (≤ 3 líneas B)"
    } else {
      "Sí (> 3 líneas B)"
    }
  })
  
  ### 3) Renderizar texto de LUS-HF
  output$LUSHF <- renderText({
    paste0(
      "Total líneas B: ", total_lines(), "\n",
      "¿Congestión pulmonar? ", congestion_flag()
    )
  })
  
  ### 4) Lógica de VExUS (igual que antes, sólo ajustes de salida)
  portal_sev <- reactive({
    if (input$portal < 30) {
      "normal"
    } else if (input$portal < 50) {
      "mild"
    } else {
      "severe"
    }
  })
  
  vu_score <- reactive({
    if (input$ivc < 2) {
      0
    } else {
      severos <- sum(
        input$hepatic == "severe",
        portal_sev()    == "severe",
        input$renal     == "severe"
      )
      as.numeric(switch(
        as.character(min(severos, 3)),
        "0" = 1, "1" = 2, "2" = 3, "3" = 3
      ))
    }
  })
  
  output$VEXUS <- renderText({
    VExUStexto(input, vu_score())
  })
}

shinyApp(ui, server)
