# Set the application-level cache -----------------------------------------------
shinyOptions(cache = cachem::cache_mem(max_size = 500e6))

# Load libraries ----------------------------------------------------------
library(shiny)
library(bslib)
library(dplyr)

# User Interface Object
ui <- page_navbar(
  
  # App theme ----
  theme = bs_theme(version = 5, bootswatch = "minty"),
  
  # App title ----
  title = h2("PISAAC"),
  
  # fillable = T,
  
  nav_spacer(),

  # Panel Protocolo 1
  nav_panel(
    title = "Protocolo 1",
    
    # Tabs
    navset_card_tab(
      
      title = "",
      # Tab de Datos
      nav_panel(
        title = "Datos",
        # Card de Botones
        card(
          max_height = 60,
          layout_column_wrap(
            width = 1/2, # Tres columnas
            actionButton("btn1", "Cargar Datos", class = "btn-primary", disabled = F),
            actionButton("btn3", "Transferir Datos", class = "btn-primary", disabled = T)
          )
        ),
        # Card de tabla
        card(
          full_screen = TRUE,
          tableOutput("data_lab_1"),
        )
      ),
      # Tab de Resultados
      nav_panel(
        title = "Resultados",
        # Card de Botones
        card(
          max_height = 60,
          layout_column_wrap(
            width = 1/2, # Tres columnas
            actionButton("btn2", "Cargar Resultados", class = "btn-primary", disabled = F),
            actionButton("btn4", "Transferir Resultados", class = "btn-primary", disabled = T)
          )
        ),
        # Card de tabla
        card(
          full_screen = TRUE,
          tableOutput("results_lab_1")
        )
      )
    )
  )
  

  
)

server <- function(input, output, session) {
  
  
  # Botones de data 
  observeEvent(input$btn1, {
    withProgress(
      min = 1,
      max = 2,
      # message = 'Calculation in progress',
      # detail = 'This may take a while...',

      {
        setProgress(1, message = "Calculation in progress", detail = 'This may take a while...')
        googlesheets4::gs4_auth(cache = ".secrets", email = "renato.cava@upch.pe")
        source(file = "data_lab_1.R")
        setProgress(2, message = "Finished!", detail = "")
        Sys.sleep(1)
      }
    )

    output$data_lab_1 <- renderTable(data_lab_1)

    updateActionButton(session, "btn1", disabled = T)

    updateActionButton(session, "btn3", disabled = F)

  })
  
  observeEvent(input$btn3, {
    withProgress(
      min = 1,
      max = 2,
      # message = 'Calculation in progress',
      # detail = 'This may take a while...',

      {
        setProgress(1, message = "Calculation in progress", detail = 'This may take a while...')
        googlesheets4::sheet_append(ss = "https://docs.google.com/spreadsheets/d/1G92S2BquXfDpBHtarxn38e3N1Tev6tLdTruXZdSZM_M/edit#gid=2104432393", data = data_lab, sheet = "PISAAC_DATA_LABORATORIO")
        setProgress(2, message = "Finished!", detail = "")
        Sys.sleep(1)
      }
    )

    updateActionButton(session, "btn3", disabled = T)

    updateActionButton(session, "btn1", disabled = F)

  })
  
  # Botones de resultados
  observeEvent(input$btn2, {
    withProgress(
      min = 1,
      max = 2,
      # message = 'Calculation in progress',
      # detail = 'This may take a while...',
      
      {
        setProgress(1, message = "Calculation in progress", detail = 'This may take a while...')
        googlesheets4::gs4_auth(cache = ".secrets", email = "renato.cava@upch.pe")
        source(file = "results_lab_1.R")
        setProgress(2, message = "Finished!", detail = "")
        Sys.sleep(1)
      }
    )
    
    output$results_lab_1 <- renderTable(results_lab_1)
    
    updateActionButton(session, "btn2", disabled = T)
    
    updateActionButton(session, "btn4", disabled = F)
    
  })
  
  observeEvent(input$btn4, {
    withProgress(
      min = 1,
      max = 2,
      # message = 'Calculation in progress',
      # detail = 'This may take a while...',
      
      {
        setProgress(1, message = "Calculation in progress", detail = 'This may take a while...')
        googlesheets4::sheet_append(ss = "https://docs.google.com/spreadsheets/d/1rS3yanBpJZdL5blHZuRNokaObOSCVdN0rl2pL-QpmwA/edit#gid=1236108235", data = results_lab_1, sheet = "04ABR2022")
        setProgress(2, message = "Finished!", detail = "")
        Sys.sleep(1)
      }
    )
    
    updateActionButton(session, "btn4", disabled = T)
    
    updateActionButton(session, "btn2", disabled = F)
    
  })
  
 
}

shinyApp(ui, server)