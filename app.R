# Set the application-level cache -----------------------------------------------
shinyOptions(cache = cachem::cache_mem(max_size = 500e6))

# Load libraries ----------------------------------------------------------
library(shiny)
library(bslib)
library(dplyr)
library(shinymanager)


# User Interface Object
ui <- page_navbar(
  
  # footer = tags$p(verbatimTextOutput("auth_output")),
  
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
            actionButton(inputId = "btn1", label = "Cargar Datos", class = "btn-primary", disabled = F),
            actionButton(inputId = "btn2", label = "Transferir Datos", class = "btn-primary", disabled = T)
          )
        ),
        # Card de tabla
        card(
          full_screen = TRUE,
          DT::dataTableOutput(outputId = "data_lab_1"),
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
            actionButton(inputId = "btn3", label = "Cargar Resultados", class = "btn-primary", disabled = F),
            actionButton(inputId = "btn4", label = "Transferir Resultados", class = "btn-primary", disabled = T)
          )
        ),
        # Card de tabla
        card(
          full_screen = TRUE,
          tableOutput(outputId = "results_lab_1")
        )
      )
    )
  ),
  
  # Panel Protocolo 2
  nav_panel(
    title = "Protocolo 2",
    
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
            actionButton(inputId = "btn5", label = "Cargar Datos", class = "btn-primary", disabled = F),
            actionButton(inputId = "btn6", label = "Transferir Datos", class = "btn-primary", disabled = T)
          )
        ),
        # Card de tabla
        card(
          full_screen = TRUE,
          DT::dataTableOutput(outputId = "data_lab_2"),
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
            actionButton(inputId = "btn7", label = "Cargar Resultados", class = "btn-primary", disabled = F),
            actionButton(inputId = "btn8", label = "Transferir Resultados", class = "btn-primary", disabled = T)
          )
        ),
        # Card de tabla
        card(
          full_screen = TRUE,
          tableOutput(outputId = "results_lab_2")
        )
      ),
      
      # Tab de Pediatria SB
      nav_panel(
        title = "Pediatria SB",
        
        # Card de Botones
        card(
          max_height = 60,
          layout_column_wrap(
            width = 1/2, # Tres columnas
            actionButton(inputId = "btn9", label = "Cargar Pediatria SB", class = "btn-primary", disabled = F),
            actionButton(inputId = "btn10", label = "Transferir Pediatria SB", class = "btn-primary", disabled = T)
          )
        ),
        # Card de tabla
        card(
          full_screen = TRUE,
          tableOutput(outputId = "results_lab_1")
        )
      ),
      
      # Tab de Pediatria SB
      nav_panel(
        title = "Xpert Pediatricos",
        
        # Card de Botones
        card(
          max_height = 60,
          layout_column_wrap(
            width = 1/2, # Tres columnas
            actionButton(inputId = "btn11", label = "Cargar Xpert Pediatricos", class = "btn-primary", disabled = F),
            actionButton(inputId = "btn12", label = "Transferir Xpert Pediatricos", class = "btn-primary", disabled = T)
          )
        ),
        # Card de tabla
        card(
          full_screen = TRUE,
          tableOutput(outputId = "results_lab_1")
        )
      )
    )
  )
  

  
)

# Wrap your UI with secure_app
ui <- secure_app(ui = ui, enable_admin = T)

server <- function(input, output, session) {
  
  # call the server part
  # check_credentials returns a function to authenticate users
  # res_auth <- secure_server(
  #   check_credentials = check_credentials(credentials)
  # )
  res_auth <- secure_server(
    check_credentials = check_credentials(
      "db/database.sqlite",
      # passphrase = key_get("R-shinymanager-key", "obiwankenobi")
      passphrase = Sys.getenv("sql_db_pass")
    )
  )
  
  # output$auth_output <- renderPrint({
  #   reactiveValuesToList(res_auth)
  # })
  
  
  # Protocolo 1
  
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

    output$data_lab_1 <- DT::renderDataTable(
      
      data_lab_1,
      
      options = list(
        paging = F,    ## paginate the output
        pageLength = 10,  ## number of rows to output for each page
        scrollX = F,   ## enable scrolling on X axis
        scrollY = F,   ## enable scrolling on Y axis
        autoWidth = TRUE, ## use smart column width handling
        server = FALSE,   ## use client-side processing
        dom = 't',
        # buttons = c('csv', 'excel'),
        columnDefs = list(
          list(targets = '_all', className = 'dt-center')
        )
                     
      ),
      # extensions = 'Buttons',
      # selection = 'single', ## enable selection of a single row
      # filter = 'bottom',              ## include column filters at the bottom
      rownames = FALSE,                ## don't show row numbers/names
      filter = "none"
    )

    updateActionButton(session, "btn1", disabled = T)

    updateActionButton(session, "btn2", disabled = F)

  })
  
  observeEvent(input$btn2, {
    withProgress(
      min = 1,
      max = 2,
      # message = 'Calculation in progress',
      # detail = 'This may take a while...',

      {
        setProgress(1, message = "Calculation in progress", detail = 'This may take a while...')
        googlesheets4::sheet_append(ss = "https://docs.google.com/spreadsheets/d/1G92S2BquXfDpBHtarxn38e3N1Tev6tLdTruXZdSZM_M/edit#gid=2104432393", data = data_lab_1, sheet = "PISAAC_DATA_LABORATORIO")
        setProgress(2, message = "Finished!", detail = "")
        Sys.sleep(1)
      }
    )

    updateActionButton(session, "btn2", disabled = T)

    updateActionButton(session, "btn1", disabled = F)

  })
  
  # Botones de resultados
  observeEvent(input$btn3, {
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
    
    output$results_lab_1 <- DT::renderDataTable(
      
      results_lab_1,
      
      options = list(
        paging = F,    ## paginate the output
        pageLength = 10,  ## number of rows to output for each page
        scrollX = F,   ## enable scrolling on X axis
        scrollY = F,   ## enable scrolling on Y axis
        autoWidth = TRUE, ## use smart column width handling
        server = FALSE,   ## use client-side processing
        dom = 't',
        # buttons = c('csv', 'excel'),
        columnDefs = list(
          list(targets = '_all', className = 'dt-center')
        )
        
      ),
      # extensions = 'Buttons',
      # selection = 'single', ## enable selection of a single row
      # filter = 'bottom',              ## include column filters at the bottom
      rownames = FALSE,                ## don't show row numbers/names
      filter = "none"
    )
    
    updateActionButton(session, "btn3", disabled = T)
    
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
    
    updateActionButton(session, "btn3", disabled = F)
    
  })
  
  
  # Protocolo 2
  
  # Botones de data 
  observeEvent(input$btn5, {
    withProgress(
      min = 1,
      max = 2,
      # message = 'Calculation in progress',
      # detail = 'This may take a while...',
      
      {
        setProgress(1, message = "Calculation in progress", detail = 'This may take a while...')
        googlesheets4::gs4_auth(cache = ".secrets", email = "renato.cava@upch.pe")
        source(file = "data_lab_2.R")
        setProgress(2, message = "Finished!", detail = "")
        Sys.sleep(1)
      }
    )
    
    output$data_lab_2 <- DT::renderDataTable(
      
      data_lab_2,
      
      options = list(
        paging = F,    ## paginate the output
        pageLength = 10,  ## number of rows to output for each page
        scrollX = T,   ## enable scrolling on X axis
        scrollY = T,   ## enable scrolling on Y axis
        autoWidth = TRUE, ## use smart column width handling
        server = FALSE,   ## use client-side processing
        dom = 't',
        # buttons = c('csv', 'excel'),
        columnDefs = list(
          list(targets = '_all', className = 'dt-center')
        )
        
      ),
      # extensions = 'Buttons',
      # selection = 'single', ## enable selection of a single row
      # filter = 'bottom',              ## include column filters at the bottom
      rownames = FALSE,                ## don't show row numbers/names
      filter = "none"
    )
    
    updateActionButton(session, "btn5", disabled = T)
    
    updateActionButton(session, "btn6", disabled = F)
    
  })
  
  observeEvent(input$btn6, {
    withProgress(
      min = 1,
      max = 2,
      # message = 'Calculation in progress',
      # detail = 'This may take a while...',
      
      {
        setProgress(1, message = "Calculation in progress", detail = 'This may take a while...')
        googlesheets4::sheet_write(ss = "https://docs.google.com/spreadsheets/d/1-3C1Mem6icPRbqrHzp-MxZMFORx5BzaXPDzjpowvnRQ/edit#gid=0", data = data_lab_2, sheet = "Datos")
        setProgress(2, message = "Finished!", detail = "")
        Sys.sleep(1)
      }
    )
    
    updateActionButton(session, "btn6", disabled = T)
    
    updateActionButton(session, "btn5", disabled = F)
    
  })
  
  # Botones de resultados
  observeEvent(input$btn7, {
    withProgress(
      min = 1,
      max = 2,
      # message = 'Calculation in progress',
      # detail = 'This may take a while...',
      
      {
        setProgress(1, message = "Calculation in progress", detail = 'This may take a while...')
        googlesheets4::gs4_auth(cache = ".secrets", email = "renato.cava@upch.pe")
        source(file = "results_lab_2.R")
        setProgress(2, message = "Finished!", detail = "")
        Sys.sleep(1)
      }
    )
    
    output$results_lab_2 <- DT::renderDataTable(
      
      results_lab_2,
      
      options = list(
        paging = F,    ## paginate the output
        pageLength = 10,  ## number of rows to output for each page
        scrollX = T,   ## enable scrolling on X axis
        scrollY = T,   ## enable scrolling on Y axis
        autoWidth = TRUE, ## use smart column width handling
        server = FALSE,   ## use client-side processing
        dom = 't',
        # buttons = c('csv', 'excel'),
        columnDefs = list(
          list(targets = '_all', className = 'dt-center')
        )
        
      ),
      # extensions = 'Buttons',
      # selection = 'single', ## enable selection of a single row
      # filter = 'bottom',              ## include column filters at the bottom
      rownames = FALSE,                ## don't show row numbers/names
      filter = "none"
    )
    
    updateActionButton(session, "btn7", disabled = T)
    
    updateActionButton(session, "btn8", disabled = F)
    
  })
  
  observeEvent(input$btn8, {
    withProgress(
      min = 1,
      max = 2,
      # message = 'Calculation in progress',
      # detail = 'This may take a while...',
      
      {
        setProgress(1, message = "Calculation in progress", detail = 'This may take a while...')
        googlesheets4::sheet_append(ss = "https://docs.google.com/spreadsheets/d/1G1-erEgp3brQaGydKSJcEzJMRhuFkNtheWSeDPPixrU/edit#gid=0", data = results_lab_2, sheet = "04ABR2022")
        setProgress(2, message = "Finished!", detail = "")
        Sys.sleep(1)
      }
    )
    
    updateActionButton(session, "btn8", disabled = T)
    
    updateActionButton(session, "btn7", disabled = F)
    
  })
  
 
}

shinyApp(ui, server)