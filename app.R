# CARGA DE PAQUETES
if(!require(pacman)) install.packages("pacman")
if(!require(devtools)) install.packages("devtools")
if(!require(SMRD)) devtools::install_github("Auburngrads/SMRD")
pacman::p_load("shiny","rintrojs","gt","SMRD","readxl")#,"tidyverse"

carpeta <- "datos"
archivos <- list.files(carpeta, pattern = "\\.RData$", full.names = TRUE)
for (archivo in archivos) {
  load(archivo)
}

ui <- fluidPage(
  introjsUI(),
  
  tags$div(
    style = "display: flex; justify-content: center; align-items: center; gap: 5px; margin-top: 5px; margin-bottom: 5px;",
    tags$img(src = "logo3.png", height = "100px", alt = "Logo UNAL"),
    tags$h2("Comparación de las MCF de dos muestras", style = "margin: 0; font-weight: bold;")
  ),
  tags$hr(style = "margin-bottom: 10px;"),
  
  actionButton("ayuda", "📘 Mostrar tutorial", class = "btn-primary", style = "margin-bottom: 15px;"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("data_input", "Fuente de los datos:",
                   choices = list("Bases de datos de R" = "predefined",
                                  "Subir archivos propios" = "upload")) %>%
        tagAppendAttributes(`data-step` = 1, `data-intro` = "Selecciona si deseas usar una 
                            base de datos predefinida o subir tus propios archivos."),
      
      
      uiOutput("data_ui1"),
      uiOutput("data_ui2"),
      
      hr(),
      
      h4("Variables Base de datos 1"),
      uiOutput("col_id1")%>%
        tagAppendAttributes(`data-step` = 7,`data-intro` = "Selecciona la variable de identificación del equipo o sistema en la primera base de datos."),
      uiOutput("col_time1") %>%
        tagAppendAttributes(`data-step` = 8,`data-intro` = "Selecciona la variable de tiempo a falla o censura en la primera base de datos."),
      uiOutput("col_event1")  %>%
        tagAppendAttributes(`data-step` = 9,`data-intro` = "Selecciona la variable indicadora de falla o censura en la primera base de datos."),
      
      h4("Variables Base de datos 2"),
      uiOutput("col_id2") %>%
        tagAppendAttributes(`data-step` = 10,`data-intro` = "Selecciona la variable de identificación del equipo o sistema en la segunda base de datos."),
      uiOutput("col_time2") %>%
        tagAppendAttributes(`data-step` = 11,`data-intro` = "Selecciona la variable de tiempo a falla o censura en la segunda base de datos."),
      uiOutput("col_event2")  %>%
        tagAppendAttributes(`data-step` = 12,`data-intro` = "Selecciona la variable indicadora de falla o censura en la segunda base de datos."),
      h4("Autores:"),
      tags$ul(
        tags$li(tags$a(href="mailto:datobonv@unal.edu.co", "Davinson Tobón-Viana")),
        tags$li(tags$a(href="mailto:mcjarami@unal.edu.co", "Mario C. Jaramillo-Elorza")),
        tags$li(tags$a(href="mailto:cmlopera@unal.edu.co", "Carlos M. Lopera-Gómez"))
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Resumen", 
                 verbatimTextOutput("summary_text") %>%
                   tagAppendAttributes(`data-step` = 13,`data-intro` = "En la pestaña resumen, podras visualizar un resumen estadístico de los dos conjuntos de datos convertidos al formato RDU, incluyendo información como número de recurrencias, tiempo minimo de las recurrencias, tiempo maximo de las recurrencias,etc. Y en la pestaña Gráficas, puedes visualizar la comparación de las curvas MCF (Mean Cumulative Function) para los dos conjuntos de datos. La primera gráfica muestra ambas curvas superpuestas, y la segunda muestra la diferencia entre ellas.") 
        ),
        
        tabPanel("Gráficas", 
                 div(id = "graficas_tab",
                     plotOutput("mcf_plot", height = "600px") 
                       
                 )
        ) %>%
        tagAppendAttributes(`data-step` = 14, `data-intro` = "En la pestaña Gráficas, podrás visualizar la comparación de las curvas MCF (Mean Cumulative Function) para las dos bases de datos seleccionadas o cargadas. La primera gráfica muestra ambas curvas superpuestas y la segunda muestra la diferencia entre ellas.")
        
      )
    )
  # ),
  # # Marca de agua
  # 
  # 
  # tags$div(
  #   style = "text-align: center; font-size: 13px; color: #888; margin-top: 40px; margin-bottom: 10px;",
  #   "Creado por Grupo de investigación en Estadística, Universidad Nacional de Colombia - Sede Medellín"
  )
)


server <- function(input, output, session) {
  
  observeEvent(input$ayuda, {
    introjs(session, options = list("nextLabel"="Siguiente", "prevLabel"="Anterior", "skipLabel"="Salir"))
  })
  
  bases_de_datos <- c("BrakingGrids", "ComputerLab", "Cylinder", 
                      "Grids1", "Grids2", "HPCRepairs", "MachineH", 
                      "SystemE", "ValveSeat", "WorkStation")
  
  read_input_data <- function(file_input, delimiter) {
    tryCatch({
      if (endsWith(file_input$name, ".xlsx")) {
        read_excel(file_input$datapath)
      } else {
        read.table(file_input$datapath, header = TRUE, sep = delimiter)
      }
    }, error = function(e) {
      NULL
    })
  }
  
  datos1 <- reactive({
    if (input$data_input == "predefined") {
      req(input$base_datos1)
      get(input$base_datos1)
    } else {
      req(input$file1)
      read_input_data(input$file1, input$delimiter1)
    }
  })
  
  datos2 <- reactive({
    if (input$data_input == "predefined") {
      req(input$base_datos2)
      get(input$base_datos2)
    } else {
      req(input$file2)
      read_input_data(input$file2, input$delimiter2)
    }
  })
  
  output$data_ui1 <- renderUI({
    if (input$data_input == "predefined") {
      selectInput("base_datos1", "Selecciona la base de datos 1:", choices = bases_de_datos) %>%
        tagAppendAttributes(`data-step` = 2, `data-intro` = "Selecciona la primera base de datos precargado.")
    } else {
      tagList(
        fileInput("file1", "Sube archivo para Dataset 1:") %>%
          tagAppendAttributes(`data-step` = 3, `data-intro` = "Sube tu archivo .csv o .xlsx."),
        selectInput("delimiter1", "Delimitador Dataset 1:",
                    choices = c("Coma" = ",", "Tab" = "\t", "Punto y coma" = ";", "Espacio" = " ")) %>%
          tagAppendAttributes(`data-step` = 4, `data-intro` = "Selecciona el tipo de separador del archivo.")
      )
    }
  })
  

  output$data_ui2 <- renderUI({
    if (input$data_input == "predefined") {
      selectInput("base_datos2", "Selecciona la base de datos 2:", choices = bases_de_datos) %>%
      tagAppendAttributes(`data-step` = 4, `data-intro` = "Selecciona el segundo conjunto de datos precargado.")
    } else {
      tagList(
        fileInput("file2", "Sube archivo para Dataset 2:")%>%
          tagAppendAttributes(`data-step` = 5, `data-intro` = "Sube tu archivo .csv o .xlsx."),
        selectInput("delimiter2", "Delimitador Dataset 2:",
                    choices = c("Coma" = ",", "Tab" = "\t", "Punto y coma" = ";", "Espacio" = " "))%>%
          tagAppendAttributes(`data-step` = 6, `data-intro` = "Selecciona el tipo de separador del archivo.")
      )
    }
  })
  
  observe({
    req(datos1())
    cols <- names(datos1())
    updateSelectInput(session, "col_id1", choices = cols, selected = cols[1])
    updateSelectInput(session, "col_time1", choices = cols, selected = cols[2])
    updateSelectInput(session, "col_event1", choices = cols, selected = cols[3])
  })
  
  observe({
    req(datos2())
    cols <- names(datos2())
    updateSelectInput(session, "col_id2", choices = cols, selected = cols[1])
    updateSelectInput(session, "col_time2", choices = cols, selected = cols[2])
    updateSelectInput(session, "col_event2", choices = cols, selected = cols[3])
  })
  
  output$col_id1 <- renderUI({
    req(datos1())
    selectInput("col_id1", "Variable ID:", choices = NULL)
  })
  
  output$col_time1 <- renderUI({
    req(datos1())
    selectInput("col_time1", "Variable tiempo:", choices = NULL)
  })
  
  output$col_event1 <- renderUI({
    req(datos1())
    selectInput("col_event1", "Variable tipo evento:", choices = NULL)
  })
  
  output$col_id2 <- renderUI({
    req(datos2())
    selectInput("col_id2", "Variable ID:", choices = NULL)
  })
  
  output$col_time2 <- renderUI({
    req(datos2())
    selectInput("col_time2", "Variable tiempo:", choices = NULL)
  })
  
  output$col_event2 <- renderUI({
    req(datos2())
    selectInput("col_event2", "Variable tipo evento:", choices = NULL)
  })
  
  rdu1 <- reactive({
    req(datos1(), input$col_id1, input$col_time1)
    frame.to.rdu(datos1(), ID.column = input$col_id1,
                          time.column = input$col_time1,
                          event.column = input$col_event1,
                          data.title = {input$base_datos1 %||% input$file1$name})
  })
  
  rdu2 <- reactive({
    req(datos2(), input$col_id2, input$col_time2)
    frame.to.rdu(datos2(), ID.column = input$col_id2,
                          time.column = input$col_time2,
                          event.column = input$col_event2,
                          data.title = {input$base_datos2 %||% input$file2$name})
  })
  
  output$mcf_plot <- renderPlot({
    req(rdu1(), rdu2())
    rdu1.mcf <- mcf(rdu1())
    rdu2.mcf <- mcf(rdu2())
    
   par(mfrow = c(2, 1))
    plot(rdu1.mcf$tuniq, rdu1.mcf$muHat, type = "s", col = "blue", ylab = "MCF", xlab = "Tiempo")
    lines(rdu2.mcf$tuniq, rdu2.mcf$muHat, type = "s", col = "red")
    legend("topleft", legend = c(rdu1.mcf$title, rdu2.mcf$title), col = c("blue", "red"), lty = 1)
    
    mcf.diff.plot(rdu1(), rdu2(), plot.seg = TRUE, 
                           xlab = "Tiempo", ylab = "Diferencia en MCF", title = "")
  })
  
  output$summary_text <- renderPrint({ 
    req(rdu1(), rdu2())
    summary(rdu1())
    summary(rdu2())
  })



observeEvent(input$start_tour, {
  introjs(session, options = list(steps = steps(), 
                                  nextLabel = "Siguiente",
                                  prevLabel = "Anterior", 
                                  doneLabel = "Finalizar"))
})
}





shinyApp(ui = ui, server = server)
