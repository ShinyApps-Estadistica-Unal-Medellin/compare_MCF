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
    style = "display: flex; justify-content: center; align-items: center; gap: 20px; margin-top: 20px; margin-bottom: 10px;",
    tags$img(src = "logo3.png", height = "100px", alt = "Logo UNAL"),
    tags$h2("Comparación de las MCF de dos muestras", style = "margin: 0; font-weight: bold;")
  ),
  tags$hr(style = "margin-bottom: 20px;"),
  
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
      
      h4("Columnas Dataset 1"),
      uiOutput("col_id1")%>%
        tagAppendAttributes(`data-step` = 7,`data-intro` = "Selecciona la variable de identificación del individuo del primer conjunto de datos"),
      uiOutput("col_time1") %>%
        tagAppendAttributes(`data-step` = 8,`data-intro` = "Selecciona la variable del tiempo del primer conjunto de datos"),
      uiOutput("col_event1")  %>%
        tagAppendAttributes(`data-step` = 9,`data-intro` = "Selecciona la variable del evento  del primer de datos"),
      
      h4("Columnas Dataset 2"),
      uiOutput("col_id2") %>%
        tagAppendAttributes(`data-step` = 10,`data-intro` = "Selecciona la variable de identificación del individuo del segundo conjunto de datos"),
      uiOutput("col_time2") %>%
        tagAppendAttributes(`data-step` = 11,`data-intro` = "Selecciona la variable del tiempo del segundo conjunto de datos"),
      uiOutput("col_event2")  %>%
        tagAppendAttributes(`data-step` = 12,`data-intro` = "Selecciona la variable del evento del segundo conjunto de datos")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Resumen", 
                 verbatimTextOutput("summary_text") %>%
                   tagAppendAttributes(`data-step` = 13,`data-intro` = "En la pestaña resumen, podras visualizar un resumen 
                   estadístico de los dos conjuntos de datos convertidos al formato RDU, incluyendo información como 
                   número de recurrencias, tiempo minimo de las recurrencias, tiempo maximo de las recurrencias,etc. Y en 
                   la pestaña Gráficas, puedes visualizar la comparación de las curvas MCF (Mean Cumulative Function) 
                   para los dos conjuntos de datos. La primera gráfica muestra ambas curvas superpuestas, y la segunda 
                   muestra la diferencia entre ellas.") 
        ),
        
        tabPanel("Gráficas", 
                 div(id = "graficas_tab",
                     plotOutput("mcf_plot", height = "600px") 
                       
                 )
        )
        
      )
    )
  ),
  # Marca de agua

  
  tags$div(
    style = "text-align: center; font-size: 13px; color: #888; margin-top: 40px; margin-bottom: 10px;",
    "Creado por Grupo de investigación en Estadística, Universidad Nacional de Colombia - Sede Medellín"
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
      selectInput("base_datos1", "Selecciona Dataset 1:", choices = bases_de_datos) %>%
        tagAppendAttributes(`data-step` = 2, `data-intro` = "Selecciona el primer conjunto de datos precargado.")
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
      selectInput("base_datos2", "Selecciona Dataset 2:", choices = bases_de_datos) %>%
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
    selectInput("col_id1", "Columna ID:", choices = NULL)
  })
  
  output$col_time1 <- renderUI({
    req(datos1())
    selectInput("col_time1", "Columna tiempo:", choices = NULL)
  })
  
  output$col_event1 <- renderUI({
    req(datos1())
    selectInput("col_event1", "Columna tipo evento:", choices = NULL)
  })
  
  output$col_id2 <- renderUI({
    req(datos2())
    selectInput("col_id2", "Columna ID:", choices = NULL)
  })
  
  output$col_time2 <- renderUI({
    req(datos2())
    selectInput("col_time2", "Columna tiempo:", choices = NULL)
  })
  
  output$col_event2 <- renderUI({
    req(datos2())
    selectInput("col_event2", "Columna tipo evento:", choices = NULL)
  })
  
  rdu1 <- reactive({
    req(datos1(), input$col_id1, input$col_time1)
    col_event <- if (input$col_event1 != "No aplica") input$col_event1 else NULL
    frame.to.rdu(datos1(), ID.column = input$col_id1,
                          time.column = input$col_time1,
                          event.column = col_event,
                          data.title = {input$base_datos1 %||% input$file1$name})
  })
  
  rdu2 <- reactive({
    req(datos2(), input$col_id2, input$col_time2)
    col_event <- if (input$col_event2 != "No aplica") input$col_event2 else NULL
    frame.to.rdu(datos2(), ID.column = input$col_id2,
                          time.column = input$col_time2,
                          event.column = col_event,
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
    cat("Resumen Dataset 1:\n")
    print(summary(rdu1()))
    cat("\nResumen Dataset 2:\n")
    print(summary(rdu2())) 
  })



observeEvent(input$start_tour, {
  introjs(session, options = list(steps = steps(), 
                                  nextLabel = "Siguiente",
                                  prevLabel = "Anterior", 
                                  doneLabel = "Finalizar"))
})
}





shinyApp(ui = ui, server = server)
