library(shiny)
library(readxl)
library(dplyr)
library(survival)
library(survminer)


ui <- fluidPage(
        titlePanel("Análisis de supervivencia básico"),
        
        sidebarLayout(
                sidebarPanel(
                        fileInput("file", "Subir archivo Excel (.xlsx)", accept = c(".xlsx")),
                        uiOutput("numPacUI"),
                        uiOutput("fdxUI"),
                        uiOutput("fusUI"),
                        uiOutput("statusUI")
                ),
                
                mainPanel(
                        plotOutput("survPlot"),
                        tableOutput("survTable")
                )
        )
)

server <- function(input, output, session) {
        
        # Leer el Excel
        data <- reactive({
                req(input$file)
                read_excel(input$file$datapath)
        })
        
        # Crear selectInput dinámicos para columnas
        output$numPacUI <- renderUI({
                req(data())
                selectInput("numPac", "Nº paciente", choices = names(data()))
        })
        output$fdxUI <- renderUI({
                req(data())
                selectInput("fdx", "Fecha diagnóstico", choices = names(data()))
        })
        output$fusUI <- renderUI({
                req(data())
                selectInput("fus", "Fecha último seguimiento", choices = names(data()))
        })
        output$statusUI <- renderUI({
                req(data())
                selectInput("status", "Estado (1=Muerto, 0=Vivo)", choices = names(data()))
        })
        
        # Calcular supervivencia
        surv_data <- reactive({
                req(input$numPac, input$fdx, input$fus, input$status)
                df <- data() %>%
                        mutate(
                                tiempo = as.numeric(as.Date(.data[[input$fus]]) - as.Date(.data[[input$fdx]])),
                                evento = as.numeric(.data[[input$status]])
                        )
                df
        })
        
        # Crear objeto Surv
        surv_obj <- reactive({
                req(surv_data())
                Surv(time = surv_data()$tiempo, event = surv_data()$evento)
        })
        
        # Ajustar Kaplan-Meier
        fit <- reactive({
                survfit(surv_obj() ~ 1)
        })
        
        # Graficar
        output$survPlot <- renderPlot({
                req(fit())
                ggsurvplot(fit(), data = surv_data(),
                           risk.table = TRUE,
                           conf.int = TRUE,
                           xlab = "Tiempo (días)",
                           ylab = "Supervivencia")
        })
        
        # Tabla resumen
        output$survTable <- renderTable({
                req(fit())
                summary(fit())$table
        }, rownames = TRUE)
}

shinyApp(ui, server)


