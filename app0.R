library(knitr)
library(dplyr)
library(survival)
library(ggplot2)
library(tibble)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)
library(condsurv)
library(prodlim)


ui <- fluidPage(
        titlePanel("Ánalisis Supervivencia (v0)"),
        sidebarLayout(
                sidebarPanel(
                        fileInput("file", "Sube archivo Excel (.xlsx)", accept = c(".xlsx")),
                        uiOutput("numPacUI"),
                        uiOutput("fdxUI"),
                        uiOutput("fusUI"),
                        uiOutput("statusUI"),
                        
                        # Selector de variable dicotómica opcional
                        uiOutput("groupUI"),
                        
                        # Inputs de etiquetas solo si hay variable dicotómica
                        uiOutput("groupLabelsUI")
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
        
        # SelectInput dinámicos para columnas
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
        
        # Selector de variable dicotómica
        output$groupUI <- renderUI({
                req(data())
                selectInput("group", "Variable dicotómica para separar curvas (opcional)",
                            choices = c("", names(data())))
        })
        
        # Inputs de etiquetas condicionales
        output$groupLabelsUI <- renderUI({
                req(input$group)
                if (input$group != "") {
                        tagList(
                                textInput("group0Label", "Etiqueta para 0", value = "Grupo 0"),
                                textInput("group1Label", "Etiqueta para 1", value = "Grupo 1")
                        )
                }
        })
        
        # Calcular dataframe base de supervivencia
        surv_data <- reactive({
                req(input$numPac, input$fdx, input$fus, input$status)
                data() %>%
                        mutate(
                                tiempo = as.numeric(as.Date(.data[[input$fus]]) - as.Date(.data[[input$fdx]])),
                                evento = ifelse(.data[[input$status]] == 1, 1, 0)
                        )
        })
        
        # Ajustar Kaplan-Meier
        fit <- reactive({
                df <- surv_data()  # Copia del dataframe reactivo
                req(df)
                
                # Si no hay variable dicotómica
                if(is.null(input$group) || input$group == "") {
                        survfit(Surv(tiempo, evento) ~ 1, data = df)
                        
                } else {
                        grupo_raw <- df[[input$group]]
                        req(length(grupo_raw) > 0)
                        
                        # Validar que solo contenga 0/1
                        if(!all(na.omit(grupo_raw) %in% c(0,1))) {
                                showNotification("La variable dicotómica debe contener solo 0 y 1", type = "error")
                                return(NULL)
                        }
                        
                        # Crear nuevo dataframe con columna 'grupo'
                        df2 <- df %>%
                                mutate(grupo = factor(grupo_raw, levels = c(0,1),
                                                      labels = c(input$group0Label, input$group1Label)))
                        
                        survfit(Surv(tiempo, evento) ~ grupo, data = df2)
                }
        })
        
        # Graficar Kaplan-Meier
        output$survPlot <- renderPlot({
                req(fit())
                ggsurv <- ggsurvplot(
                        fit(),
                        data = surv_data(),
                        risk.table = TRUE,
                        conf.int = TRUE,
                        xlab = "Tiempo (días)",
                        ylab = "Supervivencia",
                        ggtheme = theme_minimal(),
                        legend.title = ifelse(!is.null(input$group) && input$group != "", "Grupo", NULL),
                        legend.labs = ifelse(!is.null(input$group) && input$group != "", 
                                             c(input$group0Label, input$group1Label), NULL)
                )
                print(ggsurv)
        })
        
        # Tabla resumen
        output$survTable <- renderTable({
                req(fit())
                summary(fit())$table
        }, rownames = TRUE)
}

shinyApp(ui, server)
