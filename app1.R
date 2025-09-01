# app_shiny_survival.R
library(shiny)
library(readxl)
library(dplyr)
library(survival)
library(ggsurvfit)
library(tidyr)

ui <- fluidPage(
        titlePanel("Análisis de supervivencia"),
        sidebarLayout(
                sidebarPanel(
                        fileInput("file", "Subir archivo Excel (.xlsx)", accept = c(".xlsx")),
                        uiOutput("numPacUI"),
                        uiOutput("fdxUI"),
                        uiOutput("fusUI"),
                        uiOutput("statusUI"),
                        uiOutput("groupUI"),
                        uiOutput("groupLabelsUI"),
                        numericInput("timePoint", "Tiempo para estimar en la unidad seleccionada", value = 365, min = 0.01, step = 0.1),
                        radioButtons("timeUnit", "Unidad de tiempo",
                                     choices = c("Días" = "days", "Meses" = "months", "Años" = "years"),
                                     selected = "days")
                ),
                mainPanel(
                        plotOutput("survPlot", height = "600px"),
                        h4("Tabla resumen (survfit)"),
                        tableOutput("survTable")
                )
        )
)

server <- function(input, output, session) {
        
        # Conversión robusta de fechas (Date, POSIXt, numérico Excel, character)
        as_date_robust <- function(x) {
                if (inherits(x, "Date")) return(as.Date(x))
                if (inherits(x, "POSIXt")) return(as.Date(x))
                if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30")) # Excel typical origin
                res <- suppressWarnings(as.Date(x))
                # si sigue NA, intentar con formato común dd/mm/yyyy o yyyy-mm-dd
                if (all(is.na(res)) && is.character(x)) {
                        res_try1 <- suppressWarnings(as.Date(x, format = "%d/%m/%Y"))
                        if (!all(is.na(res_try1))) return(res_try1)
                        res_try2 <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
                        return(res_try2)
                }
                res
        }
        
        # Factor de conversión para la unidad de tiempo
        time_factor <- reactive({
                switch(input$timeUnit,
                       "days" = 1,
                       "months" = 365.25 / 12, # ~30.4375 días
                       "years" = 365.25,
                       1)
        })
        
        time_label <- reactive({
                switch(input$timeUnit,
                       "days" = "días",
                       "months" = "meses",
                       "years" = "años",
                       "tiempo")
        })
        
        # Leer el Excel
        data <- reactive({
                req(input$file)
                read_excel(input$file$datapath)
        })
        
        # SelectInputs dinámicos
        output$numPacUI <- renderUI({
                req(data())
                selectInput("numPac", "Nº paciente (id único)", choices = names(data()), selected = names(data())[1])
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
        
        output$groupUI <- renderUI({
                req(data())
                selectInput("group", "Variable dicotómica para separar curvas (opcional)",
                            choices = c("", names(data())))
        })
        
        output$groupLabelsUI <- renderUI({
                req(input$group)
                if (input$group != "") {
                        tagList(
                                textInput("group0Label", "Etiqueta para 0", value = "Grupo 0"),
                                textInput("group1Label", "Etiqueta para 1", value = "Grupo 1")
                        )
                }
        })
        
        # Dataframe preparado: fechas, tiempo (convertido), evento, y grupo
        surv_df <- reactive({
                req(input$numPac, input$fdx, input$fus, input$status)
                df_raw <- data()
                
                # Validaciones básicas
                if (!(input$fdx %in% names(df_raw)) || !(input$fus %in% names(df_raw)) ||
                    !(input$status %in% names(df_raw))) {
                        showNotification("Selecciona correctamente las columnas de fecha/estado.", type = "error")
                        return(NULL)
                }
                
                df <- df_raw %>%
                        mutate(
                                .fdx = as_date_robust(.data[[input$fdx]]),
                                .fus = as_date_robust(.data[[input$fus]])
                        ) %>%
                        mutate(
                                tiempo = as.numeric(.fus - .fdx) / as.numeric(time_factor()),
                                evento = ifelse(.data[[input$status]] == 1, 1, 0)
                        ) %>%
                        filter(!is.na(tiempo), tiempo >= 0, !is.na(evento))
                
                # Manejo variable dicotómica opcional
                if (!is.null(input$group) && input$group != "") {
                        if (!(input$group %in% names(df_raw))) {
                                showNotification("La variable dicotómica seleccionada no existe.", type = "error")
                                return(NULL)
                        }
                        grupo_raw <- df_raw[[input$group]]
                        # Validar 0/1 (admitir NA)
                        if (!all(na.omit(grupo_raw) %in% c(0, 1))) {
                                showNotification("La variable dicotómica debe contener solo 0 y 1 (o NA).", type = "error")
                                return(NULL)
                        }
                        df <- df %>%
                                mutate(grupo = factor(.data[[input$group]],
                                                      levels = c(0, 1),
                                                      labels = c(input$group0Label %||% "Grupo 0", input$group1Label %||% "Grupo 1")))
                } else {
                        df <- df %>% mutate(grupo = factor("Global"))
                }
                
                # Si tras filtros no hay filas, notificar
                if (nrow(df) == 0) {
                        showNotification("No hay datos válidos tras la transformación (fechas/estado).", type = "warning")
                        return(NULL)
                }
                
                df
        })
        
        # Ajuste Kaplan-Meier
        fit <- reactive({
                df <- surv_df(); req(df)
                survfit(Surv(tiempo, evento) ~ grupo, data = df)
        })
        
        # Gráfico KM con ggsurvfit usando la unidad seleccionada en el eje X
        output$survPlot <- renderPlot({
                req(fit())
                ggsurvfit(fit()) +
                        labs(x = paste0("Tiempo (", time_label(), ")"),
                             y = "Probabilidad de supervivencia") +
                        add_confidence_interval() +
                        add_risktable()
        })
        
        # Tabla resumen del objeto survfit
        output$survTable <- renderTable({
                req(fit())
                as.data.frame(summary(fit())$table)
        }, rownames = TRUE)
}

# helper: operador %%||%% para default simple si NULL (si no tienes rlang)
`%||%` <- function(a, b) if (!is.null(a)) a else b

shinyApp(ui, server)

